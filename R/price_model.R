### the Price projection: a model of how the storage price moves, from staked nodes per neighbourhood
# How the PriceOracle works (storage-incentives src/PriceOracle.sol and Redistribution.sol):
# each round one neighbourhood is drawn by the round anchor. Its staked nodes reveal, and the claim
# reports the number of reveals that match the chosen truth (the redundancy). The price is then
# multiplied by changeRate[min(redundancy, 8)] / priceBase. A round without a claim is charged
# changeRate[0], the largest rise, when the next claim arrives. The price never goes below the
# minimum price.
#
# The model assumes each neighbourhood is drawn equally often, and that each active staked node in
# the drawn neighbourhood reveals a matching hash with probability q (the participation), so the
# redundancy follows a binomial distribution. A redundancy of 0 means no claim

# the average block time in seconds over the window, from the block headers of the window's first
# block and the head. NA when there is no start time or the window has fewer than 1,000 blocks
measured_block_seconds <- function(chain) {
  if (is.null(chain$window_start_time)) return(NA_real_)
  blocks <- chain$to_block - chain$window_start
  if (blocks < 1000) return(NA_real_)
  as.numeric(difftime(chain$head_time, chain$window_start_time, units = "secs")) / blocks
}

# rounds per day for a block time in seconds (a round is 152 blocks)
rounds_per_day <- function(block_seconds) 86400 / (round_length_blocks * block_seconds)

# the redundancy clamp proposed in ethersphere/storage-incentives PR #322 (open, not deployed as of
# 2026-10-08): Redistribution.claim passes min(max(count, 3), 5) to the PriceOracle instead of the
# count, so a claimed round moves the price at most one step either way. Rounds nobody claims are
# charged by the PriceOracle itself, which the PR does not change: they still count as changeRate[0]
slow_clamp <- c(3, 5)

# expected change of log(price) in one round, for each neighbourhood size in n. clamp: NULL for
# today's contracts, or c(low, high) to clamp the redundancy of claimed rounds as PR #322 does
expected_round_log_change <- function(n, q, oracle, clamp = NULL) {
  log_rate <- log(oracle$change_rate / oracle$price_base)
  vapply(n, function(k) {
    redundancy <- 0:k
    reported <- if (is.null(clamp)) redundancy else ifelse(redundancy == 0, 0, pmin(pmax(redundancy, clamp[1]), clamp[2]))
    sum(stats::dbinom(redundancy, k, q) * log_rate[pmin(reported, 8) + 1])
  }, numeric(1))
}

# expected change of log(price) per day, with every neighbourhood drawn equally often
model_drift <- function(n, q, oracle, block_seconds, clamp = NULL) {
  rounds_per_day(block_seconds) * mean(expected_round_log_change(n, q, oracle, clamp))
}

# the participation q (0 to 1) at which the model gives the observed drift; NA when no q does,
# because the observed change is outside what the model can give for these neighbourhoods
fit_participation <- function(n, observed_drift, oracle, block_seconds) {
  gap <- function(q) model_drift(n, q, oracle, block_seconds) - observed_drift
  if (is.na(observed_drift) || gap(0) * gap(1) > 0) return(NA_real_)
  stats::uniroot(gap, c(0, 1), tol = 1e-6)$root
}

# the price over the coming days at a constant drift, never below the minimum price
project_price <- function(start_price, start_time, drift, days, minimum_price) {
  t <- seq(0, days, length.out = max(2, ceiling(days) + 1))
  data.frame(time = start_time + t * 86400, price = pmax(start_price * exp(drift * t), minimum_price))
}

# observed change of log(price) per day over the last `days` days, from the PriceUpdate history:
# from the price in force at the start of that period to the current price. NA without history
observed_drift <- function(prices, current_price, head_time, days) {
  start <- head_time - days * 86400
  before <- prices[prices$time <= start, ]
  after <- prices[prices$time > start, ]
  if (nrow(before) > 0) {
    start_price <- before$price[which.max(before$time)]; span <- days
  } else if (nrow(after) > 0) {
    # the history starts inside the period: measure from its first update
    first <- which.min(after$time)
    start_price <- after$price[first]; span <- as.numeric(difftime(head_time, after$time[first], units = "days"))
  } else {
    return(NA_real_)
  }
  if (span <= 0) return(NA_real_)
  log(current_price / start_price) / span
}

# rounds in the last `days` days: how many were claimed, and the mean number of matching reveals
# in a claimed round. Only complete rounds count
round_stats <- function(chain, days, block_seconds = 5) {
  last_round <- chain$to_block %/% round_length_blocks - 1
  first_round <- max(ceiling((chain$to_block - days * 86400 / block_seconds) / round_length_blocks),
                     ceiling(chain$window_start / round_length_blocks))
  rounds <- if (last_round >= first_round) first_round:last_round else numeric(0)
  claimed <- intersect(rounds, chain$truths$round)
  matched <- chain$reveals[chain$reveals$round %in% claimed & chain$reveals$matched_truth %in% TRUE, ]
  # the depth of the claimed truths, the most common one. Nodes reveal their committed depth,
  # which is the same with and without reserve doubling (bee: committed depth = storage radius +
  # height; in the recorded fixture, height-0 and height-1 nodes matched the same truths at depth
  # 9), so this is the depth at which the neighbourhoods were drawn
  depths <- chain$truths$truth_depth[chain$truths$round %in% claimed]
  list(rounds = length(rounds), claimed = length(claimed),
       mean_matching = if (length(claimed)) nrow(matched) / length(claimed) else NA_real_,
       truth_depth = if (length(depths)) as.numeric(names(which.max(table(depths)))) else NA_real_)
}

# what it costs, in BZZ, to keep 1 GiB (2^18 chunks of 4 KiB) for 30 days at a price in PLUR per
# chunk per block; the bare storage rent, without the unused space of a postage batch
gib_month_bzz <- function(price, block_seconds) price * 2^18 * (30 * 86400 / block_seconds) / bzz_base_units

# as a percentage change per day, for display
drift_percent <- function(drift) 100 * (exp(drift) - 1)

# how many active staked nodes would have to join (or could leave) for the modelled price to stop
# rising (or falling), at participation q, and what spreading today's nodes evenly would do.
# Nodes join where they lower the expected change most and leave where they raise it least; the
# neighbourhoods are handled in groups of equal size, which is exact because the expected change
# depends only on a neighbourhood's node count.
# Returns: change per round now and after an even spread (mean log change), nodes to add (NA if no
# number of nodes is enough), nodes that could leave, and the moves an even spread takes
balance_plan <- function(n, q, oracle) {
  levels <- max(n) + ceiling(8 / max(q, 0.01)) + 2
  f <- expected_round_log_change(0:levels, q, oracle)          # f[k + 1]: change for k nodes
  gain <- c(f[-length(f)] - f[-1], -Inf)                       # adding one node at k nodes
  loss <- c(Inf, f[-length(f)] - f[-1])                        # removing one node at k nodes
  now <- sum(f[n + 1])

  # a sum that should come out at exactly 0 can be left at about 1e-20 by rounding; that counts as 0
  tolerance <- 1e-12
  add <- 0; count <- tabulate(n + 1, nbins = levels + 1); total <- now
  while (total > tolerance) {
    g <- ifelse(count > 0, gain, -Inf); k <- which.max(g)
    if (!is.finite(g[k]) || g[k] <= 1e-12) { add <- NA; break }
    take <- min(count[k], ceiling(total / g[k] - 1e-9))
    count[k] <- count[k] - take; count[k + 1] <- count[k + 1] + take
    total <- total - take * g[k]; add <- add + take
  }
  leave <- 0; count <- tabulate(n + 1, nbins = levels + 1); total <- now
  while (total < -tolerance) {
    l <- ifelse(count > 0, loss, Inf); k <- which.min(l)
    take <- if (is.finite(l[k])) min(count[k], floor(-total / l[k] + tolerance)) else 0
    if (take == 0) break
    count[k] <- count[k] - take; count[k - 1] <- count[k - 1] + take
    total <- total + take * l[k]; leave <- leave + take
  }
  # an even spread: every neighbourhood gets the average, rounded down or up; the fullest
  # neighbourhoods keep the extra ones, so the moves are the nodes above each target
  low <- sum(n) %/% length(n)
  target <- rep(low, length(n)); target[seq_len(sum(n) - low * length(n))] <- low + 1
  moves <- sum(pmax(sort(n, decreasing = TRUE) - target, 0))
  list(now = now / length(n), even = sum(f[target + 1]) / length(n), add = add, leave = leave, moves = moves)
}
