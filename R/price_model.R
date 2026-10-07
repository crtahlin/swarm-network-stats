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

# rounds per day for a block time in seconds (a round is 152 blocks)
rounds_per_day <- function(block_seconds) 86400 / (round_length_blocks * block_seconds)

# expected change of log(price) in one round, for each neighbourhood size in n
expected_round_log_change <- function(n, q, oracle) {
  log_rate <- log(oracle$change_rate / oracle$price_base)
  vapply(n, function(k) {
    redundancy <- 0:k
    sum(stats::dbinom(redundancy, k, q) * log_rate[pmin(redundancy, 8) + 1])
  }, numeric(1))
}

# expected change of log(price) per day, with every neighbourhood drawn equally often
model_drift <- function(n, q, oracle, block_seconds) {
  rounds_per_day(block_seconds) * mean(expected_round_log_change(n, q, oracle))
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
  list(rounds = length(rounds), claimed = length(claimed),
       mean_matching = if (length(claimed)) nrow(matched) / length(claimed) else NA_real_)
}

# what it costs, in BZZ, to keep 1 GiB (2^18 chunks of 4 KiB) for 30 days at a price in PLUR per
# chunk per block; the bare storage rent, without the unused space of a postage batch
gib_month_bzz <- function(price, block_seconds) price * 2^18 * (30 * 86400 / block_seconds) / bzz_base_units

# as a percentage change per day, for display
drift_percent <- function(drift) 100 * (exp(drift) - 1)
