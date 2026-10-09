### the Nbhood map: one tile per neighbourhood, with the staked and full nodes in it
# Staked nodes come from the chain (R/chain.R), full nodes without stake from the swarmscan dump.
# Light and ultra-light nodes are not shown: swarmscan does not list them, and their overlays
# cannot be derived from chain data (see issue #41)

# kinds of node, in the order the map and its table use them
node_kinds <- c(active = "Staked, active", idle = "Staked, idle", unstaked = "Full, not staked")

# position of each neighbourhood on the map: the overlay bits alternate between column and row
# (Z-order), so the two sister neighbourhoods that one split creates are next to each other.
# Returns x (column) and y (row), both starting at 0; row 0 is drawn at the top
zorder_xy <- function(nbhoods) {
  radius <- nchar(nbhoods[1])
  x <- y <- numeric(length(nbhoods))
  for (k in seq_len(radius)) {
    bit <- as.numeric(substr(nbhoods, k, k))
    if (k %% 2 == 1) x <- 2 * x + bit else y <- 2 * y + bit
  }
  data.frame(nbhood = nbhoods, x = x, y = y)
}

# one row per node and neighbourhood it counts in, at the given radius.
# stakes: chain_stakes(); latest: last_reveals() for the active window; dump_nodes: the prepared
# swarmscan nodes table (overlay_binary added), or NULL. A staked node with height d stores the
# reserve of 2^d neighbourhoods (the contract lets it play where its overlay is within proximity
# radius - d of the anchor), so it is listed once in each of them. Full nodes without stake are
# listed in their own neighbourhood. A staked node whose height is greater than the radius is left
# out: the contract computes depth - height as unsigned 8-bit arithmetic, so its reveal reverts and
# it cannot play at that depth. Full nodes without an overlay are left out too
nbhood_members <- function(stakes, latest, dump_nodes, radius) {
  all_staked <- stakes$overlay
  stakes <- stakes[stakes$height <= radius, ]
  reveal <- latest[match(stakes$overlay, latest$overlay), ]
  dump_overlays <- if (is.null(dump_nodes)) character(0) else dump_nodes$overlay
  in_dump <- match(stakes$overlay, dump_overlays)
  field <- function(path, rows) {
    value <- dump_nodes
    for (name in path) value <- if (is.null(value)) NULL else value[[name]]
    if (is.null(value)) rep(NA, length(rows)) else value[rows]
  }
  staked <- data.frame(
    overlay = stakes$overlay,
    kind = unname(ifelse(!is.na(reveal$round), node_kinds["active"], node_kinds["idle"])),
    stake = stakes$stake_bzz, effective_stake = stakes$effective_stake_bzz, height = stakes$height,
    last_reveal = reveal$time, last_round = reveal$round, matched_truth = reveal$matched_truth,
    in_swarmscan = !is.na(in_dump),
    reachable = field(c("statusSnapshot", "isReachable"), in_dump),
    country = field(c("location", "country"), in_dump),
    user_agent = field("userAgent", in_dump)
  )
  # neighbourhoods each staked node covers: its first (radius - height) bits, followed by every
  # combination of the remaining height bits
  bits <- overlay_to_bits(staked$overlay)
  covered <- lapply(seq_len(nrow(staked)), function(i) {
    shared <- radius - staked$height[i]
    rest <- radius - shared
    paste0(substr(bits[i], 1, shared), if (rest > 0) nbhood_names(rest) else "")
  })
  members <- staked[rep(seq_len(nrow(staked)), lengths(covered)), ]
  members$nbhood <- unlist(covered)

  if (!is.null(dump_nodes)) {
    full <- which(dump_nodes$fullNode %in% TRUE & !(dump_nodes$overlay %in% all_staked) & !is.na(dump_nodes$overlay))
    unstaked <- data.frame(
      overlay = dump_nodes$overlay[full], kind = unname(node_kinds["unstaked"]),
      stake = NA_real_, effective_stake = NA_real_, height = NA_real_,
      last_reveal = as.POSIXct(NA, tz = "UTC"), last_round = NA_real_, matched_truth = NA,
      in_swarmscan = TRUE,
      reachable = field(c("statusSnapshot", "isReachable"), full),
      country = field(c("location", "country"), full),
      user_agent = field("userAgent", full),
      nbhood = substr(dump_nodes$overlay_binary[full], 1, radius)
    )
    members <- rbind(members, unstaked)
  }
  rownames(members) <- NULL
  members
}

# one row per neighbourhood: counts of each kind, the count the map colours by, and its position.
# show_unstaked adds full nodes without stake to that count
nbhood_tiles <- function(members, radius, show_unstaked) {
  nbhoods <- nbhood_names(radius)
  count <- function(kind) tabulate(match(members$nbhood[members$kind == kind], nbhoods), nbins = length(nbhoods))
  tiles <- zorder_xy(nbhoods)
  tiles$active <- count(node_kinds[["active"]])
  tiles$idle <- count(node_kinds[["idle"]])
  tiles$unstaked <- count(node_kinds[["unstaked"]])
  tiles$not_in_dump <- tabulate(match(members$nbhood[!members$in_swarmscan], nbhoods), nbins = length(nbhoods))
  tiles$shown <- tiles$active + if (show_unstaked) tiles$unstaked else 0
  # colour classes; 4 is the price oracle's target number of matching reveals per round. Fewer
  # raise the price, more lower it, so both sides of 4 are off target. A neighbourhood with no
  # node of any kind has its own class, apart from one whose nodes are all hidden or idle
  tiles$class <- factor(ifelse(tiles$active + tiles$idle + tiles$unstaked == 0, "No node", as.character(pmin(tiles$shown, 5))),
                        levels = c("No node", 0:5), labels = c("No node", "0", "1", "2", "3", "4", "5 or more"))
  tiles
}

# the hover text for one neighbourhood
nbhood_summary <- function(tile, show_unstaked) {
  text <- sprintf("Neighbourhood %s: %d staked and active, %d staked and idle", tile$nbhood, tile$active, tile$idle)
  if (show_unstaked) text <- paste0(text, sprintf(", %d full nodes without stake", tile$unstaked))
  if (tile$not_in_dump > 0) text <- paste0(text, sprintf(". %d of the staked nodes are not listed by swarmscan", tile$not_in_dump))
  paste0(text, ".")
}

# the last round in which each neighbourhood was drawn: the round anchor fell in it. Rounds in
# which nobody revealed leave no anchor on chain, so they are not counted. NA: not drawn in the window
last_drawn <- function(anchors, truths, nbhoods) {
  radius <- nchar(nbhoods[1])
  anchors <- anchors[order(anchors$round, decreasing = TRUE), ]
  latest <- match(nbhoods, substr(overlay_to_bits(anchors$anchor), 1, radius))
  data.frame(nbhood = nbhoods, round = anchors$round[latest], time = anchors$time[latest],
             claimed = anchors$round[latest] %in% truths$round)
}

# what one neighbourhood actually got over the last `days` days: the rounds in which it was drawn
# (an anchor fell in it) and the pots paid in those rounds. A pot is paid by the claim, in the second
# half of the round it closes, so its block gives the round
nbhood_history <- function(chain, nbhood, days) {
  start <- chain$head_time - days * 86400
  anchors <- chain$anchors[chain$anchors$time >= start, ]
  rounds <- anchors$round[substr(overlay_to_bits(anchors$anchor), 1, nchar(nbhood)) == nbhood]
  pots <- chain$pots[chain$pots$time >= start, ]
  list(rounds = length(rounds), paid = sum(pots$amount_bzz[(pots$block %/% round_length_blocks) %in% rounds]))
}

# the pot paid to winners per day across the whole network, in xBZZ, over the last `days` days
pot_per_day <- function(chain, days) {
  sum(chain$pots$amount_bzz[chain$pots$time >= chain$head_time - days * 86400]) / days
}

# what a new node staking `stake` xBZZ (no reserve doubling) in one neighbourhood can expect to
# earn per 30 days. Each of the 2^radius neighbourhoods is drawn equally often, and the winner of
# a drawn neighbourhood is picked among the matching reveals in proportion to stake density, which
# for nodes at the same depth is effective stake / 2^height (Redistribution.reveal and
# winnerSelection). others: the summed stake / 2^height of the active staked nodes already there,
# all assumed to reveal a matching hash
expected_earnings <- function(pot_per_day, radius, stake, others) {
  win_share <- stake / (stake + others)
  list(win_share = win_share, per_30_days = pot_per_day * 30 / 2^radius * win_share)
}

# the summed stake density weight (effective stake / 2^height) of the active staked nodes in a
# neighbourhood, from nbhood_members()
nbhood_stake_weight <- function(members, nbhood) {
  active <- members[members$nbhood %in% nbhood & members$kind == node_kinds[["active"]], ]
  sum(active$effective_stake / 2^active$height)
}

# the earnings estimate for a tile of the map, made where the game is played: in the neighbourhoods
# at the depth of the claimed truths (`depth`), whatever the sidebar radius. A tile at a radius at or
# above that depth lies inside one game neighbourhood; a tile at a lower radius covers several, and
# the estimate is their average (a new node lands in one of them, set by its overlay).
# game_members: nbhood_members() at `depth`; stakes: chain_stakes(), to count frozen nodes
tile_earnings <- function(pot_per_day, tile, depth, stake, game_members, stakes) {
  r <- nchar(tile)
  game <- if (r >= depth) substr(tile, 1, depth) else paste0(tile, nbhood_names(depth - r))
  per_game <- lapply(game, function(nb) {
    weight <- nbhood_stake_weight(game_members, nb)
    active <- game_members[game_members$nbhood == nb & game_members$kind == node_kinds[["active"]], ]
    c(expected_earnings(pot_per_day, depth, stake, weight), others = weight, active = nrow(active),
      frozen = sum(stakes$frozen[match(active$overlay, stakes$overlay)] %in% TRUE))
  })
  pick <- function(name) vapply(per_game, function(x) x[[name]], 0)
  list(game = game, depth = depth, per_30_days = mean(pick("per_30_days")), range = range(pick("per_30_days")),
       win_share = mean(pick("win_share")), others = mean(pick("others")), active = sum(pick("active")), frozen = sum(pick("frozen")))
}

# what the whole network paid per `stake` xBZZ of effective stake per 30 days: the pot shared over the
# effective stake of all active staked nodes, a comparison for one neighbourhood's estimate
network_earnings <- function(pot_per_day, stake, active_effective_stake) {
  if (active_effective_stake <= 0) return(NA_real_)
  pot_per_day * 30 * stake / active_effective_stake
}
