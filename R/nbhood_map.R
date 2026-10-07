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
# listed in their own neighbourhood
nbhood_members <- function(stakes, latest, dump_nodes, radius) {
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
    shared <- max(radius - staked$height[i], 0)
    rest <- radius - shared
    paste0(substr(bits[i], 1, shared), if (rest > 0) nbhood_names(rest) else "")
  })
  members <- staked[rep(seq_len(nrow(staked)), lengths(covered)), ]
  members$nbhood <- unlist(covered)

  if (!is.null(dump_nodes)) {
    full <- which(dump_nodes$fullNode %in% TRUE & !(dump_nodes$overlay %in% stakes$overlay))
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
  # colour classes; 4 is the price oracle's target number of matching reveals per round
  tiles$class <- factor(pmin(tiles$shown, 4), levels = 0:4, labels = c("0", "1", "2", "3", "4 or more"))
  tiles
}

# the hover text for one neighbourhood
nbhood_summary <- function(tile, show_unstaked) {
  text <- sprintf("Neighbourhood %s: %d staked and active, %d staked and idle", tile$nbhood, tile$active, tile$idle)
  if (show_unstaked) text <- paste0(text, sprintf(", %d full nodes without stake", tile$unstaked))
  if (tile$not_in_dump > 0) text <- paste0(text, sprintf(". %d of the staked nodes are not listed by swarmscan", tile$not_in_dump))
  paste0(text, ".")
}
