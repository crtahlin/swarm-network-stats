# Regression test for app.R, run from the repository root:
#
#   Rscript tests/test_outputs.R
#
# It loads app.R, feeds it the saved sample in tests/fixtures/ instead of downloading from
# swarmscan, runs every output with shiny::testServer, and checks the values against counts
# computed directly from the data. It also runs the app on copies of the sample with fields
# removed, because swarmscan's data has changed shape before. Exits with status 1 on any failure.
#
# The fixture is 492 nodes from the swarmscan dump of 2026-09-30, reduced to the fields the app
# reads. Public IP addresses are replaced by addresses from 198.18.0.0/15 (a benchmarking range
# that real hosts do not use, but which the app's is_public_ip4 counts as public), so the
# location borrowing between nodes that share an IP still works.

suppressPackageStartupMessages({ library(shiny); library(ggplot2) })

fixture <- jsonlite::fromJSON("tests/fixtures/swarmscan-sample.json", simplifyVector = TRUE)

# load app.R without starting it; DT tables and the neighbourhood plot are swapped for renderers
# that save their data, because DT's server mode sends no data in the rendered widget
capture_dir <- tempfile("captures"); dir.create(capture_dir)
src <- readLines("app.R")
src <- src[!grepl("^shinyApp\\(", src)]
for (table_output in c("stats_table", "nodes_data", "reachability_status", "stakes_table", "nbhood_nodes")) {
  src <- sub(paste0("output\\$", table_output, " <- DT::renderDataTable\\("),
             paste0("output$", table_output, " <- capture_table('", table_output, "', "), src)
}
src <- sub("output\\$distPlot <- renderPlot\\(", "output$distPlot <- capture_plot(", src)
capture_table <- function(name, expr, ...) {
  e <- substitute(expr); env <- parent.frame()
  shiny::renderText({ saveRDS(eval(e, env), file.path(capture_dir, paste0(name, ".rds"))); "saved" })
}
capture_plot <- function(expr, ...) {
  e <- substitute(expr); env <- parent.frame()
  shiny::renderText({ saveRDS(ggplot_build(eval(e, env)), file.path(capture_dir, "plot.rds")); "saved" })
}
captured <- function(name) readRDS(file.path(capture_dir, paste0(name, ".rds")))
suppressWarnings(suppressPackageStartupMessages(eval(parse(text = src))))
# the chain answers from tests/fixtures/chain-sample.json; its window is 6 hours, as recorded
source("tests/fake_rpc.R")
chain_window_days <- 0.25

passed <- 0; failed <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) { passed <<- passed + 1 } else {
    failed <<- failed + 1; cat("FAIL:", label, if (nzchar(detail)) paste0("(", detail, ")") else "", "\n")
  }
}
output_or_error <- function(expr) tryCatch(expr, error = function(e) structure(conditionMessage(e), class = "output_error"))

# copies of the sample with fields removed
remove_fields <- function(d, variant) {
  n <- d$nodes
  if (variant %in% c("no_top_error", "no_errors")) { n$error <- NULL; n$unreachable <- NULL }
  if (variant %in% c("no_status_error", "no_errors")) n$statusSnapshot$error <- NULL
  if (variant == "no_location") n$location <- NULL
  if (variant == "no_underlays") n$underlays <- NULL
  if (variant == "empty_country") {
    ph <- which(n$location$latitude %in% 0 & n$location$longitude %in% 0 & is.na(n$location$country))
    n$location$country[ph] <- ""
  }
  d$nodes <- n
  d
}
variants <- c("as_served", "no_top_error", "no_status_error", "no_errors", "no_location", "no_underlays", "empty_country")

# independent counts: nodes per nbhood from the integer value of the overlay prefix
prefix_counts <- function(bits, radius) tabulate(strtoi(substr(bits, 1, radius), base = 2) + 1, nbins = 2^radius)
has_text_value <- function(x) if (is.null(x)) FALSE else !is.na(x) & x != ""
expected_max_radius <- function(bits, minimum) {
  if (length(bits) < minimum) return(NA)
  radius <- 0
  while (min(prefix_counts(bits, radius + 1)) >= minimum) radius <- radius + 1
  radius
}

all_outputs <- c("leafletMap", "map_note", "data_status", "chain_status", "nbhoodMap", "nbhood_hover_text", "nbhood_selected_text", "storage_taken", "max_radius", "max_capacity",
                 "reachability_status", "nodes_count", "distPlot", "explainer_text_1", "stats_table", "nodes_data",
                 "stakes_table")

for (variant in variants) {
  data <- remove_fields(fixture, variant)
  fetch_swarmscan_data <- function() data
  # every session starts with an empty cache
  swarm_cache$data <- NULL; swarm_cache$version <- 0; swarm_cache$next_attempt <- -Inf
  prepared <- tryCatch(prepare_nodes_data(data), error = function(e) e)
  check(paste(variant, "- data preparation succeeds"), !inherits(prepared, "error"),
        if (inherits(prepared, "error")) conditionMessage(prepared) else "")
  if (inherits(prepared, "error")) next
  location <- prepared[["location"]]

  check(paste(variant, "- no location at 0,0"), !any(location$latitude %in% 0 & location$longitude %in% 0))
  check(paste(variant, "- no location with only one coordinate"), !any(xor(is.na(location$latitude), is.na(location$longitude))))
  if (variant == "as_served") {
    check("as_served - locations borrowed from nodes sharing a public IP", sum(prepared$location_source %in% "same IP") > 0)
  }

  for (full in c(TRUE, FALSE)) for (radius in c(4, 9)) {
    testServer(server, {
      session$setInputs(storageRadius = radius, minNodesPerNbhood = 2, onlyFullNodes = full, activeDays = 0.25, showUnstaked = full)
      label <- sprintf("%s, full=%s, radius %d", variant, full, radius)
      shown <- prepared
      if (full) shown <- shown[!is.na(shown$fullNode) & shown$fullNode, ]
      node_counts <- prefix_counts(shown$overlay_binary, radius)
      error_node <- has_text_value(shown[["error"]]) | has_text_value(shown[["statusSnapshot"]][["error"]])
      unreachable_node <- shown[["unreachable"]] %in% TRUE

      # every output renders
      results <- lapply(all_outputs, function(o) output_or_error(output[[o]]))
      broken <- all_outputs[sapply(results, inherits, "output_error")]
      check(paste(label, "- every output renders"), length(broken) == 0, paste(broken, collapse = ", "))

      # Nbhoods stats table: one row per nbhood, totals from the data
      stats <- captured("stats_table")
      check(paste(label, "- stats table has one row per nbhood"), nrow(stats) == 2^radius)
      check(paste(label, "- stats table node counts per nbhood"), identical(as.numeric(stats$Freq), as.numeric(node_counts)))
      check(paste(label, "- stats table error total"), sum(stats$Freq.error) == sum(error_node))
      check(paste(label, "- empty nbhoods have no error percent"), all(is.na(stats$Percent.error[stats$Freq == 0])) && !any(is.nan(stats$Percent.error)))

      # neighbourhood plot: every nbhood is a position, grey bars match the table, yellow visible above red
      plot <- captured("plot")
      check(paste(label, "- plot has a position for every nbhood"), length(plot$layout$panel_params[[1]]$x$get_labels()) == 2^radius)
      check(paste(label, "- plot average line counts empty nbhoods"), isTRUE(all.equal(plot$data[[2]]$yintercept[1], nrow(shown) / 2^radius)))
      grey <- numeric(2^radius); grey[plot$data[[1]]$x] <- plot$data[[1]]$count
      check(paste(label, "- grey bars equal the stats table"), identical(grey, as.numeric(stats$Freq)))
      yellow <- red <- numeric(2^radius)
      if (nrow(plot$data[[3]])) yellow[plot$data[[3]]$x] <- plot$data[[3]]$count
      if (nrow(plot$data[[4]])) red[plot$data[[4]]$x] <- plot$data[[4]]$count
      check(paste(label, "- visible yellow is reachable nodes with an error"), sum(pmax(yellow - red, 0)) == sum(error_node & !unreachable_node))
      check(paste(label, "- red is unreachable nodes"), sum(red) == sum(unreachable_node))

      # Nodes info: every shown node, errors that add up to the stats table, location source
      nodes_info <- captured("nodes_data")
      check(paste(label, "- Nodes info has every shown node"), nrow(nodes_info) == nrow(shown))
      flagged <- has_text_value(nodes_info$error) | has_text_value(nodes_info$status_error)
      check(paste(label, "- Nodes info errors add up to the stats table"), sum(flagged) == sum(stats$Freq.error))
      check(paste(label, "- Nodes info location source"), identical(as.character(nodes_info$location_source), as.character(shown$location_source)))

      # map: no marker at 0,0
      map_calls <- jsonlite::fromJSON(output$leafletMap, simplifyVector = FALSE)$x$calls
      markers <- Filter(function(call) call$method == "addCircleMarkers", map_calls)[[1]]$args
      lat <- unlist(lapply(markers[[1]], function(v) if (is.null(v)) NA else v))
      lng <- unlist(lapply(markers[[2]], function(v) if (is.null(v)) NA else v))
      check(paste(label, "- no map marker at 0,0"), !any(lat %in% 0 & lng %in% 0))

      # maximum radius and capacity against brute force
      for (minimum in c(1, 2, 4)) {
        session$setInputs(minNodesPerNbhood = minimum)
        expected <- expected_max_radius(shown$overlay_binary, minimum)
        check(sprintf("%s - maximum radius, minimum %d", label, minimum), output$max_radius == format_number(expected),
              paste("got", output$max_radius, "expected", expected))
        check(sprintf("%s - maximum capacity, minimum %d", label, minimum),
              output$max_capacity == paste(format_number(2^22 * 4096 * 2^expected / 2^40), "TiB"))
      }
      session$setInputs(minNodesPerNbhood = 0)
      check(paste(label, "- a minimum of 0 shows a message"), grepl("Enter a minimum", output_or_error(output$max_radius)))
      
      # x-axis labels: shown up to 128 nbhoods, hidden above with the count in the axis title
      x_title <- plot$plot$labels$x
      labels_hidden <- inherits(plot$plot$theme$axis.text.x, "element_blank")
      check(paste(label, "- x-axis labels shown or hidden by nbhood count"),
            if (2^radius <= 128) identical(x_title, "Neighbourhood") && !labels_hidden
            else grepl("labels hidden", x_title) && labels_hidden, x_title)
    })
  }
}

# storage radius: only whole radii from 1 to 16, because outputs build 2^radius nbhood names
fetch_swarmscan_data <- function() fixture
swarm_cache$data <- NULL; swarm_cache$version <- 0; swarm_cache$next_attempt <- -Inf
testServer(server, {
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE)
  for (radius in list(24, 9.5, 0)) {
    session$setInputs(storageRadius = radius)
    check(sprintf("radius %s - shows the 1 to 16 message", format(radius)),
          grepl("Enter a storage radius from 1 to 16", output_or_error(output$stats_table)))
  }
  check("radius - nothing above 16 was cached", all(as.integer(ls(nbhood_name_cache)) <= 16))
})

# default radius: set to the radius most nodes report when data arrives, within 1-16, unless the
# user already changed it
radius_updates <- list()
updateNumericInput <- function(session, inputId, ...) radius_updates[[length(radius_updates) + 1]] <<- list(...)$value
with_typical_radius <- function(r) { d <- fixture; reported <- !is.na(d$nodes$statusSnapshot$storageRadius); d$nodes$statusSnapshot$storageRadius[reported] <- r; d }
for (case in list(list(typical = 8, user = NULL, expected = 8), list(typical = 17, user = NULL, expected = 16),
                  list(typical = 8, user = 6, expected = NULL))) {
  radius_updates <- list()
  clock <- as.POSIXct("2026-10-05 12:00:00", tz = "UTC"); current_time <- function() clock
  download_ok <- is.null(case$user)
  fetch_swarmscan_data <- function() if (download_ok) with_typical_radius(case$typical) else stop("down")
  swarm_cache$data <- NULL; swarm_cache$version <- 0; swarm_cache$next_attempt <- -Inf
  testServer(server, {
    session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE)
    if (!is.null(case$user)) {   # the user changes the radius while the first download is failing
      session$setInputs(storageRadius = case$user); download_ok <<- TRUE; clock <<- clock + 61; session$elapse(60 * 1000)
    }
    output$data_status
  })
  got <- unlist(radius_updates)
  check(sprintf("default radius - typical %d%s", case$typical, if (is.null(case$user)) "" else ", user changed it first"),
        identical(as.numeric(got), as.numeric(case$expected)), paste("updates:", paste(got, collapse = ",")))
}
rm(updateNumericInput)

# refresh: a failed first download, recovery, and a failed refresh that keeps the last good data
clock <- as.POSIXct("2026-10-04 12:00:00", tz = "UTC"); current_time <- function() clock
mode <- "fail"
fetch_swarmscan_data <- function() switch(mode, fail = stop("network down"), ok = fixture)
swarm_cache$data <- NULL; swarm_cache$version <- 0; swarm_cache$next_attempt <- -Inf
testServer(server, {
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE)
  check("refresh - no data yet shows a message", grepl("No data from swarmscan yet \\(network down\\)", output_or_error(output$nodes_count)))
  mode <<- "ok"; clock <<- clock + 61; session$elapse(60 * 1000)
  check("refresh - data shown after recovery", grepl(paste("Total nodes:", format_number(fixture$count)), output$nodes_count))
  mode <<- "fail"; clock <<- clock + 10 * 60 + 1; session$elapse(60 * 1000)
  check("refresh - failed refresh keeps the last good data", grepl(paste("Total nodes:", format_number(fixture$count)), output$nodes_count))
  check("refresh - status reports the failed refresh", grepl("last refresh failed", output$data_status))
})

### chain data (R/chain.R) against the fake chain in tests/fake_rpc.R
reset_chain_cache <- function() {
  chain_cache$data <- NULL; chain_cache$version <- 0; chain_cache$next_attempt <- -Inf; chain_cache$last_error <- NULL
}
current_time <- function() Sys.time()

# hex decoding, including values above 32 bits
check("chain - hex to number", identical(hex_to_number(c("0x0", "ff", "0x20000000000000", "0x0de0b6b3a7640000")),
                                         c(0, 255, 2^53, 1e18)))

# decoders against the same fixture values decoded with foundry's cast
chain <- fetch_chain_data()
reveal <- chain$reveals[chain$reveals$overlay == "e15d72ea416c695e3afb7d7c18a2a72208d0c59af3a81a74433382737cdb9bfc" &
                          chain$reveals$round == 319931, ]
check("chain - Revealed decoded as cast decodes it", nrow(reveal) == 1 && reveal$stake_bzz == 120 &&
        reveal$stake_density == 3.072e20 && reveal$depth == 9 &&
        reveal$reserve_commitment == "005a6636207fbb921e95326429c992a2be3813346d4d33ec23e1a5a6f0b3078d")
stake <- chain$stakes[chain$stakes$owner == "0x013f327c6ae396b2e23d0a9cc35a7360aa4132ce", ]
check("chain - stakes() decoded as cast decodes it", nrow(stake) == 1 && stake$stake_bzz == 300 &&
        stake$height == 1 && stake$last_updated_block == 46011240 && stake$minimum_stake_bzz == 20 &&
        stake$overlay == "3909579c7d67be0b6f7d1e955ea17d17e25bf658bb24f5ed7e6f5fe8f6096b0f")
# committed stake 23,758,612,497,030 x 2^1 x price is worth more than the 300 BZZ deposit, so the deposit caps it
check("chain - effective stake is capped at the deposit", isTRUE(stake$effective_stake_bzz == 300) && !stake$frozen && stake$can_play)
check("chain - current price", chain$price == strtoi(chain_fixture$current_price, 16L))

# window, counts and matching against the raw fixture
raw_logs <- chain_fixture$logs[[tolower(chain_contracts$redistribution$address)]]
raw_block <- vapply(raw_logs, function(l) hex_to_number(l$blockNumber), 0)
raw_topic <- vapply(raw_logs, function(l) l$topics[[1]], "")
in_window <- raw_block >= chain$window_start & raw_block <= chain$to_block
check("chain - every reveal in the window and none before it",
      nrow(chain$reveals) == sum(in_window & raw_topic == chain_topics$revealed) && min(chain$reveals$block) >= chain$window_start)
check("chain - the window is 6 hours", chain$to_block - chain$window_start == 4320)
check("chain - every reveal has a time", !anyNA(chain$reveals$time))
truth_rounds <- raw_block[in_window & raw_topic == chain_topics$truth] %/% 152
claimed <- chain$reveals$round %in% truth_rounds
check("chain - reveals in claimed rounds are matched or not; others are NA",
      !anyNA(chain$reveals$matched_truth[claimed]) && all(is.na(chain$reveals$matched_truth[!claimed])))
truth_of <- setNames(substr(vapply(raw_logs[in_window & raw_topic == chain_topics$truth], `[[`, "", "data"), 3, 66),
                     truth_rounds)
check("chain - a matched reveal has its round's truth hash",
      all(chain$reveals$reserve_commitment[chain$reveals$matched_truth %in% TRUE] ==
            truth_of[as.character(chain$reveals$round[chain$reveals$matched_truth %in% TRUE])]))
check("chain - one stake row per owner with a stake",
      nrow(chain$stakes) == length(chain_fixture$stakes) && !anyDuplicated(chain$stakes$overlay))
check("chain - every revealing overlay has a stake", all(chain$reveals$overlay %in% chain$stakes$overlay))
latest <- last_reveals(chain, 0.25)
check("chain - last_reveals gives one row per overlay, its latest round",
      !anyDuplicated(latest$overlay) && nrow(latest) == length(unique(chain$reveals$overlay)) &&
        all(latest$round == tapply(chain$reveals$round, chain$reveals$overlay, max)[latest$overlay]))

# a query the RPC refuses for too many results is split until it succeeds, with the same result
same_data <- function(a, b) {
  sorted <- function(d) { d <- d[do.call(order, unname(as.list(d))), ]; rownames(d) <- NULL; d }
  isTRUE(all.equal(sorted(a$reveals), sorted(b$reveals))) && isTRUE(all.equal(sorted(a$prices), sorted(b$prices))) &&
    isTRUE(all.equal(sorted(a$stakes), sorted(b$stakes))) && setequal(a$owners, b$owners)
}
fake_rpc$max_logs <- 100; fake_rpc$requests <- 0
split <- fetch_chain_data()
check("chain - refused log queries are split and give the same data", same_data(split, chain) && fake_rpc$requests > 10)
fake_rpc$max_logs <- Inf

# a network error or timeout on a log query stops the read at once instead of splitting the query
answering_post <- rpc_post
rpc_post <- function(body) {
  if (!is.null(names(body)) && body$method == "eth_getLogs") { log_queries <<- log_queries + 1; stop("Timeout was reached") }
  answering_post(body)
}
log_queries <- 0
timed_out <- tryCatch(fetch_chain_data(), error = function(e) e)
rpc_post <- answering_post
check("chain - a timed-out log query is not split", inherits(timed_out, "error") && log_queries == 1,
      paste(log_queries, "log queries"))

# an update reads only the new blocks and ends with the same data as one full read
fake_rpc$head <- chain_fixture$head_block - 2000
older <- fetch_chain_data()
fake_rpc$head <- chain_fixture$head_block
updated <- fetch_chain_data(older)
check("chain - an update equals a full read", same_data(updated, chain))
check("chain - owners are addresses", all(grepl("^0x[0-9a-f]{40}$", updated$owners)))

# eth_calls a batch leaves unanswered are sent again
fake_rpc$drop_calls <- 7
retried <- tryCatch(read_stakes(chain$owners, chain$price, chain$to_block), error = function(e) e)
check("chain - refused calls in a batch are retried", !inherits(retried, "error") && isTRUE(all.equal(retried, chain$stakes)))
fake_rpc$drop_calls <- 0

# the cache: no data yet, recovery, and a failed refresh that keeps the last good data
clock <- as.POSIXct("2026-10-07 12:00:00", tz = "UTC"); current_time <- function() clock
reset_chain_cache(); fake_rpc$fail <- TRUE
refresh_chain_cache()
check("chain cache - no data yet shows a message", grepl("No data from the Gnosis chain yet \\(the Gnosis RPC answered with HTTP status 503\\)", chain_status_text()))
fake_rpc$fail <- FALSE; clock <- clock + 61
refresh_chain_cache()
check("chain cache - data after recovery", !is.null(chain_cache$data) && grepl("Chain data up to Gnosis block 48,632,531", chain_status_text()))
fake_rpc$fail <- TRUE; clock <- clock + chain_refresh_secs + 1
refresh_chain_cache()
check("chain cache - a failed refresh keeps the last good data", !is.null(chain_cache$data) && grepl("The last read failed", chain_status_text()))
fake_rpc$fail <- FALSE
testServer(server, {
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE)
  check("chain - the sidebar shows the chain status", grepl("staked overlays", output$chain_status))

  # Nodes info: the stakes table has every staked overlay, its nbhood and its latest reveal
  output$stakes_table
  stakes_info <- captured("stakes_table")
  chain <- chain_cache$data
  check("stakes table - one row per staked overlay", nrow(stakes_info) == nrow(chain$stakes) &&
          setequal(stakes_info$overlay, chain$stakes$overlay))
  check("stakes table - nbhood is the overlay prefix at the radius",
        all(stakes_info$nbhood == substr(overlay_to_bits(stakes_info$overlay), 1, 9)))
  latest_round <- tapply(chain$reveals$round, chain$reveals$overlay, max)
  revealed <- !is.na(stakes_info$last_round)
  check("stakes table - last round is the overlay's latest reveal",
        sum(revealed) == length(latest_round) && all(stakes_info$last_round[revealed] == latest_round[stakes_info$overlay[revealed]]))
  check("stakes table - in swarmscan matches the dump", identical(stakes_info$in_swarmscan, stakes_info$overlay %in% fixture$nodes$overlay))
  header <- as.character(stakes_table_header())
  check("stakes table - every column has a header with an explanation",
        length(stakes_table_columns) == ncol(stakes_info) && lengths(regmatches(header, gregexpr("<th title=\"[^\"]+\"", header))) == ncol(stakes_info))
  session$setInputs(storageRadius = 4)
  output$stakes_table
  check("stakes table - nbhood follows the radius", all(nchar(captured("stakes_table")$nbhood) == 4))
})

### Nbhood map (R/nbhood_map.R)
# Z-order: every nbhood has its own position, and sister nbhoods (differing only in the last bit) touch
for (radius in c(4, 5)) {
  xy <- zorder_xy(nbhood_names(radius))
  sister <- match(paste0(substr(xy$nbhood, 1, radius - 1), ifelse(substr(xy$nbhood, radius, radius) == "0", "1", "0")), xy$nbhood)
  check(sprintf("map radius %d - one position per nbhood", radius), !anyDuplicated(paste(xy$x, xy$y)) &&
          max(xy$x) + 1 == 2^ceiling(radius / 2) && max(xy$y) + 1 == 2^floor(radius / 2))
  check(sprintf("map radius %d - sister nbhoods are next to each other", radius),
        all(abs(xy$x - xy$x[sister]) + abs(xy$y - xy$y[sister]) == 1))
}

chain <- fetch_chain_data()
dump <- prepare_nodes_data(fixture)
latest <- last_reveals(chain, 0.25)
for (radius in c(4, 9)) {
  members <- nbhood_members(chain$stakes, latest, dump, radius)
  label <- sprintf("map radius %d", radius)
  staked_rows <- members[members$kind != node_kinds[["unstaked"]], ]
  # a staked node with height d is listed in 2^d nbhoods, all sharing its first radius - d bits
  times <- table(staked_rows$overlay)[chain$stakes$overlay]
  check(paste(label, "- a staked node is listed 2^height times"), all(as.vector(times) == 2^chain$stakes$height))
  bits <- overlay_to_bits(staked_rows$overlay)
  height <- chain$stakes$height[match(staked_rows$overlay, chain$stakes$overlay)]
  check(paste(label, "- each listing shares the overlay's first radius - height bits"),
        all(substr(staked_rows$nbhood, 1, radius - height) == substr(bits, 1, radius - height)) && !anyDuplicated(paste(staked_rows$overlay, staked_rows$nbhood)))
  check(paste(label, "- active means a reveal in the window"),
        identical(sort(unique(staked_rows$overlay[staked_rows$kind == node_kinds[["active"]]])), sort(intersect(latest$overlay, chain$stakes$overlay))))
  full <- dump[dump$fullNode %in% TRUE & !(dump$overlay %in% chain$stakes$overlay), ]
  unstaked_rows <- members[members$kind == node_kinds[["unstaked"]], ]
  check(paste(label, "- full nodes without stake are listed once, in their own nbhood"),
        nrow(unstaked_rows) == nrow(full) && all(unstaked_rows$nbhood == substr(full$overlay_binary, 1, radius)))
  for (show in c(FALSE, TRUE)) {
    tiles <- nbhood_tiles(members, radius, show)
    check(sprintf("%s, unstaked shown %s - one tile per nbhood, counts add up", label, show),
          nrow(tiles) == 2^radius && sum(tiles$active) == sum(members$kind == node_kinds[["active"]]) &&
            sum(tiles$idle) == sum(members$kind == node_kinds[["idle"]]) && sum(tiles$unstaked) == nrow(unstaked_rows) &&
            all(tiles$shown == tiles$active + if (show) tiles$unstaked else 0) &&
            all(as.character(tiles$class) == ifelse(tiles$shown >= 4, "4 or more", as.character(tiles$shown))))
  }
}

# hover and click: the tile under the pointer, its summary, and its nodes
testServer(server, {
  session$setInputs(storageRadius = 4, minNodesPerNbhood = 2, onlyFullNodes = FALSE, activeDays = 0.25, showUnstaked = FALSE)
  members <- nbhood_members(chain_cache$data$stakes, last_reveals(chain_cache$data, 0.25), swarm_cache$data$nodes, 4)
  tiles <- nbhood_tiles(members, 4, FALSE)
  busiest <- tiles[which.max(tiles$active + tiles$idle), ]
  session$setInputs(nbhoodHover = list(x = busiest$x + 0.3, y = -busiest$y - 0.3))
  check("map - hover names the tile under the pointer", output$nbhood_hover_text == nbhood_summary(busiest, FALSE))
  session$setInputs(nbhoodHover = list(x = 100, y = 100))
  check("map - hover outside the tiles asks to point at one", grepl("Point at a neighbourhood", output$nbhood_hover_text))
  session$setInputs(nbhoodClick = list(x = busiest$x, y = -busiest$y))
  check("map - click selects the tile", output$nbhood_selected_text == paste("Nodes in neighbourhood", busiest$nbhood))
  output$nbhood_nodes
  listed <- captured("nbhood_nodes")
  check("map - the table lists the staked nodes of that nbhood",
        nrow(listed) == busiest$active + busiest$idle && all(listed$kind != node_kinds[["unstaked"]]))
  session$setInputs(showUnstaked = TRUE)
  output$nbhood_nodes
  check("map - with non-staking nodes shown, the table adds them", nrow(captured("nbhood_nodes")) == busiest$active + busiest$idle + busiest$unstaked)
  session$setInputs(storageRadius = 5)
  check("map - changing the radius clears the selection", grepl("Click a neighbourhood", output$nbhood_selected_text))
  session$setInputs(activeDays = 0)
  check("map - an active period of 0 shows a message", grepl("Enter an active period", output_or_error(output$nbhood_hover_text)))
})

cat(sprintf("%d checks passed, %d failed\n", passed, failed))
quit(status = if (failed > 0) 1 else 0)
