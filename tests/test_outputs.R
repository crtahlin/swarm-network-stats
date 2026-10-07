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
# bee's default bootnodes resolve through a fake lookup: two levels of dnsaddr records, three bootnodes on two IP addresses,
# one of them with a WSS address too (shaped like mainnet.ethswarm.org on 2026-10-07)
fake_dns <- list(
  "mainnet.ethswarm.org" = "dnsaddr=/dnsaddr/eu.mainnet.ethswarm.org",
  "eu.mainnet.ethswarm.org" = c("dnsaddr=/dnsaddr/a.mainnet.ethswarm.org", "dnsaddr=/dnsaddr/b.mainnet.ethswarm.org"),
  "a.mainnet.ethswarm.org" = c("dnsaddr=/ip4/198.18.1.1/tcp/1634/p2p/QmA", "dnsaddr=/ip4/198.18.1.1/tcp/1635/tls/sni/198-18-1-1.k.libp2p.direct/ws/p2p/QmA",
                               "dnsaddr=/ip4/198.18.1.1/tcp/1636/p2p/QmB"),
  "b.mainnet.ethswarm.org" = "dnsaddr=/ip4/198.18.1.2/tcp/1634/p2p/QmC")
doh_txt <- function(name) { r <- fake_dns[[name]]; if (is.null(r)) character(0) else r }
chain_window_days <- 0.25
# the saved storage history changes as it is extended; the tests use their own
storage_history_data <- read_storage_history(tempfile())

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

all_outputs <- c("leafletMap", "map_note", "data_status", "chain_status", "nbhoodMap", "nbhood_hover_text", "nbhood_selected_text",
                 "price_now", "price_model_change", "price_observed_change", "price_at_horizon", "price_gib_month", "price_calibration", "pricePlot",
                 "price_balance", "price_balance_note", "price_balance_text",
                 "light_verdict", "light_capable", "light_capable_note", "light_places", "light_places_note", "light_transport_note",
                 "light_input_warning", "light_most", "light_most_note", "light_load", "light_load_note", "light_list_note",
                 "light_takes", "light_start_burst", "lightPlot", "bootnode_list_note", "bootnode_concurrent", "bootnode_concurrent_note",
                 "bootnode_max_joins", "bootnode_max_joins_note", "bootnode_hosts", "bootnode_lose_host", "growth_summary", "growthPlot", "fullnessPlot", "growthPlot_hover", "fullnessPlot_hover", "pricePlot_hover",
                 "storage_taken", "max_radius", "max_capacity",
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
      session$setInputs(storageRadius = radius, minNodesPerNbhood = 2, onlyFullNodes = full, activeDays = 0.25, showUnstaked = full,
                        participation = 100, horizonDays = 90, blockSeconds = "5", extraNodes = 0, fitDays = 90, growthHorizon = 180, assumedGrowth = 0,
                        clientConnections = 200, startDials = 0, listLimit = 100, otherLimit = 100,
                        listNodes = 0, expectedClients = 4000, bootnodeSource = "default", bootnodeWssOnly = FALSE,
                        joinsPerMinute = 600, bootnodeHoldSecs = 30, bootnodesDialled = 3, bootnodeLimit = 100)
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

# an RPC that answers an empty list over its limit (issue #50) loses data on a long query; reading
# in pieces of at most logs_max_span blocks keeps every query under the limit
fake_rpc$empty_over <- 100
saved_span <- logs_max_span; logs_max_span <- 1e9
check("chain - an empty answer over the limit loses data when queries are not capped", !same_data(fetch_chain_data(), chain))
logs_max_span <- 1000; fake_rpc$requests <- 0
capped <- fetch_chain_data()
check("chain - log queries capped at logs_max_span blocks give the same data", same_data(capped, chain) && fake_rpc$requests > 10)
logs_max_span <- saved_span; fake_rpc$empty_over <- Inf

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
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE, activeDays = chain_window_days)
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
  # a shorter active period keeps only the reveals in it
  session$setInputs(activeDays = 0.05)
  output$stakes_table
  short <- captured("stakes_table")
  recent <- last_reveals(chain, 0.05)
  check("stakes table - last reveals follow the active period", sum(!is.na(short$last_round)) == nrow(recent) &&
          nrow(recent) < sum(revealed) && nrow(recent) > 0, paste(nrow(recent), sum(revealed)))
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
            all(as.character(tiles$class) == ifelse(tiles$active + tiles$idle + tiles$unstaked == 0, "No node",
                                                    ifelse(tiles$shown >= 5, "5 or more", as.character(tiles$shown)))))
  }
}

# a nbhood with no node at all has its own class; one with only idle or hidden nodes does not
fake_members <- data.frame(nbhood = c("01", "10"), kind = c(node_kinds[["idle"]], node_kinds[["unstaked"]]), in_swarmscan = TRUE)
fake_tiles <- nbhood_tiles(fake_members, 2, FALSE)
check("map - empty nbhoods are told apart from hidden or idle ones",
      identical(as.character(fake_tiles$class[match(c("00", "01", "10", "11"), fake_tiles$nbhood)]), c("No node", "0", "0", "No node")))
# a stake with height greater than the radius cannot play there and is left out; a full node without an overlay is left out
deep <- chain$stakes[1, ]; deep$overlay <- paste(rep("ab", 32), collapse = ""); deep$height <- 6
# the node without an overlay goes through prepare_nodes_data, as in the app
odd_fixture <- fixture
odd <- which(prepare_nodes_data(fixture)$fullNode %in% TRUE & !(prepare_nodes_data(fixture)$overlay %in% chain$stakes$overlay))[1]
odd_fixture$nodes$overlay[odd] <- NA
odd_dump <- prepare_nodes_data(odd_fixture)
with_deep <- nbhood_members(rbind(chain$stakes, deep), latest, odd_dump, 4)
check("map - a stake deeper than the radius and a node without an overlay are left out", !(deep$overlay %in% with_deep$overlay) &&
        !anyNA(with_deep$nbhood) && nrow(with_deep) == nrow(nbhood_members(chain$stakes, latest, dump, 4)) - 1)
check("map - at a radius equal to its height the stake is listed in every nbhood",
      sum(nbhood_members(rbind(chain$stakes, deep), latest, dump, 6)$overlay == deep$overlay) == 2^6)

# last drawn: for each nbhood, the latest anchor whose first bits name it, against the raw fixture
raw_redis <- chain_fixture$logs[[tolower(chain_contracts$redistribution$address)]]
raw_anchor <- Filter(function(l) l$topics[[1]] == chain_topics$anchor && hex_to_number(l$blockNumber) >= chain$window_start, raw_redis)
anchor_round <- vapply(raw_anchor, function(l) hex_to_number(substr(l$data, 3, 66)), 0)
anchor_bits <- overlay_to_bits(vapply(raw_anchor, function(l) substr(l$data, 67, 130), ""))
check("chain - every anchor in the window is read", nrow(chain$anchors) == length(raw_anchor) && setequal(chain$anchors$round, anchor_round))
drawn <- last_drawn(chain$anchors, chain$truths, nbhood_names(4))
expected_round <- vapply(nbhood_names(4), function(nb) { r <- anchor_round[substr(anchor_bits, 1, 4) == nb]; if (length(r)) max(r) else NA }, 0)
check("map - last drawn is each nbhood's latest anchor", identical(unname(drawn$round), unname(expected_round)))
check("map - last drawn is claimed when its round has a truth", identical(drawn$claimed, drawn$round %in% chain$truths$round))
# pot payouts: the fixture's PotWithdrawn amounts in the last 6 hours, per day
raw_pot <- chain_fixture$logs[[tolower(chain_contracts$postage_stamp$address)]]
pot_time <- vapply(raw_pot, function(l) hex_to_number(l$blockTimestamp), 0)
pot_amount <- vapply(raw_pot, function(l) hex_to_number(substr(l$data, 67, 130)), 0) / 1e16
recent <- pot_time >= as.numeric(chain$head_time) - 0.25 * 86400
check("map - pot paid per day", isTRUE(all.equal(pot_per_day(chain, 0.25), sum(pot_amount[recent]) / 0.25)) && sum(recent) > 0)
# one nbhood's history: rounds whose anchor names it, and pots paid in those rounds, from the raw fixture
pot_block <- vapply(raw_pot, function(l) hex_to_number(l$blockNumber), 0)
anchor_time <- vapply(raw_anchor, function(l) hex_to_number(l$blockTimestamp), 0)
for (nb in unique(substr(anchor_bits, 1, 4))[1:3]) {
  history <- nbhood_history(chain, nb, 0.25)
  mine <- anchor_round[substr(anchor_bits, 1, 4) == nb & anchor_time >= as.numeric(chain$head_time) - 0.25 * 86400]
  check(sprintf("map - nbhood %s history: rounds drawn and pots paid", nb), history$rounds == length(mine) &&
          isTRUE(all.equal(history$paid, sum(pot_amount[recent & (pot_block %/% 152) %in% mine]))) && history$paid > 0)
}
# the histories of all nbhoods add up to the network's payouts in rounds with an anchor
all_paid <- sum(vapply(nbhood_names(4), function(nb) nbhood_history(chain, nb, 0.25)$paid, 0))
check("map - nbhood payouts add up to the network's", isTRUE(all.equal(all_paid, sum(pot_amount[recent & (pot_block %/% 152) %in% anchor_round]))))

# expected earnings by hand: 1,000 xBZZ a day over 512 nbhoods, 10 xBZZ against 90 is a tenth of the wins
earn <- expected_earnings(1000, 9, 10, 90)
check("map - expected earnings", isTRUE(all.equal(earn$win_share, 0.1)) && isTRUE(all.equal(earn$per_30_days, 1000 * 30 / 512 * 0.1)))
members4 <- nbhood_members(chain$stakes, latest, dump, 4)
some <- members4$nbhood[members4$kind == node_kinds[["active"]]][1]
active_here <- members4[members4$nbhood == some & members4$kind == node_kinds[["active"]], ]
check("map - stake weight halves per reserve doubling", isTRUE(all.equal(nbhood_stake_weight(members4, some), sum(active_here$effective_stake / 2^active_here$height))))

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
  session$setInputs(earningsStake = 10)
  details <- output$nbhood_selected_details
  drawn_here <- last_drawn(chain_cache$data$anchors, chain_cache$data$truths, busiest$nbhood)
  expected_earn <- expected_earnings(pot_per_day(chain_cache$data, 0.25), 4, 10, nbhood_stake_weight(members, busiest$nbhood))
  check("map - details give the last draw", if (is.na(drawn_here$round)) grepl("Not drawn", details) else grepl(format_number(drawn_here$round), details, fixed = TRUE), details)
  history_here <- nbhood_history(chain_cache$data, busiest$nbhood, 0.25)
  check("map - details give this nbhood's own payouts", grepl(sprintf("drawn in %s rounds, and their winners were paid %s xBZZ",
          format_number(history_here$rounds), format_number(round(history_here$paid, 1))), details, fixed = TRUE), details)
  explainer <- output$nbhood_earnings_explainer
  check("map - the explanation is a separate paragraph and says the payouts are network-wide",
        grepl("whole network's payouts", explainer, fixed = TRUE) && !grepl("whole network", details, fixed = TRUE))
  check("map - details give the expected earnings", grepl(sprintf("about %s xBZZ per 30 days", format_number(round(expected_earn$per_30_days, 2))), details, fixed = TRUE), details)
  check("map - the explanation gives the win share", grepl(sprintf("win %.1f%% of the time", 100 * expected_earn$win_share), explainer, fixed = TRUE), explainer)
  session$setInputs(earningsStake = 5)
  check("map - a stake below the minimum shows a message", grepl("at least 10 xBZZ", output_or_error(output$nbhood_selected_details)))
  session$setInputs(earningsStake = 10)
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

### Price projection (R/price_model.R)
oracle <- chain$oracle
check("price - oracle parameters read from the contract",
      identical(oracle$change_rate, c(1049417, 1049206, 1048996, 1048786, 1048576, 1048366, 1048156, 1047946, 1047736)) &&
        oracle$price_base == 2^20 && oracle$minimum_price == 24000 && identical(oracle$paused, FALSE))
check("price - rounds per day", isTRUE(all.equal(rounds_per_day(5), 86400 / 760)) && isTRUE(all.equal(rounds_per_day(2), 86400 / 304)))
# per round: a full redundancy r multiplies the price by changeRate[r] / priceBase; nobody revealing is the largest rise
check("price - one round with every node revealing",
      isTRUE(all.equal(expected_round_log_change(c(0, 3, 4, 8, 12), 1, oracle),
                       log(c(1049417, 1048786, 1048576, 1047736, 1047736) / 2^20))))
check("price - one round with nobody revealing", isTRUE(all.equal(expected_round_log_change(5, 0, oracle), log(1049417 / 2^20))))
# hand calculation: 2 nodes at q = 0.5 give redundancy 0, 1 or 2 with probabilities 1/4, 1/2, 1/4
check("price - binomial redundancy", isTRUE(all.equal(expected_round_log_change(2, 0.5, oracle),
                                                    sum(c(0.25, 0.5, 0.25) * log(c(1049417, 1049206, 1048996) / 2^20)))))
# the daily figures in issue #43: 4 everywhere is no change, 3 about +2.3%, 0 about +9.5%, 8 about -8.7%
daily <- function(n) drift_percent(model_drift(rep(n, 512), 1, oracle, 5))
check("price - 4 nodes everywhere: no change", abs(daily(4)) < 1e-12)
check("price - 3 nodes everywhere: about +2.3% a day", abs(daily(3) - 2.3) < 0.05, sprintf("%.3f", daily(3)))
check("price - no nodes: about +9.5% a day", abs(daily(0) - 9.5) < 0.1, sprintf("%.3f", daily(0)))
check("price - 8 nodes everywhere: about -8.7% a day", abs(daily(8) + 8.7) < 0.1, sprintf("%.3f", daily(8)))
check("price - 2-second blocks give 2.5 times the daily change", isTRUE(all.equal(model_drift(rep(3, 8), 1, oracle, 2), 2.5 * model_drift(rep(3, 8), 1, oracle, 5))))
# fitting the participation finds the q that produced a drift
counts <- c(rep(2, 50), rep(3, 200), rep(4, 200), rep(6, 62))
check("price - fitted participation recovers the q behind a drift",
      abs(fit_participation(counts, model_drift(counts, 0.7, oracle, 5), oracle, 5) - 0.7) < 1e-4)
check("price - no participation fits a drift outside the model's range", is.na(fit_participation(counts, 1, oracle, 5)))
falling <- project_price(30000, as.POSIXct("2026-10-07", tz = "UTC"), log(0.9), 30, 24000)
check("price - the projection never goes below the minimum price",
      min(falling$price) == 24000 && falling$price[1] == 30000 && nrow(falling) == 31)
check("price - 1 GiB for 30 days", isTRUE(all.equal(gib_month_bzz(100000, 5), 100000 * 262144 * 518400 / 1e16)))
# observed change and round statistics against the raw fixture
# over the 6 hours: from the price in force 6 hours ago (the last update before then) to now
raw_price <- chain$prices[order(chain$prices$time), ]
in_force <- tail(raw_price$price[raw_price$time <= chain$head_time - 0.25 * 86400], 1)
check("price - observed change from the price in force at the start", length(in_force) == 1 &&
        isTRUE(all.equal(observed_drift(chain$prices, chain$price, chain$head_time, 0.25), log(chain$price / in_force) / 0.25)))
# over a longer period than the history: from the first update, over the time since then
since_first <- as.numeric(difftime(chain$head_time, raw_price$time[1], units = "days"))
check("price - observed change from the first update when the history is shorter",
      isTRUE(all.equal(observed_drift(chain$prices, chain$price, chain$head_time, 2), log(chain$price / raw_price$price[1]) / since_first)))
check("price - no history gives no observed change", is.na(observed_drift(chain$prices[0, ], chain$price, chain$head_time, 1)))
stats <- round_stats(chain, 0.25)
truth_rounds_window <- unique(truth_rounds)
complete <- truth_rounds_window[truth_rounds_window <= chain$to_block %/% 152 - 1 & truth_rounds_window >= ceiling(chain$window_start / 152)]
check("price - claimed rounds are the complete rounds with a truth", stats$claimed == length(complete))
check("price - matching reveals per claimed round",
      isTRUE(all.equal(stats$mean_matching, sum(chain$reveals$round %in% complete & chain$reveals$matched_truth %in% TRUE) / length(complete))))

# nodes to hold the price flat: hand-countable cases
plan <- balance_plan(rep(4, 10), 1, oracle)
check("balance - 4 nodes everywhere needs nothing", plan$add == 0 && plan$leave == 0 && plan$moves == 0)
check("balance - 3 nodes everywhere needs one more in each", balance_plan(rep(3, 10), 1, oracle)$add == 10)
check("balance - 5 nodes everywhere lets one leave from each", balance_plan(rep(5, 10), 1, oracle)$leave == 10)
check("balance - no participation is out of reach", is.na(balance_plan(rep(3, 4), 0, oracle)$add))
check("balance - an even spread moves the nodes above the average", balance_plan(c(5, 1, 3, 3), 1, oracle)$moves == 2)
# the step per matching reveal is almost constant, so the count is close to 4 / q per neighbourhood minus today's nodes
counts <- c(rep(1, 40), rep(2, 60), rep(3, 250), rep(4, 200), rep(6, 62))
for (q in c(1, 0.87, 0.6)) {
  plan <- balance_plan(counts, q, oracle)
  check(sprintf("balance - nodes to add at q = %.2f is about 4 / q per neighbourhood less today's", q),
        abs(plan$add - (4 / q * length(counts) - sum(counts))) <= 3, paste(plan$add, 4 / q * length(counts) - sum(counts)))
  check(sprintf("balance - at q = %.2f the even spread changes the price by under 0.05%% a day", q),
        abs(drift_percent(rounds_per_day(5) * plan$even) - drift_percent(rounds_per_day(5) * plan$now)) < 0.05)
}

# the button's slider update is recorded instead of sent to a browser
slider_updates <- list()
updateSliderInput <- function(session, inputId, ...) slider_updates[[length(slider_updates) + 1]] <<- list(id = inputId, value = list(...)$value)
testServer(server, {
  session$setInputs(storageRadius = 4, minNodesPerNbhood = 2, onlyFullNodes = FALSE, activeDays = 0.25, showUnstaked = FALSE,
                    participation = 100, horizonDays = 90, blockSeconds = "5", extraNodes = 0)
  n <- nbhood_tiles(nbhood_members(chain_cache$data$stakes, last_reveals(chain_cache$data, 0.25), swarm_cache$data$nodes, 4), 4, FALSE)$active
  observed <- observed_drift(chain_cache$data$prices, chain_cache$data$price, chain_cache$data$head_time, 0.25)
  fitted <- fit_participation(n, observed, oracle, 5)
  output$price_calibration
  check("price tab - the participation starts at the fitted value",
        length(slider_updates) == 1 && slider_updates[[1]]$id == "participation" && slider_updates[[1]]$value == round(100 * fitted),
        paste("fitted", fitted, "updates", length(slider_updates)))
  slider_updates <<- list()
  check("price tab - model change matches the model", output$price_model_change == sprintf("%+.2f%%", drift_percent(model_drift(n, 1, oracle, 5))))
  full_change <- output$price_model_change
  session$setInputs(participation = 50)
  check("price tab - lower participation raises the change", as.numeric(sub("%", "", output$price_model_change)) > as.numeric(sub("%", "", full_change)))
  session$setInputs(participation = 100, extraNodes = 3)
  check("price tab - extra nodes lower the change", as.numeric(sub("%", "", output$price_model_change)) <= as.numeric(sub("%", "", full_change)))
  check("price tab - the plot renders with the comparison line", !inherits(output_or_error(output$pricePlot), "output_error"))
  plan <- balance_plan(n, 1, oracle)
  check("price tab - nodes to hold the price flat", output$price_balance ==
          (if (plan$add > 0) paste0("+", format_number(plan$add)) else if (plan$leave > 0) paste0("-", format_number(plan$leave)) else "0"),
        output$price_balance)
  # the fit uses the real counts, so extra nodes do not change it
  session$setInputs(useFittedParticipation = 1)
  check("price tab - the button sets the fitted participation, ignoring extra nodes",
        length(slider_updates) == 1 && slider_updates[[1]]$id == "participation" && slider_updates[[1]]$value == round(100 * fitted),
        paste("fitted", fitted))
  check("price tab - the calibration counts exclude extra nodes", grepl(sprintf("against %.2f active staked", mean(n)), output$price_calibration))
  head_time <- as.numeric(chain_cache$data$head_time)
  session$setInputs(pricePlot_pointer = list(x = head_time - 3600, y = 1))
  check("price tab - the pointer reads the price on chain", grepl("price", output$pricePlot_hover) && !grepl("projection", output$pricePlot_hover),
        output$pricePlot_hover)
  session$setInputs(pricePlot_pointer = list(x = head_time + 10 * 86400, y = 1))
  check("price tab - in the future the pointer reads the projection", grepl("projection", output$pricePlot_hover), output$pricePlot_hover)
  session$setInputs(pricePlot_brush = list(xmin = head_time - 4 * 3600, xmax = head_time + 86400))
  check("price tab - the plot renders zoomed in", !inherits(output_or_error(output$pricePlot), "output_error"))
  rs <- round_stats(chain_cache$data, 0.25)
  # from the raw fixture: every TruthSelected log in it has depth 9 (its second data word)
  raw_truth <- Filter(function(l) l$topics[[1]] == chain_topics$truth, chain_fixture$logs[[tolower(chain_contracts$redistribution$address)]])
  check("price tab - the truths' depth is read from TruthSelected",
        all(vapply(raw_truth, function(l) hex_to_number(substr(l$data, 67, 130)), 0) == 9) && rs$truth_depth == 9, rs$truth_depth)
  check("price tab - the truths' depth is shown against the radius",
          grepl("truths were at depth 9, but the sidebar radius is 4", output$price_calibration, fixed = TRUE), output$price_calibration)
  session$setInputs(storageRadius = 9)
  check("price tab - no warning when the radius is the truths' depth", grepl("the radius set in the sidebar", output$price_calibration) &&
          !grepl("Warning", output$price_calibration))
  session$setInputs(storageRadius = 4)
  session$setInputs(horizonDays = 0)
  check("price tab - a horizon of 0 shows a message", grepl("Enter a horizon", output_or_error(output$price_at_horizon)))
})
# while the price oracle is paused, the projection holds the price flat
chain_cache$data$oracle$paused <- TRUE
testServer(server, {
  session$setInputs(storageRadius = 4, minNodesPerNbhood = 2, onlyFullNodes = FALSE, activeDays = 0.25, showUnstaked = FALSE,
                    participation = 50, horizonDays = 90, blockSeconds = "5", extraNodes = 0)
  check("price tab - paused: the model change is 0", output$price_model_change == "+0.00%", output$price_model_change)
  check("price tab - paused: the price at the horizon is today's", output$price_at_horizon == output$price_now)
  check("price tab - paused: the calibration says so, without a fitted participation", grepl("paused", output$price_calibration) &&
          !grepl("reproduces the observed change", output$price_calibration))
  check("price tab - paused: no nodes to hold the price flat", output$price_balance == "–" && grepl("paused", output$price_balance_note))
  session$setInputs(extraNodes = 2)
  check("price tab - paused: the line without extra nodes is flat too", all(price_series()$plain$y == price_series()$plain$y[1]))
})
chain_cache$data$oracle$paused <- FALSE
rm(updateSliderInput)

### Storage growth (R/storage_history.R)
prepared <- prepare_nodes_data(fixture)
status <- prepared$statusSnapshot
usable <- !is.na(status$reserveSizeWithinRadius) & !is.na(status$storageRadius) & status$reserveSizeWithinRadius > 0 & status$storageRadius > 0
check("storage - the estimate is the median of reserve x 4096 x 2^radius, in TiB",
      isTRUE(all.equal(estimate_stored_tib(prepared),
                       median(status$reserveSizeWithinRadius[usable] * 4096 * 2^status$storageRadius[usable]) / 2^40)))
row <- summarise_dump(prepared, "2026-09-30")
check("storage - one history row from a dump", row$measure == "within radius" && row$nodes == nrow(prepared) &&
        row$nodes_reporting == sum(usable) && row$radius_mode == as.integer(names(which.max(table(status$storageRadius[usable])))) &&
        isTRUE(all.equal(row$fullness_median, median(status$reserveSizeWithinRadius[usable]) / 2^22)))
old_dump <- data.frame(overlay = c("a", "b", "c"))
old_dump$statusSnapshot <- data.frame(reserveSize = c(2^21, 2^22, 0), storageRadius = c(10, 10, 0))
old_row <- summarise_dump(old_dump, "2024-01-20")
check("storage - an old dump falls back to the whole reserve", old_row$measure == "whole reserve" && old_row$nodes_reporting == 2 &&
        isTRUE(all.equal(old_row$stored_tib, median(c(2^21, 2^22) * 4096 * 2^10) / 2^40)))
# as bee reports them: a node with doubling d has storageRadius = committedDepth - d and a reserve over 2^d neighbourhoods
doubled <- data.frame(overlay = c("a", "b", "c"))
doubled$statusSnapshot <- data.frame(reserveSizeWithinRadius = c(0.9, 1.8, 7.2) * 2^22, storageRadius = c(9, 8, 6), committedDepth = 9)
doubled_row <- summarise_dump(doubled, "2026-10-07")
check("storage - doubled nodes: fullness against their own capacity, stored data unchanged",
      isTRUE(all.equal(doubled_row$fullness_p90, 0.9)) && isTRUE(all.equal(doubled_row$stored_tib, 0.9 * 2^22 * 4096 * 2^9 / 2^40)),
      paste(doubled_row$fullness_p90, doubled_row$stored_tib))
check("storage - a zero-byte history file reads as empty", { f <- tempfile(); file.create(f); nrow(read_storage_history(f)) == 0 })
check("storage - capacity at radius 9 is 8 TiB", capacity_tib(9) == 8)
check("storage - no history file gives an empty history", nrow(read_storage_history(tempfile())) == 0)
# a synthetic history: 5 TiB growing by 0.01 TiB a day, plus older days of the old measure that the fit ignores
days <- seq(as.Date("2026-06-01"), as.Date("2026-10-06"), by = "day")
synthetic <- data.frame(date = days, measure = "within radius", nodes = 5000, nodes_reporting = 2400, radius_mode = 9L,
                        reserve_median = 3e6, fullness_median = 0.7 + 0.002 * seq_along(days), fullness_p90 = 0.8,
                        stored_tib = 5 + 0.01 * as.numeric(days - days[1]))
older <- synthetic[1:10, ]; older$date <- older$date - 400; older$measure <- "whole reserve"; older$stored_tib <- 99
synthetic <- rbind(older, synthetic)
fit <- fit_growth(synthetic, 90)
check("storage - the straight-line fit finds the growth", isTRUE(all.equal(unname(stats::coef(fit$linear)[2]), 0.01)))
expected_cross <- days[1] + (8 - 5) / 0.01
check("storage - the straight line crosses 8 TiB on the right day", identical(crossing_date(fit, 8, "linear"), expected_cross),
      format(crossing_date(fit, 8, "linear")))
check("storage - a growing curve never reaches a lower level", is.na(crossing_date(fit, 4, "linear", "down")))
# a level the fitted line passed inside the fit window, but the measured data has not: the day after the window
check("storage - a level the fit already passed is reached the day after the fit window",
      identical(crossing_date(fit, 4, "linear", "up"), fit$to + 1))
check("storage - the exponential fit crosses later points at increasing dates",
      crossing_date(fit, 16, "exponential") > crossing_date(fit, 8, "exponential"))
check("storage - the fit ignores days of the older measure", fit$from >= days[1])
thin <- synthetic; thin$nodes_reporting[nrow(thin)] <- 5; thin$stored_tib[nrow(thin)] <- 500
check("storage - the fit ignores days with too few reporting nodes", isTRUE(all.equal(unname(stats::coef(fit_growth(thin, 90)$linear)[2]), 0.01)))
check("storage - a dump without status is marked", summarise_dump(data.frame(overlay = "a"), "2026-09-30")$measure == "no status")
check("storage - too little history gives no fit", is.null(fit_growth(synthetic[nrow(synthetic) - 1:0, ], 90)))
projection <- project_growth(fit, 30)
grown <- assumed_growth(as.Date("2026-10-07"), 7, 10, 365)
check("storage - assumed growth compounds per month", isTRUE(all.equal(tail(grown$stored_tib, 1), 7 * 1.1^(365 / (365.25 / 12)))) && nrow(grown) == 366)
check("storage - assumed growth reaches a level on the right day",
      identical(assumed_crossing(as.Date("2026-10-07"), 7, 10, 7 * 1.1^3), as.Date("2026-10-07") + floor(3 * 365.25 / 12)))
check("storage - shrinking never reaches a higher level", is.na(assumed_crossing(as.Date("2026-10-07"), 7, -5, 8)))
check("storage - the projection reaches the horizon for both fits", max(projection$date) == max(days) + 30 && setequal(projection$fit, c("Straight line", "Exponential")))

# zoom and pointer helpers (R/time_plots.R)
pts <- data.frame(x = as.Date("2026-01-01") + 0:9, y = c(1, 2, 3, 10, 5, 6, 7, 8, 9, 4))
lim <- zoom_limits(as.numeric(as.Date(c("2026-01-02", "2026-01-04"))), list(pts))
check("zoom - y covers only the points in view, with some room", lim$y[1] < 2 && lim$y[1] > 1.4 && lim$y[2] > 10 && lim$y[2] < 10.5)
check("zoom - no zoom is the whole plot", is.null(zoom_limits(NULL, list(pts))))
check("zoom - the coordinates take date limits", inherits(zoom_coord(lim, "date")$limits$x, "Date"))
at <- as.numeric(as.Date("2026-01-04")) + 0.3
check("pointer - nearest point", value_at(pts, at) == 10)
check("pointer - a step series holds the last value", value_at(pts, as.numeric(as.Date("2026-01-05")) - 0.1, step = TRUE) == 10)
check("pointer - outside a series gives NA", is.na(value_at(pts, as.numeric(as.Date("2026-02-01")), max_gap = 0)))

storage_history_data <- synthetic
testServer(server, {
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE, activeDays = 0.25, showUnstaked = FALSE,
                    participation = 100, horizonDays = 90, blockSeconds = "5", extraNodes = 0, fitDays = 90, growthHorizon = 180, assumedGrowth = 0,
                        clientConnections = 200, startDials = 0, listLimit = 100, otherLimit = 100,
                        listNodes = 0, expectedClients = 4000, bootnodeSource = "default", bootnodeWssOnly = FALSE,
                        joinsPerMinute = 600, bootnodeHoldSecs = 30, bootnodesDialled = 3, bootnodeLimit = 100)
  summary <- output$growth_summary
  check("growth tab - summary gives the capacity and the crossing dates", grepl("Capacity at radius 9: 8 TiB", summary, fixed = TRUE) &&
          grepl("radius rises to 10 on 20", summary, fixed = TRUE) && grepl("radius falls to 8 on no date (not reached)", summary, fixed = TRUE) &&
          grepl("Projection from a fit to", summary, fixed = TRUE), summary)
  check("growth tab - plots render", !inherits(output_or_error(output$growthPlot), "output_error") &&
          !inherits(output_or_error(output$fullnessPlot), "output_error"))
  check("growth tab - hint until the pointer is over the plot", output$growthPlot_hover == time_plot_hint)
  at <- as.numeric(as.Date("2026-08-01"))
  session$setInputs(growthPlot_pointer = list(x = at + 0.2, y = 6))
  hover <- output$growthPlot_hover
  expected_stored <- synthetic$stored_tib[synthetic$date == as.Date("2026-08-01") & synthetic$measure == "within radius"]
  check("growth tab - the pointer reads the date and the stored data", grepl("2026-08-01", hover, fixed = TRUE) &&
          grepl(sprintf("stored %s TiB at radius 9", format_number(round(expected_stored, 2))), hover, fixed = TRUE), hover)
  session$setInputs(growthPlot_pointer = list(x = as.numeric(as.Date("2026-12-01")), y = 7))
  future <- output$growthPlot_hover
  check("growth tab - in the future the pointer reads the fits", grepl("straight line", future) && grepl("exponential", future) && !grepl("stored", future), future)
  session$setInputs(fullnessPlot_pointer = list(x = at, y = 80))
  check("growth tab - the fullness pointer reads median and 90th percentile", grepl("2026-08-01 | median", output$fullnessPlot_hover, fixed = TRUE))
  # drag to zoom, double-click to zoom out: the plot renders both ways
  session$setInputs(growthPlot_brush = list(xmin = as.numeric(as.Date("2026-07-01")), xmax = as.numeric(as.Date("2026-09-01"))))
  check("growth tab - the plot renders zoomed in", !inherits(output_or_error(output$growthPlot), "output_error"))
  session$setInputs(growthPlot_dblclick = list(x = at, y = 6))
  check("growth tab - the plot renders zoomed out again", !inherits(output_or_error(output$growthPlot), "output_error"))
  session$setInputs(growthPlot_brush = list(xmin = as.numeric(as.Date("2026-07-01")), xmax = as.numeric(as.Date("2026-09-01"))), growthPlot_zoomout = 1)
  check("growth tab - the Zoom out link renders the whole plot", !inherits(output_or_error(output$growthPlot), "output_error"))
  session$setInputs(assumedGrowth = 5)
  summary_assumed <- output$growth_summary
  now_row <- tail(synthetic, 1)
  expected_date <- assumed_crossing(as.Date(current_time()), now_row$stored_tib, 5, 8)
  check("growth tab - the assumed growth gives its own crossing dates", grepl("Assumed +5% a month from 20", summary_assumed, fixed = TRUE),
        summary_assumed)
  session$setInputs(growthPlot_pointer = list(x = as.numeric(as.Date("2026-12-01")), y = 7))
  check("growth tab - the pointer reads the assumed growth", grepl("assumed +5% a month", output$growthPlot_hover, fixed = TRUE))
  check("growth tab - the plot renders with the assumed growth", !inherits(output_or_error(output$growthPlot), "output_error"))
  session$setInputs(assumedGrowth = -100)
  check("growth tab - an assumed growth of -100% keeps the rest of the summary", grepl("must be above -100%", output$growth_summary) &&
          grepl("Straight line", output$growth_summary))
  session$setInputs(storageRadius = 2)
  check("growth tab - a level already passed says so", grepl("radius rises to 3: already above", output$growth_summary, fixed = TRUE))
  session$setInputs(storageRadius = 9, growthPlot_pointer = list(x = as.numeric(as.Date("2020-01-01")), y = 1))
  check("growth tab - the pointer far from any data shows only the date", output$growthPlot_hover == "2020-01-01")
  session$setInputs(assumedGrowth = 0)
  check("growth tab - 0% leaves the assumed growth out", !grepl("Assumed", output$growth_summary) && is.null(isolate(growth_series())$Assumed))
  session$setInputs(fitDays = 3)
  check("growth tab - a fit window under 7 days shows a message", grepl("Fit over 7 days", output_or_error(output$growth_summary)))
})
storage_history_data <- read_storage_history(tempfile())

### Connectivity: browser client capacity (R/light_capacity.R)
# the sample has no secure WebSocket addresses (its underlays were reduced), so two full nodes get one
with_wss <- fixture
wss_rows <- which(with_wss$nodes$fullNode %in% TRUE)[1:2]
for (i in wss_rows) with_wss$nodes$underlays[[i]] <- rbind(with_wss$nodes$underlays[[i]],
  data.frame(address = "/ip4/198.18.0.9/tcp/1635/tls/sni/198-18-0-9.k2k4.libp2p.direct/ws/p2p/16Uiu2"))
# address kinds: WSS, plain TCP (including DNS names starting with "ws") and anything else
kinds <- fixture
kinds$nodes <- kinds$nodes[1:3, ]
kinds$nodes$fullNode <- TRUE
kinds$nodes$underlays <- list(data.frame(address = "/ip4/198.18.0.1/tcp/1635/tls/sni/x.libp2p.direct/ws/p2p/A"),
                              data.frame(address = "/dns4/ws.example.org/tcp/1634/p2p/B"),
                              data.frame(address = "/ip4/198.18.0.3/udp/1634/quic-v1/p2p/C"))
kp <- prepare_nodes_data(kinds)
check("light - address kinds: WSS, plain TCP with a ws-like DNS name, and other",
      identical(kp$secure_websocket, c(TRUE, FALSE, FALSE)) && identical(kp$plain_tcp, c(FALSE, TRUE, FALSE)) &&
        identical(kp$other_address, c(FALSE, FALSE, TRUE)))
check("light - only full nodes offering a secure WebSocket address are counted",
      browser_capable_nodes(prepare_nodes_data(with_wss)) == 2 && browser_capable_nodes(prepared) == 0)
# without a start-up list: 2,493 nodes x 100 places / 200 connections = 1,246 clients
cap <- client_capacity(2493, 100, 200, clients = 4000)
check("light - even spread", cap$on_list == 0 && cap$elsewhere == 200 && cap$most == 1246 && cap$bound_by == "network")
check("light - demand against capacity", isTRUE(all.equal(cap$load, 4000 / 1246)))
check("light - a client cannot connect to more nodes than there are", client_capacity(50, 100, 200)$elsewhere == 50 &&
        client_capacity(50, 100, 200)$most == 100)
check("light - no nodes, no room", client_capacity(0, 100, 200, clients = 10)$most == 0 && !is.finite(client_capacity(0, 100, 200, clients = 10)$load))
# with a start-up list of 300 nodes taking 150 of 200 connections: the list holds 300 x 100 / 150 = 200 clients
with_list <- client_capacity(2193, 100, 200, bootnodes = 300, bootnode_limit = 100, bootnode_peers = 150, clients = 4000)
check("light - a start-up list fills first", with_list$on_list == 150 && with_list$elsewhere == 50 && with_list$list_clients == 200 &&
        with_list$other_clients == floor(2193 * 100 / 50) && with_list$most == 200 && with_list$bound_by == "list")
# what it would take, without bootnodes: 2,493 x 100 / 4,000 = 62 peers; 4,000 x 200 / 2,493 = 321 light-node-limit; 8,000 nodes
t <- what_it_takes(2493, 100, 200, 4000)
check("light - what it takes without bootnodes", identical(t$change, c("peers", "limit", "nodes")) &&
        identical(t$value, c(62, 321, 8000)) && all(t$status == "enough"), paste(t$value, t$status, collapse = " "))
check("light - clients that already fit need no change", identical(what_it_takes(2493, 100, 200, 10)$status, "already enough") &&
        identical(what_it_takes(2493, 100, 200, 0)$status, "already enough"))
# more peers per client than nodes: 50 nodes hold 5,000 light peers; 400 clients x 200 peers need 800 nodes
check("light - nodes needed use the uncapped peers per client", what_it_takes(50, 100, 200, 400)$value[3] == 800 &&
        client_capacity(800, 100, 200)$most == 400)
# with 300 bootnodes taking 150 of 200 peers: the other 2,193 nodes (50 peers) hold 4,386, already enough; the bootnodes
# hold 200: 7 bootnode peers (not enough: the other 193 hold 1,136), 2,000 light-node-limit (enough), 6,000 bootnodes (not possible)
tl <- what_it_takes(2493, 100, 200, 4000, bootnodes = 300, bootnode_limit = 100, bootnode_peers = 150)
check("light - with bootnodes: the other nodes are already enough", tl$status[tl$change == "other"] == "already enough" &&
        tl$value[tl$change == "other"] == 4386)
check("light - with bootnodes: fewer bootnode peers is not enough on its own",
      tl$value[tl$change == "bootnode_peers"] == 7 && tl$status[tl$change == "bootnode_peers"] == "not enough on its own")
check("light - with bootnodes: a higher light-node-limit on them is enough",
      tl$value[tl$change == "bootnode_limit"] == 2000 && tl$status[tl$change == "bootnode_limit"] == "enough")
check("light - with bootnodes: more bootnodes than WSS full nodes is not possible",
      tl$value[tl$change == "bootnodes"] == 6000 && tl$status[tl$change == "bootnodes"] == "not possible")
# all peers on the bootnodes: no row for the other nodes, so no "Inf"
all_on_list <- what_it_takes(2493, 100, 150, 4000, bootnodes = 300, bootnode_limit = 100, bootnode_peers = 150)
check("light - with every peer on the bootnodes there is no row for the other nodes", !("other" %in% all_on_list$change) &&
        all(is.finite(all_on_list$value)))
check("light - dial attempts use the bootnode peers each client keeps", start_attempts_per_node(4000, 300, 150) == 2000)
check("light - with every WSS full node a bootnode there is no row for the other nodes",
      !("other" %in% what_it_takes(300, 100, 200, 4000, bootnodes = 300, bootnode_limit = 100, bootnode_peers = 150)$change))
check("light - no WSS full nodes is not possible", identical(what_it_takes(0, 100, 200, 10)$status, "not possible"))
storage_history_data <- read_storage_history(tempfile())
fetch_swarmscan_data <- function() with_wss
swarm_cache$data <- NULL; swarm_cache$version <- 0; swarm_cache$next_attempt <- -Inf
testServer(server, {
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE, otherLimit = 100, listNodes = 0, startDials = 0,
                    listLimit = 100, clientConnections = NA, expectedClients = NA)
  check("light tab - network facts show without client numbers", output$light_capable == "2" && output$light_places == "200")
  check("light tab - no verdict until the client's numbers are entered", grepl("Set peers per client and concurrent clients", output$light_verdict$html) &&
          output$light_most == "–" && !inherits(output_or_error(output$lightPlot), "output_error"))
  check("light tab - the transport sentence uses the data", grepl("2 a WSS underlay", output$light_transport_note, fixed = TRUE))
  session$setInputs(clientConnections = 2, expectedClients = 400)
  # 2 nodes x 100 places / 2 connections = 100 clients
  check("light tab - clients at once", output$light_most == "100", output$light_most)
  verdict <- output$light_verdict$html
  check("light tab - the verdict states the clients and the maximum", grepl("Over capacity: 400 concurrent clients", verdict, fixed = TRUE) &&
          grepl("at most 100", verdict, fixed = TRUE), verdict)
  check("light tab - demand is red when over capacity", grepl(swarm_colours$unreachable, output$light_load$html, fixed = TRUE))
  check("light tab - no bootnode list by default", output$light_list_note == "" && output$light_start_burst == "")
  takes <- output$light_takes
  check("light tab - three changes without bootnodes", lengths(regmatches(takes, gregexpr("<tr", takes))) == 4 && grepl("Result", takes, fixed = TRUE))
  session$setInputs(listNodes = 5, startDials = 0)
  check("light tab - warnings for a list longer than the reachable nodes and a list with no connections",
        grepl("More bootnodes than WSS full nodes", output$light_input_warning) && grepl("0 bootnode peers per client", output$light_input_warning))
  session$setInputs(listNodes = 1, startDials = 3)
  check("light tab - a warning for more bootnode peers than peers", grepl("exceed peers per client", output$light_input_warning))
  session$setInputs(listNodes = 1, startDials = 2)
  check("light tab - a warning for more bootnode peers than bootnodes", grepl("exceed the bootnodes", output$light_input_warning))
  session$setInputs(clientConnections = 1, listNodes = 1, startDials = 1)
  check("light tab - all peers on bootnodes: no Inf in the note", grepl("no peers on other nodes", output$light_list_note) &&
          !grepl("Inf", output$light_list_note) && !grepl("Inf", output$light_takes))
  session$setInputs(clientConnections = 2, startDials = 1)
  check("light tab - bootnodes add their rows and the dial-attempt line",
        grepl("dial attempts", output$light_start_burst) && grepl("bootnode", output$light_takes))
  session$setInputs(listNodes = 0, startDials = 0, otherLimit = 100, expectedClients = 10)
  check("light tab - few clients fit", grepl("Within capacity", output$light_verdict$html))
})
fetch_swarmscan_data <- function() fixture

### bootnode load (R/bootnodes.R)
resolved <- resolve_dnsaddr(bee_default_bootnode, doh_txt)
check("bootnodes - dnsaddr resolves through two levels", length(resolved) == 4 && all(startsWith(resolved, "/ip4/")))
check("bootnodes - a loop of dnsaddr records stops", length(resolve_dnsaddr("/dnsaddr/loop", function(n) "dnsaddr=/dnsaddr/loop")) == 0)
check("bootnodes - pasted multiaddresses", identical(parse_multiaddrs("/ip4/1.2.3.4/tcp/1/p2p/A, junk\n/dns4/x.y/tcp/2/p2p/B "),
                                                      c("/ip4/1.2.3.4/tcp/1/p2p/A", "/dns4/x.y/tcp/2/p2p/B")))
bt <- bootnode_table(resolved)
check("bootnodes - peers, hosts and WSS", length(unique(bt$peer)) == 3 && length(unique(bt$host)) == 2 && sum(bt$wss) == 1)
# by hand: 600 joins a minute = 10 a second, kept 30 s, 3 bootnodes dialled of 3: 300 concurrent per bootnode, 3x a limit of 100;
# 100 x 3 x 60 / (30 x 3) = 200 joins a minute
bl <- bootnode_load(3, 600, 30, 3, 100)
check("bootnodes - Little's law", bl$concurrent == 300 && bl$load == 3 && bl$max_joins_per_min == 200)
check("bootnodes - a client cannot dial more bootnodes than there are", bootnode_load(2, 600, 30, 5, 100)$per_client == 2)
# a bootnode with addresses on two hosts counts once, on the first; the totals stay those of the model
two_hosts <- bootnode_table(c(resolved, "/ip6/2001:db8::1/tcp/1634/p2p/QmC"))
bh2 <- bootnode_hosts(two_hosts, 600, 30, 3, 100)
check("bootnodes - a bootnode on two hosts is counted once", bh2$bootnodes == 3 && sum(bh2$hosts$bootnodes) == 3 &&
        sum(bh2$hosts$concurrent) == 3 * bh2$load$concurrent)
# the cache keeps the last good list when part of the lookup fails, and tries again sooner
bootnode_cache$next_attempt <- -Inf
check("bootnodes - the cache resolves the default list", length(refresh_bootnode_cache(doh_txt)) == 4 && is.null(bootnode_cache$last_error))
partial <- function(name) if (name == "b.mainnet.ethswarm.org") character(0) else doh_txt(name)
bootnode_cache$next_attempt <- -Inf
check("bootnodes - a partial answer keeps the last good list", length(refresh_bootnode_cache(partial)) == 4 &&
        grepl("b.mainnet.ethswarm.org", bootnode_cache$last_error) &&
        as.numeric(bootnode_cache$next_attempt) - as.numeric(current_time()) <= retry_with_data_secs)
bootnode_cache$next_attempt <- -Inf; refresh_bootnode_cache(doh_txt)
bh <- bootnode_hosts(bt, 600, 30, 3, 100)
check("bootnodes - per IP address and losing the busiest", bh$bootnodes == 3 && bh$busiest_host == "198.18.1.1" &&
        identical(bh$hosts$bootnodes, c(2L, 1L)) && bh$without_busiest$per_client == 1 && bh$without_busiest$concurrent == 300)
testServer(server, {
  session$setInputs(storageRadius = 9, minNodesPerNbhood = 2, onlyFullNodes = FALSE, bootnodeSource = "default", bootnodeWssOnly = FALSE,
                    joinsPerMinute = NA, bootnodeHoldSecs = NA, bootnodesDialled = NA, bootnodeLimit = 100)
  check("bootnode tab - the default list is resolved and described", grepl("3 bootnodes (peer IDs) on 2 IP addresses", output$bootnode_list_note, fixed = TRUE),
        output$bootnode_list_note)
  check("bootnode tab - no load until the client's numbers are set", grepl("–", output$bootnode_concurrent$html) && output$bootnode_max_joins == "–")
  session$setInputs(joinsPerMinute = 600, bootnodeHoldSecs = 30, bootnodesDialled = 3)
  check("bootnode tab - concurrent connections and the max join rate", grepl(">300<", output$bootnode_concurrent$html) && output$bootnode_max_joins == "200")
  check("bootnode tab - the host table and losing the busiest host", grepl("198.18.1.1", output$bootnode_hosts) &&
          grepl("If 198.18.1.1 (2 of the 3 bootnodes) is lost", output$bootnode_lose_host, fixed = TRUE))
  session$setInputs(bootnodeWssOnly = TRUE)
  check("bootnode tab - WSS only keeps the bootnodes with a WSS address", grepl("1 bootnodes (peer IDs)", output$bootnode_list_note, fixed = TRUE))
  session$setInputs(bootnodeSource = "pasted", bootnodeList = "/ip4/198.18.2.1/tcp/1634/p2p/QmX")
  check("bootnode tab - WSS only with no WSS address says so", grepl("None of these bootnodes has a WSS address", output_or_error(output$bootnode_list_note)))
  session$setInputs(bootnodeSource = "default")
  session$setInputs(bootnodeWssOnly = FALSE, bootnodeSource = "pasted", bootnodeList = "")
  check("bootnode tab - an empty pasted list asks for addresses", grepl("Paste one or more multiaddresses", output_or_error(output$bootnode_list_note)))
  session$setInputs(bootnodeList = "/ip4/198.18.2.1/tcp/1634/p2p/QmX\n/ip4/198.18.2.2/tcp/1634/p2p/QmY")
  check("bootnode tab - a pasted list", grepl("2 bootnodes (peer IDs) on 2 IP addresses", output$bootnode_list_note, fixed = TRUE))
})

cat(sprintf("%d checks passed, %d failed\n", passed, failed))
quit(status = if (failed > 0) 1 else 0)
