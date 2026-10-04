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
for (table_output in c("stats_table", "nodes_data", "reachability_status")) {
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

all_outputs <- c("leafletMap", "map_note", "data_status", "storage_taken", "max_radius", "max_capacity",
                 "reachability_status", "nodes_count", "distPlot", "explainer_text_1", "stats_table", "nodes_data")

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
      session$setInputs(storageRadius = radius, minNodesPerNbhood = 2, onlyFullNodes = full)
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
    })
  }
}

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

cat(sprintf("%d checks passed, %d failed\n", passed, failed))
quit(status = if (failed > 0) 1 else 0)
