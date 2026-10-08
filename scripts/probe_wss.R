# Check which full nodes advertising a secure WebSocket (WSS) address accept a browser's connection,
# and append one summary row to data/wss-probe.csv. Run from the repository root:
#
#   Rscript scripts/probe_wss.R [parallel]
#
# parallel: how many probes run at once (default 32). For every full node in swarmscan's current
# dump with a WSS address on a public IPv4 address, it opens that address the way a browser does
# (R/wss_probe.R): curl over HTTP/1.1 with certificate checking, asking for a WebSocket upgrade,
# with a 5-second limit. It needs the curl command line tool and takes a few minutes.

suppressMessages(suppressWarnings(source("app.R")))
args <- commandArgs(trailingOnly = TRUE)
parallel_probes <- if (length(args) >= 1) as.integer(args[1]) else 32L
probe_secs <- 5
# the handshake key from RFC 6455's example; any 16 bytes in base64 will do
websocket_key <- jsonlite::base64_enc(charToRaw("the sample nonce"))

raw <- fetch_swarmscan_data()
nodes <- raw$nodes
full <- which(nodes$fullNode %in% TRUE)
urls <- vapply(full, function(i) {
  u <- nodes$underlays[[i]]
  if (is.null(u) || NROW(u) == 0) NA_character_ else node_wss_url(u$address)
}, "")
wss_nodes <- sum(vapply(full, function(i) { u <- nodes$underlays[[i]]; !is.null(u) && NROW(u) > 0 && any(!is.na(wss_url(u$address))) }, TRUE))
targets <- urls[!is.na(urls)]
cat(sprintf("%d full nodes, %d with a WSS address, %d of them on a public address; probing\n", length(full), wss_nodes, length(targets)))

probe <- function(url) {
  https <- sub("^wss://", "https://", url)
  out <- suppressWarnings(system2("curl", c("--http1.1", "-s", "-o", "/dev/null", "--max-time", probe_secs,
                                            "-w", shQuote("%{http_code} %{ssl_verify_result} %{time_appconnect}"),
                                            "-H", shQuote("Connection: Upgrade"), "-H", shQuote("Upgrade: websocket"),
                                            "-H", shQuote("Sec-WebSocket-Version: 13"), "-H", shQuote(paste("Sec-WebSocket-Key:", websocket_key)),
                                            shQuote(https)), stdout = TRUE, stderr = FALSE))
  status <- attr(out, "status"); if (is.null(status)) status <- 0
  fields <- strsplit(if (length(out)) out[1] else "0 0 0", " ")[[1]]
  data.frame(url = url, exit_status = status, http_code = as.integer(fields[1]), ssl_verify = as.integer(fields[2]),
             tls_secs = as.numeric(fields[3]))
}
results <- parallel::mclapply(targets, probe, mc.cores = parallel_probes)
# a probe whose worker failed counts as no connection
results <- do.call(rbind, lapply(seq_along(results), function(k) if (is.data.frame(results[[k]])) results[[k]] else
  data.frame(url = targets[k], exit_status = NA, http_code = 0L, ssl_verify = NA_integer_, tls_secs = NA_real_)))
results$outcome <- classify_probe(results$exit_status, results$http_code, results$ssl_verify)
print(table(results$outcome))

counts <- table(factor(results$outcome, levels = wss_probe_outcomes))
row <- data.frame(date = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"), full_nodes = length(full), wss_nodes = wss_nodes,
                  probed = nrow(results), check.names = FALSE)
for (o in wss_probe_outcomes) row[[o]] <- as.integer(counts[[o]])
row$median_tls_secs <- round(stats::median(results$tls_secs[results$outcome == "accepted"]), 3)
dir.create(dirname(wss_probe_file), showWarnings = FALSE)
# appended by column name, so a file written with other columns still lines up
old <- if (file.exists(wss_probe_file)) utils::read.csv(wss_probe_file, check.names = FALSE, stringsAsFactors = FALSE) else NULL
if (!is.null(old)) for (col in setdiff(names(row), names(old))) old[[col]] <- NA
if (!is.null(old)) for (col in setdiff(names(old), names(row))) row[[col]] <- NA
utils::write.csv(rbind(old, row[names(if (is.null(old)) row else old)]), wss_probe_file, row.names = FALSE)
cat(sprintf("%d of %d probed WSS full nodes accepted a browser-style WebSocket connection (%.1f%%); saved to %s\n",
            row$accepted, row$probed, 100 * row$accepted / row$probed, wss_probe_file))
