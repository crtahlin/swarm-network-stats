# Build or extend data/storage-history.csv from swarmscan's archive of daily network dumps.
# Run from the repository root:
#
#   Rscript scripts/build_storage_history.R [from] [to]
#
# from, to: dates as YYYY-MM-DD; the defaults are the first archived day (2024-01-13) and
# yesterday. Dates already in the file are skipped, so a second run only fetches new days, and
# an interrupted run continues where it stopped: each day's row is written as soon as it is done.
# A day missing from the archive is reported and skipped.
#
# Each dump is 5 to 40 MB compressed. It is downloaded to a temporary file, summarised into one
# row (R/storage_history.R, summarise_dump) and deleted. The 00:00 dump is used; on some days it
# was taken before swarmscan had the nodes' status (no statusSnapshot, or status for only a few
# nodes), and then the 06:00, 12:00 and 18:00 dumps are tried in turn. The first with at least
# good_status_nodes reporting nodes is kept, otherwise the one with the most.

source("R/storage_history.R")

archive_url <- "https://swarmscan.sos-ch-dk-2.exo.io/network/dumps"
first_archived_day <- as.Date("2024-01-13")
download_timeout_secs <- 300
good_status_nodes <- 500

args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.Date(args[1]) else first_archived_day
to <- if (length(args) >= 2) as.Date(args[2]) else Sys.Date() - 1

dir.create(dirname(storage_history_file), showWarnings = FALSE)
# days in the range with fewer than min_reporting_nodes reporting nodes are fetched again: their row is dropped
history <- read_storage_history()
thin <- history$nodes_reporting < min_reporting_nodes & history$date >= from & history$date <= to
if (any(thin)) {
  cat(sprintf("fetching %d thin days again\n", sum(thin)))
  kept <- history[!thin, ]
  kept$date <- format(kept$date)
  utils::write.table(kept, storage_history_file, sep = ",", row.names = FALSE)
}
done <- history$date[!thin]
todo <- setdiff(seq(from, to, by = "day"), done)
cat(sprintf("%d days to fetch between %s and %s (%d already in %s)\n",
            length(todo), from, to, sum(done >= from & done <= to), storage_history_file))

# one day's row from the dump taken at the given hour; NULL if that dump cannot be read
day_row <- function(day, hour) {
  url <- sprintf("%s/%s/%s-%02d.json.gz", archive_url, format(day, "%Y/%m/%d"), format(day, "%Y-%m-%d"), hour)
  file <- tempfile(fileext = ".json.gz")
  on.exit(unlink(file))
  tryCatch({
    response <- httr::GET(url, httr::write_disk(file, overwrite = TRUE), httr::timeout(download_timeout_secs))
    if (httr::status_code(response) != 200) stop("HTTP status ", httr::status_code(response))
    summarise_dump(jsonlite::fromJSON(gzfile(file), simplifyVector = TRUE)$nodes, day)
  }, error = function(e) { cat(format(day), sprintf("%02d:00", hour), "skipped:", conditionMessage(e), "\n"); NULL })
}

for (day in todo) {
  day <- as.Date(day, origin = "1970-01-01")
  row <- NULL
  for (hour in c(0, 6, 12, 18)) {
    attempt <- day_row(day, hour)
    if (!is.null(attempt) && (is.null(row) || attempt$nodes_reporting > row$nodes_reporting)) row <- attempt
    if (!is.null(row) && row$nodes_reporting >= good_status_nodes) break
  }
  if (is.null(row)) next
  # append in the file's column order, writing the header only into a new file
  new_file <- !file.exists(storage_history_file) || file.size(storage_history_file) == 0
  if (!new_file) row <- row[, names(utils::read.csv(storage_history_file, nrows = 1))]
  utils::write.table(row, storage_history_file, sep = ",", row.names = FALSE, col.names = new_file, append = !new_file)
  cat(sprintf("%s  %6d nodes  radius %s  stored %.2f TiB\n", format(day), row$nodes, row$radius_mode, row$stored_tib))
  gc(verbose = FALSE)
}
