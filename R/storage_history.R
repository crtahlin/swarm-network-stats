### stored data over time: the daily history, the growth fit and the radius lines
# Used by app.R (the Data and Storage growth tabs) and by scripts/build_storage_history.R, so the
# history and the live figures are computed the same way

reserve_capacity_chunks <- 2^22      # bee's DefaultReserveCapacity, per node (pkg/storer/storer.go)
chunk_bytes <- 4096
storage_history_file <- "data/storage-history.csv"
# a day whose dump has fewer reporting nodes than this is left out of the plots and fits: the
# median of a handful of nodes says little about the network
min_reporting_nodes <- 100

# the reserve and storage radius of the nodes that report both (above 0), with the reserve per
# neighbourhood. field: the reserve figure to use. Dumps before bee reported reserveSizeWithinRadius
# only have reserveSize, which also counts chunks outside the node's radius. A node with reserve
# doubling d (committedDepth - storageRadius) reports the reserve of all 2^d neighbourhoods it
# stores, so it is divided by 2^d; older dumps have no committedDepth, from before doubling existed
reporting_reserves <- function(nodes, field = "reserveSizeWithinRadius") {
  status <- nodes[["statusSnapshot"]]
  reserve <- status[[field]]; radius <- status[["storageRadius"]]
  if (is.null(reserve) || is.null(radius)) return(data.frame(reserve = numeric(0), radius = numeric(0), doubling = numeric(0)))
  keep <- !is.na(reserve) & !is.na(radius) & reserve > 0 & radius > 0
  committed <- status[["committedDepth"]]
  doubling <- if (is.null(committed)) rep(0, length(reserve)) else pmax(ifelse(is.na(committed), radius, committed) - radius, 0)
  data.frame(reserve = reserve[keep] / 2^doubling[keep], radius = radius[keep], doubling = doubling[keep])
}

# estimated amount of data stored on the network, in TiB (2^40 bytes): each reporting node's
# reserve within radius per neighbourhood, times the 2^radius neighbourhoods, and the median over
# the nodes.
# NA when no node reports its reserve
estimate_stored_tib <- function(nodes, field = "reserveSizeWithinRadius") {
  r <- reporting_reserves(nodes, field)
  if (nrow(r) == 0) return(NA_real_)
  median(r$reserve * chunk_bytes * 2^r$radius) / 2^40
}

# what the network can hold at a radius, in TiB: 2^radius neighbourhoods of one full reserve each
capacity_tib <- function(radius) 2^radius * reserve_capacity_chunks * chunk_bytes / 2^40

# one row of the history for one dump. measure: "within radius" (reserveSizeWithinRadius),
# "whole reserve" (reserveSize) for older dumps without it, which overstates the stored data, or
# "no status" when the dump has no reserve figures at all
summarise_dump <- function(nodes, date) {
  field <- "reserveSizeWithinRadius"
  if (nrow(reporting_reserves(nodes, field)) == 0) field <- "reserveSize"
  r <- reporting_reserves(nodes, field)
  fullness <- r$reserve / reserve_capacity_chunks
  measure <- if (nrow(r) == 0) "no status" else if (field == "reserveSize") "whole reserve" else "within radius"
  data.frame(date = as.Date(date), measure = measure,
             nodes = nrow(nodes), nodes_reporting = nrow(r),
             radius_mode = if (nrow(r)) as.integer(names(which.max(table(r$radius)))) else NA_integer_,
             reserve_median = if (nrow(r)) median(r$reserve) else NA_real_,
             fullness_median = if (nrow(r)) median(fullness) else NA_real_,
             fullness_p90 = if (nrow(r)) unname(stats::quantile(fullness, 0.9)) else NA_real_,
             stored_tib = estimate_stored_tib(nodes, field))
}

# the saved history, sorted by date; an empty table if there is none
read_storage_history <- function(path = storage_history_file) {
  empty <- data.frame(date = as.Date(character(0)), measure = character(0), nodes = numeric(0), nodes_reporting = numeric(0),
                      radius_mode = integer(0), reserve_median = numeric(0), fullness_median = numeric(0),
                      fullness_p90 = numeric(0), stored_tib = numeric(0))
  if (!file.exists(path)) return(empty)
  history <- utils::read.csv(path, stringsAsFactors = FALSE)
  history$date <- as.Date(history$date)
  history[order(history$date), ]
}

### growth fit
# straight-line and exponential fits of stored data against time, over the last `window_days`
# days of the history, using only days measured within radius. NULL when there are fewer than 3 points
fit_growth <- function(history, window_days) {
  recent <- history[history$measure == "within radius" & history$nodes_reporting >= min_reporting_nodes &
                      !is.na(history$stored_tib) & history$stored_tib > 0 & history$date > max(history$date) - window_days, ]
  if (nrow(recent) < 3) return(NULL)
  days <- as.numeric(recent$date)
  list(linear = stats::lm(recent$stored_tib ~ days), exponential = stats::lm(log(recent$stored_tib) ~ days),
       from = min(recent$date), to = max(recent$date))
}

# the fitted curves from the start of the fit window to the horizon, one point a day
project_growth <- function(fit, horizon_days) {
  dates <- seq(fit$from, fit$to + horizon_days, by = "day")
  days <- as.numeric(dates)
  rbind(data.frame(date = dates, stored_tib = stats::coef(fit$linear)[1] + stats::coef(fit$linear)[2] * days, fit = "Straight line"),
        data.frame(date = dates, stored_tib = exp(stats::coef(fit$exponential)[1] + stats::coef(fit$exponential)[2] * days), fit = "Exponential"))
}

# the first date after the last data point at which a fitted curve reaches a level, or NA if it
# never does (a flat or shrinking curve never reaches a higher level, nor a growing one a lower)
crossing_date <- function(fit, level, kind = c("linear", "exponential")) {
  kind <- match.arg(kind)
  coefs <- stats::coef(fit[[kind]])
  value <- if (kind == "linear") level else log(level)
  if (!is.finite(value) || coefs[2] == 0) return(as.Date(NA))
  # the day the curve crosses, as a whole day (the small tolerance keeps rounding error from
  # turning an exact crossing into the day before)
  day <- floor(unname((value - coefs[1]) / coefs[2]) + 1e-9)
  if (day <= as.numeric(fit$to)) return(as.Date(NA))
  as.Date(day, origin = "1970-01-01")
}
