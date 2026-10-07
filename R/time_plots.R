### zoom and pointer read-outs for the plots against time (Price projection, Storage growth)
# Drag across a plot to zoom into that period; double-click to zoom out. The plots use Shiny's
# own brush, double-click and hover events, so they need no extra library. Positions arrive as
# numbers on the time axis: days since 1970 for dates, seconds for date-times

# the x and y limits for the zoomed period, or NULL for the whole plot. series: data frames with
# columns x and y; y is taken from the points in view, with a little room above and below
zoom_limits <- function(zoom, series) {
  if (is.null(zoom)) return(NULL)
  y <- unlist(lapply(series, function(s) s$y[as.numeric(s$x) >= zoom[1] & as.numeric(s$x) <= zoom[2]]))
  y <- y[is.finite(y)]
  if (length(y) == 0) return(list(x = zoom, y = NULL))
  pad <- max(diff(range(y)) * 0.05, abs(max(y)) * 0.01, 1e-9)
  list(x = zoom, y = c(min(y) - pad, max(y) + pad))
}

# the plot's coordinates for a zoom: limits for a date or date-time axis, or the whole plot
zoom_coord <- function(limits, time_type = c("date", "datetime")) {
  if (is.null(limits)) return(coord_cartesian())
  x <- if (match.arg(time_type) == "date") as.Date(limits$x, origin = "1970-01-01") else
    as.POSIXct(limits$x, origin = "1970-01-01", tz = "UTC")
  coord_cartesian(xlim = x, ylim = limits$y, expand = FALSE)
}

# each series' value at a position on the time axis: the nearest point, or the last point at or
# before the position for series drawn as steps. A series whose points do not reach the position
# (within max_gap, in axis units) gives NA
value_at <- function(s, at, step = FALSE, max_gap = Inf) {
  x <- as.numeric(s$x)
  if (length(x) == 0 || at < min(x) - max_gap || at > max(x) + max_gap) return(NA_real_)
  if (step) {
    before <- which(x <= at)
    return(if (length(before)) s$y[before[which.max(x[before])]] else NA_real_)
  }
  s$y[which.min(abs(x - at))]
}

# the hint shown above a plot until the pointer is over it
time_plot_hint <- "Point at the plot to read the values; drag across it to zoom in; double-click it or use Zoom out to see it all again."
