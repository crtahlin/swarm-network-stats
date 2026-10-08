### data reads in a background R process (issue #48)
# The swarmscan download and the Gnosis chain read take 3 to 15 seconds. Run inside a session's
# poll, they would stall every session of this R process for that long. Instead, each read runs in
# a separate R process started with callr; the session poll only checks whether it has finished,
# which takes no time, and sessions keep showing the last good data meanwhile. The tests set
# background_reads to FALSE, so reads run in the test's own process with its fake chain and data.

background_reads <- TRUE
background_max_secs <- 180   # a read still running after this long is stopped and counted as failed

# what the background process runs: it loads the app's code in a fresh R process and does one read.
# It must not refer to anything outside itself, because callr runs it in another process
background_read <- function(dir, what, previous) {
  setwd(dir)
  env <- new.env()
  suppressMessages(suppressWarnings(source("app.R", local = env)))
  if (what == "swarm") env$compute_swarm_data() else env$fetch_chain_data(previous)
}

# one refresh step for a cache (an environment with data, version, fetched_at, last_attempt,
# last_error, next_attempt and job). compute(previous) does the read; refresh_secs is the interval
# after a good read. Returns the cache's version, which changes only when new data arrives
refresh_cache <- function(cache, what, compute, refresh_secs) {
  now <- current_time()
  finish <- function(result) {
    if (inherits(result, "error")) {
      cache$last_error <- conditionMessage(result)
      cache$next_attempt <- now + if (is.null(cache$data)) retry_interval_secs else retry_with_data_secs
    } else {
      cache$data <- result
      cache$fetched_at <- now
      cache$last_error <- NULL
      cache$version <- cache$version + 1
      cache$next_attempt <- now + refresh_secs
    }
  }
  job <- cache$job
  if (!is.null(job)) {
    if (job$is_alive()) {
      if (as.numeric(now) - as.numeric(cache$last_attempt) > background_max_secs) {
        job$kill()
        cache$job <- NULL
        finish(simpleError(sprintf("the read took longer than %d seconds and was stopped", background_max_secs)))
      }
    } else {
      cache$job <- NULL
      finish(tryCatch(job$get_result(), error = function(e) {
        # callr wraps the child's error; report the child's own message
        simpleError(if (!is.null(e$parent)) conditionMessage(e$parent) else conditionMessage(e))
      }))
    }
  } else if (as.numeric(now) >= as.numeric(cache$next_attempt)) {
    cache$last_attempt <- now
    # without callr, or when the process cannot be started, the read runs here as before
    job <- if (background_reads && requireNamespace("callr", quietly = TRUE)) tryCatch(
      callr::r_bg(background_read, args = list(dir = getwd(), what = what, previous = cache$data),
                  supervise = TRUE, stdout = NULL, stderr = NULL),
      error = function(e) NULL)
    if (!is.null(job)) cache$job <- job else finish(tryCatch(compute(cache$data), error = function(e) e))
  }
  cache$version
}

# the message while a cache has no data yet: reading for the first time, or the last read's error
no_data_message <- function(cache, source) {
  if (is.null(cache$last_error)) paste0("Reading data from ", source, "; this takes up to a minute.") else
    paste0("No data from ", source, " yet (", cache$last_error, "). Retrying every minute.")
}
