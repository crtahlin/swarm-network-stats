### which full nodes advertising a secure WebSocket address accept a browser's connection (issue #70)
# A browser on an HTTPS page opens wss://<name>:<port>/ for a /tls/sni/<name>/ws address: the name
# must resolve, the certificate must be valid for it, and the node must answer the WebSocket
# upgrade with 101 Switching Protocols. scripts/probe_wss.R checks this for every full node with a
# public WSS address and appends one summary row to data/wss-probe.csv; the Connectivity tab shows
# the latest row. Whether the node then has room for one more light peer is not part of the check.

wss_probe_file <- "data/wss-probe.csv"

# the wss:// URL a browser opens for a /ip4/<ip>/tcp/<port>/tls/sni/<name>/ws multiaddress, or NA
wss_url <- function(address) {
  m <- regmatches(address, regexec("^/(ip4|ip6|dns|dns4|dns6)/([^/]+)/tcp/([0-9]+)/tls/sni/([^/]+)/ws(/|$)", address))
  vapply(m, function(x) if (length(x) == 0) NA_character_ else paste0("wss://", x[5], ":", x[4], "/"), "")
}

# the WSS address a browser would try for one node: its first one on a public IPv4 address (or a
# DNS name), as a wss:// URL; NA if it has none
node_wss_url <- function(addresses) {
  ip <- sub("^/ip4/([^/]+)/.*", "\\1", addresses)
  public <- !grepl("^/ip4/", addresses) | is_public_ip4(ip)
  urls <- wss_url(addresses[public])
  urls <- urls[!is.na(urls)]
  if (length(urls)) urls[1] else NA_character_
}

# the outcome of one probe from curl's exit status and its http_code and ssl_verify_result output:
# "accepted" (101; curl then waits on the open WebSocket until its time limit, exit status 28),
# "name does not resolve", "no connection", "certificate or TLS failed" or "no WebSocket upgrade"
classify_probe <- function(exit_status, http_code, ssl_verify) {
  ifelse(http_code == 101 & ssl_verify == 0, "accepted",
    ifelse(exit_status == 6, "name does not resolve",
      ifelse(exit_status %in% c(35, 51, 58, 59, 60, 77, 83, 90, 91) | (!is.na(ssl_verify) & ssl_verify != 0), "certificate or TLS failed",
        ifelse(http_code == 0, "no connection", "no WebSocket upgrade"))))
}

wss_probe_outcomes <- c("accepted", "no connection", "certificate or TLS failed", "no WebSocket upgrade", "name does not resolve")

# the last probe summary, or NULL if there is none
read_wss_probe <- function(path = wss_probe_file) {
  if (!file.exists(path) || file.size(path) == 0) return(NULL)
  probes <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(probes) == 0) return(NULL)
  probes[nrow(probes), ]
}

# one sentence on the latest probe, for the Connectivity tab; NULL row: says it was not measured
wss_probe_text <- function(row) {
  if (is.null(row)) return("Whether the WSS underlays actually accept browser connections has not been measured.")
  failed <- vapply(wss_probe_outcomes[-1], function(o) row[[o]], 0)
  failed <- failed[failed > 0]
  sprintf("Measured %s UTC: %s of %s full nodes with a public WSS underlay accepted a browser's connection (valid certificate and WebSocket upgrade; %.1f%%)%s. Whether they then had room for another light peer is not part of the check.",
          row$date, format_number(row$accepted), format_number(row$probed), 100 * row$accepted / row$probed,
          if (length(failed)) paste0("; the rest: ", paste(sprintf("%s %s", format_number(failed), names(failed)), collapse = ", ")) else "")
}
