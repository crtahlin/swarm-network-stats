### bootnode load: how many joining clients a set of bootnodes can take
# A joining client dials bootnodes to find its first peers. Each connection holds one of the
# bootnode's light peer slots for as long as the client keeps it, so by Little's law the
# concurrent connections on a bootnode are: joins per second x seconds each connection is held x
# bootnodes each client dials / bootnodes. The list is bee's default bootnode address resolved
# from DNS, or any list of multiaddresses pasted in. Client behaviour (joins, hold time, bootnodes
# dialled) is an input.

bee_default_bootnode <- "/dnsaddr/mainnet.ethswarm.org"
bootnode_refresh_secs <- 6 * 60 * 60

# TXT records of _dnsaddr.<name> through DNS over HTTPS; character(0) if there are none
doh_txt <- function(name) {
  response <- httr::GET("https://dns.google/resolve", query = list(name = paste0("_dnsaddr.", name), type = "TXT"),
                        httr::timeout(10))
  if (httr::status_code(response) != 200) stop("DNS over HTTPS answered with HTTP status ", httr::status_code(response))
  answer <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))$Answer
  if (is.null(answer)) character(0) else gsub('^"|"$', "", answer$data)
}

# a multiaddress, with any /dnsaddr/ entries replaced by what they resolve to (libp2p dnsaddr:
# TXT records "dnsaddr=<multiaddress>" on _dnsaddr.<name>), up to `depth` levels
resolve_dnsaddr <- function(address, lookup = doh_txt, depth = 4) {
  if (!startsWith(address, "/dnsaddr/")) return(address)
  if (depth == 0) return(character(0))
  name <- strsplit(address, "/", fixed = TRUE)[[1]][3]
  records <- sub("^dnsaddr=", "", grep("^dnsaddr=", lookup(name), value = TRUE))
  unique(unlist(lapply(records, resolve_dnsaddr, lookup = lookup, depth = depth - 1)))
}

# multiaddresses from pasted text: one per line or separated by spaces or commas
parse_multiaddrs <- function(text) {
  parts <- trimws(unlist(strsplit(if (is.null(text)) "" else text, "[[:space:],]+")))
  unique(parts[startsWith(parts, "/")])
}

# one row per multiaddress: the bootnode (its /p2p/ peer ID), the host (IP address or DNS name)
# and whether it is a WSS address
bootnode_table <- function(addresses) {
  if (length(addresses) == 0) return(data.frame(address = character(0), peer = character(0), host = character(0), wss = logical(0)))
  parts <- strsplit(addresses, "/", fixed = TRUE)
  host <- vapply(parts, function(p) if (length(p) >= 3 && p[2] %in% c("ip4", "ip6", "dns", "dns4", "dns6")) p[3] else NA_character_, "")
  peer <- vapply(addresses, function(a) if (grepl("/p2p/", a, fixed = TRUE)) sub(".*/p2p/", "", a) else a, "", USE.NAMES = FALSE)
  data.frame(address = addresses, peer = peer, host = host,
             wss = grepl("/tls/", addresses, fixed = TRUE) & grepl("/ws", addresses, fixed = TRUE))
}

# concurrent connections per bootnode and the highest join rate the bootnodes take, for `bootnodes`
# bootnodes each with `limit` light peer slots. Each joining client dials min(dialled, bootnodes)
# of them, spread evenly, and keeps each connection for `hold_secs`
bootnode_load <- function(bootnodes, joins_per_min, hold_secs, dialled, limit) {
  per_client <- min(dialled, bootnodes)
  concurrent <- if (bootnodes > 0) joins_per_min / 60 * hold_secs * per_client / bootnodes else NA_real_
  list(per_client = per_client, concurrent = concurrent, load = if (bootnodes > 0) concurrent / limit else NA_real_,
       max_joins_per_min = if (bootnodes > 0 && per_client > 0 && hold_secs > 0) 60 * limit * bootnodes / (hold_secs * per_client) else NA_real_)
}

# the same per host (IP address): bootnodes on each host and their concurrent connections, and the
# load if the host with the most bootnodes is lost and its share moves to the others
bootnode_hosts <- function(table, joins_per_min, hold_secs, dialled, limit) {
  nodes <- unique(table[, c("peer", "host")])
  n <- length(unique(nodes$peer))
  load <- bootnode_load(n, joins_per_min, hold_secs, dialled, limit)
  hosts <- aggregate(peer ~ host, data = transform(nodes, host = ifelse(is.na(host), "(unknown)", host)),
                     FUN = function(p) length(unique(p)))
  names(hosts)[2] <- "bootnodes"
  hosts <- hosts[order(-hosts$bootnodes, hosts$host), ]
  hosts$concurrent <- hosts$bootnodes * load$concurrent
  busiest <- if (nrow(hosts)) hosts$bootnodes[1] else 0
  without <- bootnode_load(n - busiest, joins_per_min, hold_secs, dialled, limit)
  rownames(hosts) <- NULL
  list(bootnodes = n, hosts = hosts, load = load, without_busiest = without, busiest_host = if (nrow(hosts)) hosts$host[1] else NA_character_)
}

### the resolved bee default list, cached like the swarmscan data; the last good list is kept
bootnode_cache <- new.env()
bootnode_cache$addresses <- NULL
bootnode_cache$fetched_at <- NULL
bootnode_cache$last_error <- NULL
bootnode_cache$next_attempt <- -Inf

refresh_bootnode_cache <- function(lookup = doh_txt) {
  now <- current_time()
  if (as.numeric(now) >= as.numeric(bootnode_cache$next_attempt)) {
    result <- tryCatch(resolve_dnsaddr(bee_default_bootnode, lookup), error = function(e) e)
    if (inherits(result, "error") || length(result) == 0) {
      bootnode_cache$last_error <- if (inherits(result, "error")) conditionMessage(result) else "no addresses"
      bootnode_cache$next_attempt <- now + retry_with_data_secs
    } else {
      bootnode_cache$addresses <- result
      bootnode_cache$fetched_at <- now
      bootnode_cache$last_error <- NULL
      bootnode_cache$next_attempt <- now + bootnode_refresh_secs
    }
  }
  bootnode_cache$addresses
}
