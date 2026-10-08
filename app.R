#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### load libraries
# httr and jsonlite are used as httr:: and jsonlite::, so they are not attached
library(shiny)
library(ggplot2)
library(DT)
library(leaflet)
library(forstringr)  # str_right
library(SwarmR)      # first_n_places
library(dplyr)
library(bslib)


### load data from swarmscan.io
swarmscan_dump_url <- "https://api.swarmscan.io/v1/network/dump"
refresh_interval_secs <- 10 * 60  # download new data this often
retry_interval_secs <- 60         # after a failed download with no data yet, try again this soon
retry_with_data_secs <- 5 * 60    # after a failed refresh while older data is shown, try again this soon
# the download took 10-12 s on 2026-10-07 (about 32 MB, sent deflate-compressed as about 5 MB; httr
# asks for compression by default) and grows with the network. A hanging server blocks every
# session until this runs out (issue #48), so it stays moderate
download_timeout_secs <- 60
current_time <- function() Sys.time()

# download the network dump; stops with a readable message on a network error,
# a non-200 response, or a response that does not contain the nodes table
fetch_swarmscan_data <- function() {
  response <- httr::GET(swarmscan_dump_url, httr::timeout(download_timeout_secs))
  if (httr::status_code(response) != 200) {
    stop("swarmscan answered with HTTP status ", httr::status_code(response))
  }
  data <- jsonlite::parse_json(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  if (!is.data.frame(data$nodes) || nrow(data$nodes) == 0 || is.null(data$nodes$overlay)) {
    stop("swarmscan's response does not contain a list of nodes")
  }
  data
}

# a number as page text: thousands separator and at most two decimals, e.g. 4,693 or 7.32;
# values below 1 keep 3 significant digits, so 16 GiB shows as 0.0156 TiB rather than 0.02
format_number <- function(x) {
  x <- ifelse(abs(x) < 1, signif(x, 3), round(x, 2))
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

# overlay addresses as bit strings. The app never needs more than the first few bits (the radius
# input goes up to 16, and the radius search stops long before 64), so only the first 16 hex digits
# (64 bits) are converted, one vectorised lookup per digit, instead of SwarmR's hexadecimal2binary,
# which converts all 256 bits one character at a time
hex_bits <- c("0" = "0000", "1" = "0001", "2" = "0010", "3" = "0011", "4" = "0100", "5" = "0101",
              "6" = "0110", "7" = "0111", "8" = "1000", "9" = "1001", "a" = "1010", "b" = "1011",
              "c" = "1100", "d" = "1101", "e" = "1110", "f" = "1111")
overlay_to_bits <- function(overlay, hex_digits = 16) {
  hex <- tolower(substr(overlay, 1, hex_digits))
  bits <- character(length(hex))
  for (k in seq_len(hex_digits)) bits <- paste0(bits, hex_bits[substr(hex, k, k)])
  unname(bits)
}

# names of all 2^radius nbhoods in order ("00", "01", "10", "11" for radius 2), built by doubling
# the list and cached per radius; replaces SwarmR's generate_short_overlay, which loops over
# 2^radius numbers on every call
nbhood_name_cache <- new.env()
nbhood_names <- function(radius) {
  # 2^radius names are built and kept, so the radius must stay small (the input allows 1 to 16)
  stopifnot(length(radius) == 1, radius %in% 0:16)
  key <- as.character(radius)
  if (is.null(nbhood_name_cache[[key]])) {
    names <- ""
    for (k in seq_len(radius)) names <- as.vector(t(outer(names, c("0", "1"), paste0)))
    nbhood_name_cache[[key]] <- names
  }
  nbhood_name_cache[[key]]
}

# TRUE where a string field is present and not empty; a missing field gives FALSE
has_text <- function(x) {
  if (is.null(x)) return(FALSE)
  !is.na(x) & nchar(x) > 0
}

# TRUE for IPv4 addresses reachable on the public internet (not private, loopback, link-local, CGNAT or multicast)
is_public_ip4 <- function(ip) {
  vapply(strsplit(ip, ".", fixed = TRUE), function(o) {
    o <- suppressWarnings(as.integer(o))
    if (length(o) != 4 || anyNA(o)) return(FALSE)
    !(o[1] %in% c(0, 10, 127) || o[1] >= 224 ||
        (o[1] == 172 && o[2] >= 16 && o[2] <= 31) || (o[1] == 192 && o[2] == 168) ||
        (o[1] == 169 && o[2] == 254) || (o[1] == 100 && o[2] >= 64 && o[2] <= 127))
  }, logical(1))
}

# TRUE for IPv6 addresses reachable on the public internet (not loopback, unique local, link-local, multicast or IPv4-mapped)
is_public_ip6 <- function(ip) {
  ip <- tolower(ip)
  !is.na(ip) & grepl(":", ip, fixed = TRUE) &
    !(ip %in% c("::", "::1") | grepl("^(fc|fd|fe[89ab]|ff)", ip) | startsWith(ip, "::ffff:"))
}

# public IP addresses of one node, taken from its underlay multiaddresses (/ip4/<address>/... or /ip6/<address>/...)
public_ips <- function(underlays) {
  if (is.null(underlays) || NROW(underlays) == 0) return(character(0))
  parts <- strsplit(underlays$address, "/", fixed = TRUE)
  family <- vapply(parts, `[`, "", 2)
  ip <- vapply(parts, `[`, "", 3)
  keep <- (family %in% "ip4" & is_public_ip4(ip)) | (family %in% "ip6" & is_public_ip6(ip))
  unique(ip[keep])
}

### prepare data
# turn the downloaded dump into the nodes table the app works with
prepare_nodes_data <- function(swarmscan_data) {
  # extract data about nodes
  nodes_data <- swarmscan_data$nodes
  # calculate binary overlay address (first 64 bits) and add it to data
  nodes_data$overlay_binary <- overlay_to_bits(nodes_data$overlay)
  # if unreachable column does not exist, fill it with NAs (to avoid corner case)
  if (is.null(nodes_data$unreachable)) {nodes_data$unreachable <- NA}

  # the location table; [[ ]] matches names exactly, because $ would partially match a missing
  # "location" to the "location_source" column added below. A dump without any location gets an
  # all-missing table, so the map and the Nodes info tab still work
  location <- nodes_data[["location"]]
  if (!is.data.frame(location)) location <- data.frame(row.names = seq_len(nrow(nodes_data)))
  # missing fields are added with the right type: the map needs numeric coordinates even when all are missing
  field_or_na <- function(field) if (is.null(location[[field]])) NA else location[[field]]
  for (field in c("latitude", "longitude")) location[[field]] <- as.numeric(field_or_na(field))
  for (field in c("country", "city")) location[[field]] <- as.character(field_or_na(field))
  
  # swarmscan gives nodes it failed to geolocate the placeholder location 0,0 with no (or an empty)
  # country, which would put them in the ocean off West Africa; treat it as missing, and treat a
  # location with only one of the two coordinates as missing too
  no_country <- is.na(location$country) | location$country == ""
  placeholder <- !is.na(location$latitude) & !is.na(location$longitude) &
    location$latitude == 0 & location$longitude == 0 & no_country
  unusable <- placeholder | is.na(location$latitude) | is.na(location$longitude)
  location$latitude[unusable] <- NA
  location$longitude[unusable] <- NA
  location$country[no_country] <- NA
  nodes_data[["location"]] <- location
  nodes_data$location_source <- ifelse(is.na(location$latitude), NA, "swarmscan")

  # the same public IP is often located for one node and not for another, so borrow the location
  # from a located node that shares a public IP; private addresses (e.g. Docker's 172.17.0.1) are
  # shared by unrelated nodes and are never used. If candidates differ, the most common location wins
  underlays <- nodes_data[["underlays"]]
  if (is.null(underlays)) underlays <- vector("list", nrow(nodes_data))
  node_ips <- lapply(underlays, public_ips)
  # the address the Map tab groups nodes by (R/map_markers.R)
  nodes_data$public_ip <- vapply(node_ips, group_ip, "")
  located <- which(!is.na(location$latitude))
  ip_rows <- data.frame(ip = unlist(node_ips[located]), row = rep(located, lengths(node_ips[located])))
  location_key <- do.call(paste, c(location, sep = "|"))
  for (i in which(is.na(location$latitude))) {
    candidates <- ip_rows$row[ip_rows$ip %in% node_ips[[i]]]
    if (length(candidates) == 0) next
    keys <- location_key[candidates]
    best <- candidates[match(names(which.max(table(keys))), keys)]
    nodes_data[["location"]][i, ] <- location[best, ]
    nodes_data$location_source[i] <- "same IP"
  }
  
  # the kinds of address a node offers: a secure WebSocket address (/tls/.../ws), the only kind a
  # browser on an HTTPS page can open; a plain TCP address; and anything else
  address_kind <- function(u, kind) {
    if (is.null(u) || NROW(u) == 0) return(FALSE)
    secure_ws <- grepl("/tls/(sni/[^/]+/)?ws(/|$)", u$address)
    plain_tcp <- grepl("^/(ip4|ip6|dns|dns4|dns6)/[^/]+/tcp/[0-9]+(/p2p/[^/]+)?$", u$address)
    any(switch(kind, secure_ws = secure_ws, plain_tcp = plain_tcp, other = !secure_ws & !plain_tcp))
  }
  nodes_data$secure_websocket <- vapply(underlays, address_kind, logical(1), kind = "secure_ws")
  nodes_data$plain_tcp <- vapply(underlays, address_kind, logical(1), kind = "plain_tcp")
  nodes_data$other_address <- vapply(underlays, address_kind, logical(1), kind = "other")

  # the underlay addresses were only needed for borrowing locations and the flag above; dropping
  # them halves the cache
  nodes_data[["underlays"]] <- NULL

  nodes_data
}

# the network's radius, used as the default radius: the most common committed depth (storage
# radius + reserve doubling; doubled nodes report a storage radius d lower), or the most common
# storage radius in dumps without committedDepth. NULL if no node reports one
typical_storage_radius <- function(nodes_data) {
  status <- nodes_data[["statusSnapshot"]]
  radius <- status[["storageRadius"]]
  if (is.null(radius)) return(NULL)
  committed <- status[["committedDepth"]]
  depth <- if (is.null(committed)) radius else ifelse(is.na(committed), radius, pmax(committed, radius))
  depth <- depth[!is.na(radius) & radius > 0]
  if (length(depth) == 0) return(NULL)
  as.integer(names(which.max(table(depth))))
}

### shared data cache
# one copy of the data for all sessions of this R process. refresh_swarm_cache() starts a download
# in a background process when it is due and takes its result when it is done; if a download or its preparation fails, the last good data is kept and
# the download is retried (every minute with no data, every 5 minutes with older data shown).
# version changes only when new data arrives
swarm_cache <- new.env()
swarm_cache$data <- NULL          # list(counts = swarmscan's node counts, nodes = prepared nodes table)
swarm_cache$version <- 0
swarm_cache$fetched_at <- NULL
swarm_cache$last_attempt <- NULL
swarm_cache$last_error <- NULL
swarm_cache$next_attempt <- -Inf

swarm_cache$job <- NULL           # the background read while one runs (R/background.R)

# one read of the swarmscan data: download and prepare. It runs in a background process
compute_swarm_data <- function(previous = NULL) {
  raw <- fetch_swarmscan_data()
  # keep only what the app reads, so the raw nodes table is not held twice
  list(counts = list(count = raw$count, unreachableCount = raw$unreachableCount),
       nodes = prepare_nodes_data(raw))
}

refresh_swarm_cache <- function() refresh_cache(swarm_cache, "swarm", compute_swarm_data, refresh_interval_secs)

# one line saying how fresh the data is and whether the last download failed
data_status_text <- function() {
  stamp <- function(t) format(t, "%Y-%m-%d %H:%M UTC", tz = "UTC")
  if (is.null(swarm_cache$data)) {
    return(no_data_message(swarm_cache, "swarmscan"))
  }
  status <- paste0("Data from swarmscan, fetched ", stamp(swarm_cache$fetched_at),
                   ". Refreshed every ", refresh_interval_secs / 60, " minutes.")
  if (!is.null(swarm_cache$last_error)) {
    status <- paste0(status, " The last refresh failed at ", stamp(swarm_cache$last_attempt),
                     " (", swarm_cache$last_error, "), so the data shown is older. Next attempt at ",
                     stamp(swarm_cache$next_attempt), ".")
  }
  status
}

# stake, reveals and price from the Gnosis chain, with their own cache (chain_cache)
# local = TRUE: runApp evaluates app.R in its own environment, which chain.R must see
source("R/chain.R", local = TRUE)
# the Nbhood map
source("R/nbhood_map.R", local = TRUE)
# the Price projection
source("R/price_model.R", local = TRUE)
# stored data over time (the Data and Storage growth tabs); the history is built by
# scripts/build_storage_history.R and read once when the app starts
source("R/storage_history.R", local = TRUE)
storage_history_data <- read_storage_history()
# zoom and pointer read-outs for the plots against time
source("R/time_plots.R", local = TRUE)
# the Connectivity tab: light-client capacity
source("R/light_capacity.R", local = TRUE)
# the Connectivity tab: bootnode load while clients join
source("R/bootnodes.R", local = TRUE)
# data reads in a background process
source("R/background.R", local = TRUE)
# the Connectivity tab: the measured share of WSS full nodes that accept a browser connection
source("R/wss_probe.R", local = TRUE)
wss_probe_latest <- read_wss_probe()
# the Map tab: one marker per public IP address
source("R/map_markers.R", local = TRUE)


# column names of the staked-nodes table on the Nodes info tab, each with the explanation its
# header shows on hover
stakes_table_columns <- c(
  "Neighbourhood" = "The overlay's neighbourhood at the storage radius set in the sidebar",
  "Overlay" = "The node's overlay address, as registered with its stake",
  "Stake (BZZ)" = "The amount deposited",
  "Effective stake (BZZ)" = "What the redistribution game counts: the committed stake at today's price, capped at the deposit, and 0 while frozen",
  "Height" = "Reserve doubling: how many times the node has doubled its storage",
  "Frozen" = "Whether the stake is currently frozen",
  "Can play" = "Whether the node can take part in the game now: staked at least 2 rounds ago and not frozen",
  "Last reveal (UTC)" = "Time of the node's latest reveal within the days set in the sidebar (Active within); empty if it has not played in that time",
  "Last round" = "Round of the node's latest reveal within the days set in the sidebar (Active within); empty if it has not played in that time",
  "Matched truth" = "Whether that reveal matched the round's agreed result; empty if the round has not been claimed",
  "In swarmscan" = "Whether swarmscan lists the node at all"
)
# a DT table header from a named vector of column name = explanation; the explanation shows
# when the pointer rests on the column name (the title attribute)
header_with_tooltips <- function(columns) {
  tags$table(class = "display", tags$thead(tags$tr(
    lapply(names(columns), function(name) tags$th(title = columns[[name]], name)))))
}
stakes_table_header <- function() header_with_tooltips(stakes_table_columns)

# columns of the node table under the Map
map_nodes_columns <- c(
  "Overlay" = "The node's overlay address",
  "User agent" = "The bee version the node reports",
  "Full node" = "Whether swarmscan lists it as a full node; unknown for nodes swarmscan did not reach",
  "Reached by swarmscan" = "Whether swarmscan reached the node",
  "Stake (xBZZ)" = "The amount deposited; empty for a node without stake",
  "Height" = "Reserve doubling: the node stores 2^height neighbourhoods",
  "Height from" = "chain: the staked height; self-reported: committed depth minus storage radius from the node's status; none: 0 assumed",
  "Neighbourhood" = "The overlay's neighbourhood at the network's radius",
  "Location source" = "swarmscan: located by swarmscan; same IP: taken from another node with the same public IP address"
)

# columns of the node table under the Nbhood map
nbhood_nodes_columns <- c(
  "Overlay" = "The node's overlay address",
  "Kind" = paste0("Staked, active: staked and revealed in the set number of days. Staked, idle: staked, no reveal in that time. ",
                  "Full, not staked: a full node in the swarmscan dump without stake. Idle also covers stakes that cannot play: ",
                  "frozen, below the minimum stake, or too new"),
  "Stake (BZZ)" = "The amount deposited; empty for a node without stake",
  "Effective stake (BZZ)" = "What the redistribution game counts: the committed stake at today's price, capped at the deposit, and 0 while frozen",
  "Height" = "Reserve doubling: the node stores 2^height neighbourhoods and is listed in each of them",
  "Last reveal (UTC)" = paste0("Time of the node's latest reveal in the set number of days; empty if it has not played in that time"),
  "Last round" = "Round of that reveal",
  "Matched truth" = "Whether that reveal matched the round's agreed result; empty if the round has not been claimed",
  "In swarmscan" = "Whether swarmscan lists the node",
  "Reachable" = "Whether the node reports itself reachable, from swarmscan's status snapshot",
  "Country" = "From swarmscan's location data",
  "User agent" = "The bee version the node reports to swarmscan"
)


### LOOK AND FEEL
# dark slate, orange and mint, after the colours of ethswarm.org (not an exact copy).
# Space Grotesk for text; JetBrains Mono for labels, figures, tables and overlay bit strings.
# The fonts load from Google Fonts in the browser, so the server needs no font files
swarm_colours <- list(
  bg = "#0d1216", surface = "#151c22", line = "#2d3843", text = "#e7eaee", muted = "#8b909a",
  orange = "#ff6b26", mint = "#14fec0",
  bars = "#5b6b7a", error = "#f2c14e", unreachable = "#ff4d5e"  # plot: grey, yellow, red
)
mono_font <- font_google("JetBrains Mono", local = FALSE)
swarm_theme <- bs_theme(
  version = 5,
  bg = swarm_colours$bg, fg = swarm_colours$text,
  primary = swarm_colours$orange, secondary = swarm_colours$line, success = swarm_colours$mint,
  base_font = font_google("Space Grotesk", local = FALSE),
  heading_font = mono_font, code_font = mono_font,
  "border-color" = swarm_colours$line
)
swarm_css <- paste0("
  :root { --mono: 'JetBrains Mono', ui-monospace, monospace; }
  .leaflet-tooltip.marker-count { color: ", swarm_colours$bg, "; font-family: var(--mono); font-weight: 700; font-size: 11px; }
  .navbar { border-bottom: 1px solid ", swarm_colours$line, "; }
  .navbar-brand { font-family: var(--mono); letter-spacing: 0.04em; }
  .navbar-brand .hex { color: ", swarm_colours$orange, "; margin-right: 0.4em; }
  .navbar .nav-link { font-family: var(--mono); font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.08em; }
  .navbar .nav-link.active { color: ", swarm_colours$orange, " !important; box-shadow: inset 0 -2px 0 ", swarm_colours$orange, "; }
  .bslib-sidebar-layout > .sidebar { background: ", swarm_colours$surface, "; border-right: 1px solid ", swarm_colours$line, "; }
  .sidebar-title, .section-label { font-family: var(--mono); font-size: 0.75rem; text-transform: uppercase;
                                   letter-spacing: 0.1em; color: ", swarm_colours$muted, "; margin: 0.25rem 0 0.75rem; }
  .section-label::before { content: '> '; color: ", swarm_colours$orange, "; }
  #data_status, #chain_status, #map_note { font-family: var(--mono); font-size: 0.8rem; color: ", swarm_colours$muted, "; }
  #nodes_count { font-family: var(--mono); font-weight: bold; color: ", swarm_colours$mint, "; margin-bottom: 1em; }
  .bslib-value-box { background: ", swarm_colours$surface, " !important; border: 1px solid ", swarm_colours$line, "; }
  .bslib-value-box .value-box-title { font-family: var(--mono); font-size: 0.8rem; text-transform: uppercase;
                                      letter-spacing: 0.08em; color: ", swarm_colours$muted, "; }
  .bslib-value-box .value-box-value { font-family: var(--mono); color: ", swarm_colours$mint, "; }
  .bslib-value-box .value-box-value .shiny-output-error-validation { font-size: 0.9rem; color: ", swarm_colours$error, "; }
  .bslib-value-box p { font-size: 0.8rem; color: ", swarm_colours$muted, "; }
  table.dataTable, .dataTables_wrapper { font-family: var(--mono); font-size: 0.8rem; }
  .table { --bs-table-striped-bg: rgba(139, 144, 154, 0.07); --bs-table-hover-bg: rgba(255, 107, 38, 0.10); }
  #explainer_text_1 { font-size: 0.85rem; color: ", swarm_colours$muted, "; }
  .marker-cluster-small, .marker-cluster-medium, .marker-cluster-large { background-color: rgba(255, 107, 38, 0.25); }
  .marker-cluster-small div, .marker-cluster-medium div, .marker-cluster-large div {
    background-color: rgba(255, 107, 38, 0.85); color: ", swarm_colours$bg, "; font-family: var(--mono); font-weight: bold; }
")

# the same look for the neighbourhood plot (R graphics use the system's monospace font)
swarm_plot_theme <- theme(
  plot.background = element_rect(fill = swarm_colours$bg, colour = NA),
  panel.background = element_rect(fill = swarm_colours$surface, colour = NA),
  panel.grid.major = element_line(colour = swarm_colours$line),
  panel.grid.minor = element_blank(),
  text = element_text(size = 14),
  axis.text = element_text(colour = "#aab2bc", family = "mono", size = 11),
  axis.title = element_text(colour = swarm_colours$text, family = "mono", size = 13),
  axis.ticks = element_line(colour = swarm_colours$line)
)


# larger, bold text for the wide plots (Price projection, Storage growth); the shared theme's sizes
# read too small there
swarm_readable_text <- theme(
  axis.text = element_text(colour = swarm_colours$text, family = "mono", face = "bold", size = 15),
  axis.title.y = element_text(colour = swarm_colours$text, family = "mono", face = "bold", size = 16),
  plot.caption = element_text(colour = swarm_colours$text, family = "mono", face = "bold", size = 14),
  legend.position = "top", legend.background = element_rect(fill = swarm_colours$bg),
  legend.key = element_rect(fill = swarm_colours$bg),
  legend.text = element_text(colour = swarm_colours$text, family = "mono", face = "bold", size = 14),
  legend.title = element_blank()
)


### APPLICATION
### UI part
# the tabs each sidebar setting applies to; the sidebar shows a setting only while one of them is open
setting_tabs <- list(
  storageRadius = c("Nbhood plot", "Nbhood map", "Price projection", "Storage growth", "Nbhoods stats", "Nodes info"),
  minNodesPerNbhood = c("Data"),
  onlyFullNodes = c("Map", "Data", "Reachability", "Nbhood plot", "Nbhoods stats", "Nodes info"),
  activeDays = c("Nbhood map", "Price projection", "Nodes info")
)
# a sidebar setting shown only on its tabs. The input keeps its value while it is hidden
for_tabs <- function(setting, input) {
  conditionalPanel(sprintf("[%s].includes(input.tab)", paste0("'", setting_tabs[[setting]], "'", collapse = ", ")), input)
}

ui <-
  page_navbar(
    title = span(span(class = "hex", HTML("&#x2B22;")), "swarm network stats"),
    theme = swarm_theme,
    # tabs keep their fixed heights and the page scrolls; filling the window squeezed the
    # 800px plot so the text under it overlapped the axis
    fillable = FALSE,
    header = tags$style(HTML(swarm_css)),
    # settings part
    id = "tab",
    sidebar = sidebar(title = "Settings",
                      # 9 until data arrives; then the radius most nodes report (see server)
                      for_tabs("storageRadius", numericInput("storageRadius", "Storage radius",
                                                             value = 9, min = 1, max = 16, step = 1)),
                      for_tabs("minNodesPerNbhood", numericInput("minNodesPerNbhood", "Minimum nodes per nbhood",
                                                                 value = 2, min = 1, max = 8)),
                      for_tabs("onlyFullNodes", checkboxInput("onlyFullNodes", "Show only full nodes",
                                                              value = TRUE)),
                      for_tabs("activeDays", numericInput("activeDays", "Active within (days)",
                                                          value = chain_window_days, min = 1, max = chain_window_days, step = 1)),
                      conditionalPanel(sprintf("![%s].includes(input.tab)",
                                               paste0("'", unique(unlist(setting_tabs)), "'", collapse = ", ")),
                                       p("No setting applies to this tab.")),
                      textOutput("data_status"),
                      textOutput("chain_status")),
    # panels part
    ###
    nav_panel("Map", 
              div(class = "section-label", "Map of nodes"),
              p("One marker per public IP address; click a marker to list the nodes behind it under the map. Markers and cluster numbers count ",
                "the setting below. Neighbourhoods covered counts 2^height for each node: a node with reserve doubling stores ",
                "2^height neighbourhoods. Height comes from the chain for staked nodes, and from the node's own status ",
                "(committed depth minus storage radius) for the others. One IP address is not always one machine: several ",
                "machines can share one behind NAT, one machine can have several, and a data centre gateway can front many ",
                "operators. The last part of each address is hidden. Neighbourhoods are counted at the radius the nodes report ",
                "(their most common committed depth), not the sidebar radius."),
              layout_column_wrap(
                width = 1/2, fill = FALSE,
                radioButtons("mapCountBy", "Count by", choices = map_count_choices, selected = "nodes", inline = TRUE, width = "100%"),
                checkboxInput("mapOnlyStaked", "Only staked nodes", value = FALSE, width = "100%")
              ),
              textOutput("map_note", container = p),
              leafletOutput("leafletMap", height = "800px"),
              br(),
              textOutput("map_selected", container = p),
              DT::dataTableOutput("map_nodes")),
    ###
    nav_panel("Data", 
              div(class = "section-label", "Storage on the network"),
              layout_column_wrap(
                width = 1/4, fill = FALSE,
                value_box(title = "Stored data", value = textOutput("storage_taken"),
                          p("Estimated total amount of stored data, in TiB (2^40 bytes), from all nodes whatever the filter")),
                value_box(title = "Maximum storage radius", value = textOutput("max_radius"),
                          p("With the set minimum required nodes per neighbourhood")),
                value_box(title = "Maximum capacity", value = textOutput("max_capacity"),
                          p("Of storage at that radius, in TiB (2^40 bytes)")),
                value_box(title = "Committed depth the nodes report", value = textOutput("reported_radius"),
                          p("The most common committed depth (storage radius + reserve doubling), from all nodes whatever the filter"))
              )
              ), 
    
    ### 
    nav_panel("Reachability",
              div(class = "section-label", "Reachability of nodes"),
              p("\"Reached by swarmscan\" is swarmscan's own check. \"Reachable (self-reported)\" is what the node reports ",
                "about itself in its status snapshot; it is unknown when there is no snapshot or swarmscan could not get one. swarmscan sets \"Full node\" only ",
                "on nodes it reached, so \"Show only full nodes\" leaves out every node it did not reach."),
              DT::dataTableOutput("reachability_status")),
    
    ###
    nav_panel("Nbhood plot", 
              div(class = "section-label", "Number of nodes (all nodes, whatever the filter)"),
              textOutput("nodes_count"),
              div(class = "section-label", "Count of nodes per neighbourhood"),
              plotOutput("distPlot", height = "800px"),
              br(),
              textOutput("explainer_text_1")),

    ###
    nav_panel("Nbhood map",
              div(class = "section-label", "Staked nodes per neighbourhood"),
              p("One tile per neighbourhood at the storage radius set in the sidebar. Sister neighbourhoods, ",
                "the two halves one split would create, sit next to each other. The colour counts staked nodes ",
                "that revealed in the redistribution game within the days set in the sidebar. 4 (light green) is the number ",
                "of matching reveals per round the price oracle aims for: fewer raise the price, more (dark green) lower it ",
                "and spread the rewards thinner. A node with reserve doubling stores ",
                "several neighbourhoods and counts in each, placed with the sidebar radius; this matches the contract when ",
                "the radius is the storage radius nodes report. A staked node whose height is greater than the radius ",
                "cannot play at that radius and is left out. Point at a tile for its counts; click it to list its nodes."),
              p("Light and ultra-light nodes are not shown: swarmscan does not list them, and their place in the ",
                "network cannot be worked out from chain data."),
              layout_column_wrap(
                width = 1/2, fill = FALSE,
                checkboxInput("showUnstaked", "Show non-staking nodes (full nodes without stake, from swarmscan)", value = FALSE, width = "100%"),
                numericInput("earningsStake", "Stake for the earnings estimate (xBZZ)", value = 10, min = min_stake_bzz, step = 1, width = "100%")
              ),
              textOutput("nbhood_hover_text"),
              plotOutput("nbhoodMap", height = "640px", hover = hoverOpts("nbhoodHover", delay = 100, delayType = "throttle"),
                         click = "nbhoodClick"),
              br(),
              textOutput("nbhood_selected_text"),
              textOutput("nbhood_selected_details", container = p),
              textOutput("nbhood_earnings_explainer", container = p),
              DT::dataTableOutput("nbhood_nodes")),

    ###
    nav_panel("Price projection",
              div(class = "section-label", "Storage price projection"),
              p("A model, not a forecast. Each round the price oracle draws one neighbourhood; its staked nodes reveal, ",
                "and the number of matching reveals sets the change: fewer than 4 raise the price, more than 4 lower it, ",
                "and a round nobody claims counts as the largest rise. The model takes the staked nodes that revealed ",
                "within the days set in the sidebar, in every neighbourhood at the sidebar's radius, and assumes each of ",
                "them reveals a matching hash with probability q (participation, below). It then extends today's price at the ",
                "expected change per day. Participation is a calibrated parameter: the value that makes the model reproduce ",
                "the observed price change, not a measured reveal rate. The chain's own figure, matching reveals per claimed round, ",
                "is shown under the boxes. The 2-second option applies 2-second blocks from today, with no rescaling of the ",
                "price; GIP-153 plans the change for about December 2026."),
              layout_column_wrap(
                width = 1/4, fill = FALSE,
                sliderInput("participation", "Participation, calibrated (%)", min = 0, max = 100, value = 100, step = 1),
                numericInput("horizonDays", "Horizon (days)", value = 90, min = 1, max = 730, step = 1),
                radioButtons("blockSeconds", "Gnosis block time",
                             choices = c("Now, measured from the chain" = "now", "2 seconds from today (GIP-153), no repricing" = 2), selected = "now"),
                numericInput("extraNodes", "Extra staked nodes in each neighbourhood with fewer than 4",
                             value = 0, min = 0, max = 10, step = 1)
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "Price now", value = textOutput("price_now"), p("PLUR per chunk per block, from the price oracle")),
                value_box(title = "Model change", value = textOutput("price_model_change"), p("Per day, with the settings above")),
                value_box(title = "Observed change", value = textOutput("price_observed_change"), p("Per day, over the days set in the sidebar")),
                value_box(title = "Price at horizon", value = textOutput("price_at_horizon"), p("PLUR per chunk per block (projection)")),
                value_box(title = "1 GiB for 30 days", value = textOutput("price_gib_month"), p("BZZ, now and at the horizon; bare rent, without unused batch space")),
                value_box(title = "To hold the price flat", value = textOutput("price_balance"), textOutput("price_balance_note", container = p))
              ),
              textOutput("price_balance_text"),
              br(),
              textOutput("price_calibration"),
              actionButton("useFittedParticipation", "Set participation to the value that reproduces the observed change",
                           class = "btn-sm btn-outline-primary"),
              br(), br(),
              div(style = "display: flex; justify-content: space-between; gap: 1em;",
                  textOutput("pricePlot_hover", container = p),
                  actionLink("pricePlot_zoomout", "Zoom out", style = "white-space: nowrap;")),
              plotOutput("pricePlot", height = "520px",
                         brush = brushOpts("pricePlot_brush", direction = "x", resetOnNew = TRUE, fill = swarm_colours$orange, stroke = swarm_colours$orange),
                         dblclick = "pricePlot_dblclick", hover = hoverOpts("pricePlot_pointer", delay = 80, delayType = "throttle"))),

    ###
    nav_panel("Storage growth",
              div(class = "section-label", "Stored data and the storage radius"),
              p("Stored data is estimated from each node's reserve: the reserve within its radius times the number of ",
                "neighbourhoods, taken as the median over the nodes that report it. The history has one value a day from ",
                "swarmscan's archive of network dumps, plus today's. The fitted curves are projections of past growth, ",
                "not forecasts. When a bee node's reserve exceeds its capacity (2^22 chunks, or 2^(22+d) with reserve doubling d), it ",
                "evicts chunks outside its radius first and raises the radius only if that is not enough, that is when the chunks ",
                "within its radius alone exceed capacity. It lowers the radius when the chunks within its radius are below 50% of ",
                "capacity and pull-sync has stopped, checked every 15 minutes (bee pkg/storer/reserve.go, pkg/node/node.go). ",
                "Reserves fill almost evenly across neighbourhoods, so the network's capacity at the radius set in the sidebar is ",
                "where nodes split."),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                numericInput("fitDays", "Fit over the last (days)", value = 90, min = 7, max = 1500, step = 1),
                numericInput("growthHorizon", "Project ahead (days)", value = 180, min = 1, max = 1095, step = 1),
                numericInput("assumedGrowth", "Assumed growth (% a month; 0 = off)", value = 0, min = -50, max = 500, step = 1)
              ),
              textOutput("growth_summary", container = p),
              div(style = "display: flex; justify-content: space-between; gap: 1em;",
                  textOutput("growthPlot_hover", container = p),
                  actionLink("growthPlot_zoomout", "Zoom out", style = "white-space: nowrap;")),
              plotOutput("growthPlot", height = "560px",
                         brush = brushOpts("growthPlot_brush", direction = "x", resetOnNew = TRUE, fill = swarm_colours$orange, stroke = swarm_colours$orange),
                         dblclick = "growthPlot_dblclick", hover = hoverOpts("growthPlot_pointer", delay = 80, delayType = "throttle")),
              br(),
              div(class = "section-label", "Reserve fullness"),
              p("The chunks within each node's radius as a share of its reserve capacity (2^22 chunks, or 2^(22+d) with reserve ",
                "doubling d). A bee node raises its radius when this goes above 100%, and lowers it below 50% once pull-sync has ",
                "stopped. Each change of radius halves or doubles the share. Before nodes reported their reserve within radius ",
                "(March 2024), the line shows the whole reserve, which overstates the share."),
              div(style = "display: flex; justify-content: space-between; gap: 1em;",
                  textOutput("fullnessPlot_hover", container = p),
                  actionLink("fullnessPlot_zoomout", "Zoom out", style = "white-space: nowrap;")),
              plotOutput("fullnessPlot", height = "420px",
                         brush = brushOpts("fullnessPlot_brush", direction = "x", resetOnNew = TRUE, fill = swarm_colours$orange, stroke = swarm_colours$orange),
                         dblclick = "fullnessPlot_dblclick", hover = hoverOpts("fullnessPlot_pointer", delay = 80, delayType = "throttle"))),

    ###
    nav_panel("Connectivity",
              div(class = "section-label", "Browser light client capacity"),
              p("How many browser light clients the network can serve concurrently. Pages served over HTTPS can only dial full nodes that advertise a WSS underlay, ",
                "and each full node accepts up to --light-node-limit light peers (default 100)."),
              layout_column_wrap(
                width = 1/2, fill = FALSE,
                value_box(title = "WSS full nodes", value = textOutput("light_capable"), textOutput("light_capable_note", container = p)),
                value_box(title = "WSS full nodes × light-node-limit", value = textOutput("light_places"), textOutput("light_places_note", container = p))
              ),
              div(class = "section-label", "Client"),
              p("How many peers a client keeps, and whether it dials a fixed bootnode list first, depends on the client implementation. ",
                "Set the values for the client you want to check; the result appears once peers per client and concurrent clients are set."),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                numericInput("clientConnections", "Peers per client", value = NA, min = 1, max = 5000, step = 1),
                numericInput("expectedClients", "Concurrent clients", value = NA, min = 0, max = 1e6, step = 100),
                numericInput("otherLimit", "light-node-limit", value = 100, min = 1, max = 100000, step = 100)
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                numericInput("listNodes", "Bootnodes (0 = none)", value = 0, min = 0, max = 100000, step = 1),
                numericInput("startDials", "Bootnode peers per client", value = 0, min = 0, max = 5000, step = 1),
                numericInput("listLimit", "light-node-limit on bootnodes", value = 100, min = 1, max = 100000, step = 100)
              ),
              textOutput("light_input_warning", container = p),
              div(class = "section-label", "Result"),
              uiOutput("light_verdict"),
              layout_column_wrap(
                width = 1/2, fill = FALSE,
                value_box(title = "Max concurrent clients", value = textOutput("light_most"), textOutput("light_most_note", container = p)),
                value_box(title = "Load", value = uiOutput("light_load"), textOutput("light_load_note", container = p))
              ),
              textOutput("light_list_note", container = p),
              div(class = "section-label", "What it would take"),
              p("Each row changes one parameter so the concurrent clients fit, and says whether that change alone is sufficient."),
              tableOutput("light_takes"),
              textOutput("light_start_burst", container = p),
              div(class = "section-label", "Max concurrent clients by peers per client"),
              plotOutput("lightPlot", height = "400px"),
              div(class = "section-label", "Bootnode load while clients join"),
              p("A joining client dials bootnodes to find its first peers. A bootnode counts a client that is not a full node ",
                "against its light-node-limit for as long as the connection is open, as any full node does (bee's bootnode mode ",
                "changes only how it treats full peers). How many bootnodes a client dials and how long it keeps them ",
                "depends on the client; set them below."),
              layout_column_wrap(
                width = 1/2, fill = FALSE,
                radioButtons("bootnodeSource", "Bootnodes", width = "100%",
                             choices = c("bee's default, /dnsaddr/mainnet.ethswarm.org, resolved now" = "default", "Pasted multiaddresses" = "pasted")),
                checkboxInput("bootnodeWssOnly", "Only bootnodes with a WSS address (browser clients)", value = FALSE, width = "100%")
              ),
              conditionalPanel("input.bootnodeSource == 'pasted'",
                textAreaInput("bootnodeList", "Multiaddresses, one per line", rows = 4, width = "100%")),
              layout_column_wrap(
                width = 1/4, fill = FALSE,
                numericInput("joinsPerMinute", "Joining clients per minute", value = NA, min = 0, max = 1e6, step = 10),
                numericInput("bootnodeHoldSecs", "Seconds a client keeps each bootnode connection", value = NA, min = 0, max = 86400, step = 1),
                numericInput("bootnodesDialled", "Bootnodes each joining client dials", value = NA, min = 1, max = 10000, step = 1),
                numericInput("bootnodeLimit", "light-node-limit on bootnodes", value = 100, min = 1, max = 100000, step = 100)
              ),
              textOutput("bootnode_list_note", container = p),
              layout_column_wrap(
                width = 1/2, fill = FALSE,
                value_box(title = "Concurrent connections per bootnode", value = uiOutput("bootnode_concurrent"), textOutput("bootnode_concurrent_note", container = p)),
                value_box(title = "Max joining clients per minute", value = textOutput("bootnode_max_joins"), textOutput("bootnode_max_joins_note", container = p))
              ),
              tableOutput("bootnode_hosts"),
              textOutput("bootnode_lose_host", container = p),
              tags$details(
                tags$summary("Assumptions and sources"),
                tags$ul(
                  tags$li("bee: defaultLightNodeLimit = 100, --light-node-limit since 2.8.2. A peer with FullNode = false counts as a light peer; ",
                          "over the limit bee disconnects a random light peer (pkg/p2p/libp2p/libp2p.go)."),
                  tags$li(textOutput("light_transport_note", inline = TRUE)),
                  tags$li("Client peers are assumed to spread evenly over the WSS full nodes; bootnode peers evenly over the bootnodes, ",
                          "one per node. Bootnodes are counted among the WSS full nodes."),
                  tags$li("The maximum is an upper bound: it assumes no other light peers are already connected. What happens after nodes saturate is not modelled."),
                  tags$li("Bootnode load uses Little's law: concurrent connections per bootnode = joining clients per second × seconds each ",
                          "connection is kept × bootnodes each client dials ÷ bootnodes, with the dials spread evenly. bee's default bootnode ",
                          "address is resolved like libp2p does for /dnsaddr: TXT records dnsaddr=<multiaddress> on _dnsaddr.<name>, here ",
                          "fetched over DNS over HTTPS. A bootnode is a peer ID; one bootnode can have several addresses."),
                  tags$li("The sidebar settings do not apply to this tab.")
                )
              )),

    # ###
    # nav_panel("Nbhood counts",
    #           "Nodes per neighborhood, sorted by least numerous based on selected radius",
    #           DT::dataTableOutput("nbhood_counts")),
 
    ###
    nav_panel("Nbhoods stats",
              div(class = "section-label", "Neighbourhoods statistics"),
              p("A node counts as an error node when swarmscan reports an error contacting it, or an error ",
                "fetching its status (the Error and Status error columns on the Nodes info tab). Most status ",
                "errors are \"peer not found\", meaning swarmscan could not fetch the status, and can be benign."),
              DT::dataTableOutput("stats_table")),
    
    ###
    nav_panel("Nodes info",
              div(class = "section-label", "Individual nodes statistics"),
              DT::dataTableOutput("nodes_data"),
              br(),
              div(class = "section-label", "Staked nodes (Gnosis chain)"),
              p("Every overlay with stake in the staking contract, and its latest reveal in the redistribution game ",
                "within the days set in the sidebar (Active within, at most ", chain_window_days, "). Effective stake is what the game weighs: the committed stake at today's ",
                "price, capped at the deposit, and 0 while frozen. A node can play once its stake is at least 2 rounds ",
                "old and not frozen. The truth match is empty for a round that has not been claimed."),
              DT::dataTableOutput("stakes_table"))
  )


# server logic: data, plots, tables and text outputs
server <- function(input, output, session) {

  ###############
  # PREPARE THE DATA (reactive function)
  ###############
  # check every 5 seconds whether a read is due or has finished (R/background.R); this does not wait
  # for the read itself. Outputs recompute only when new data arrives.
  # while there is no data yet, every failed attempt also counts as a change, so the
  # "no data" message below always shows the latest error
  swarm_data_polled <- reactivePoll(5 * 1000, session,
                                    checkFunc = function() {
                                      version <- refresh_swarm_cache()
                                      if (is.null(swarm_cache$data)) paste(version, format(swarm_cache$last_attempt), swarm_cache$last_error) else version
                                    },
                                    valueFunc = function() swarm_cache$data)

  # the current data; until the first download succeeds, outputs show why there is no data
  swarm_data <- reactive({
    data <- swarm_data_polled()
    shiny::validate(shiny::need(!is.null(data),
                                no_data_message(swarm_cache, "swarmscan")))
    data
  })

  # when the first data arrives, set the radius to the one most nodes report, within the 1-16 the
  # app accepts; a radius the user already changed (while waiting for data) is left alone
  observeEvent(swarm_data_polled(), {
    radius <- typical_storage_radius(swarm_data_polled()$nodes)
    if (!is.null(radius) && isTRUE(input$storageRadius == 9)) {
      updateNumericInput(session, "storageRadius", value = min(max(radius, 1), 16))
    }
  }, once = TRUE)
  
  # data freshness, shown in the sidebar; re-read every minute so a failed refresh shows up
  output$data_status <- renderText({
    invalidateLater(60 * 1000)
    swarm_data_polled()
    data_status_text()
  })

  # chain data (stake, reveals, price), polled like the swarmscan data; outputs that use it
  # read chain_data_polled() and recompute when a new read arrives
  chain_data_polled <- reactivePoll(5 * 1000, session,
                                    checkFunc = function() {
                                      version <- refresh_chain_cache()
                                      if (is.null(chain_cache$data)) paste(version, format(chain_cache$last_attempt), chain_cache$last_error) else version
                                    },
                                    valueFunc = function() chain_cache$data)

  # the chain data for the views; while there is none (first read running, or failing) they show
  # why. A validation stop is silent in observers, so it cannot end the session
  chain_data <- reactive({
    chain <- chain_data_polled()
    shiny::validate(shiny::need(!is.null(chain), no_data_message(chain_cache, "the Gnosis chain")))
    chain
  })

  output$chain_status <- renderText({
    invalidateLater(60 * 1000)
    chain_data_polled()
    chain_status_text()
  })

  # the nodes after the "Show only full nodes" filter; tabs that show no neighbourhood (Map, Data,
  # Reachability) use this, so they do not depend on the storage radius, which they hide
  filtered_nodes_reactive <- reactive({
    nodes_data <- swarm_data()$nodes
    # set TRUE if an error string is found in the top-level or the status snapshot error field
    nodes_data$error_logical <- has_text(nodes_data$error) | has_text(nodes_data$statusSnapshot$error)

    # if user sets to only display full nodes, filter out the rest
    if (input$onlyFullNodes) {
      nodes_data <- nodes_data[!is.na(nodes_data$fullNode), ]
      nodes_data <- nodes_data[nodes_data$fullNode, ]
    }
    nodes_data
  })

  # based on the storage radius set, take the first n chars of the overlay address and add to the data
  nodes_data_reactive <- reactive({
    # the input's max = 16 is not enforced on typed values, and outputs build 2^radius
    # nbhood names, so only whole radii from 1 to 16 are accepted
    shiny::validate(shiny::need(isTRUE(input$storageRadius %in% 1:16), "Enter a storage radius from 1 to 16."))
    nodes_data <- filtered_nodes_reactive()
    nodes_data$overlay_short <- first_n_places(nodes_data$overlay_binary, input$storageRadius)
    nodes_data$overlay_short_next <- str_right( first_n_places(nodes_data$overlay_binary, (input$storageRadius + 1)), 1 )
    nodes_data
  })
  
  ###############
  # PREPARE THE PLOTS
  ###############
  
  # barchart nodes per neighbourhood
  output$distPlot <- renderPlot({
    # output the distribution by neighborhoods

    # all nodes count towards their nbhood, with or without a known location (same as the Nbhoods stats table)
    nodes_data <- nodes_data_reactive()
    # every nbhood is a level, so empty nbhoods keep their place on the x axis (as gaps)
    nodes_data$overlay_short <- factor(nodes_data$overlay_short, levels = nbhood_names(input$storageRadius))
    # average nodes per nbhood, counting empty nbhoods too
    average_per_nbhood <- nrow(nodes_data) / 2^input$storageRadius
    
    # generate plot
    plot <-
      ggplot(data = nodes_data, aes(x = overlay_short)) +
      geom_bar(fill = swarm_colours$bars) +
      geom_hline(yintercept = average_per_nbhood, colour = swarm_colours$mint, linetype = "dashed") +
      # yellow: nodes with an error in either error field (same rule as the Nbhoods stats table),
      # plus unreachable nodes, so the yellow visible above red is always reachable nodes with an error;
      # red: unreachable nodes, drawn on top. Both subsets come from the plotted data, so rows line up
      geom_bar(data = nodes_data[nodes_data$error_logical | nodes_data$unreachable %in% TRUE, ], fill = swarm_colours$error, width = 1) +
      geom_bar(data = nodes_data[nodes_data$unreachable %in% TRUE, ], fill = swarm_colours$unreachable, width = 1) +
      scale_x_discrete(drop = FALSE) +
      swarm_plot_theme +
      # more than 128 labels overlap into an unreadable band (and 65,536 at radius 16 slow the
      # drawing), so above that the labels are hidden and the axis title gives the count instead
      theme(axis.text.x = if (2^input$storageRadius <= 128) element_text(angle = -90, hjust = 0) else element_blank()) +
      labs(y = "Node count",
           x = if (2^input$storageRadius <= 128) "Neighbourhood" else
             paste0("Neighbourhood (", format_number(2^input$storageRadius), ", labels hidden; see Nbhoods stats)"))
    
    # return plot
    return(plot)
  })
  
  # how many shown nodes have a borrowed location, and how many are left off the map
  # the markers: the filtered nodes grouped by public IP address, at the network's radius (the radius
  # the nodes report; the sidebar radius does not apply to this tab)
  map_radius <- reactive({
    radius <- typical_storage_radius(swarm_data()$nodes)
    if (is.null(radius)) 9L else radius
  })
  map_markers_reactive <- reactive({
    chain <- chain_cache$data
    chain_data_polled()
    map_markers(filtered_nodes_reactive(), if (is.null(chain)) NULL else chain_stakes(chain), map_radius(), isTRUE(input$mapOnlyStaked))
  })
  output$map_note <- renderText({
    shown <- filtered_nodes_reactive()
    m <- map_markers_reactive()
    paste(
      sprintf("%s markers for %s nodes on %s public IP addresses; %s neighbourhood coverages, at radius %d.",
              format_number(nrow(m)), format_number(sum(m$nodes)), format_number(sum(!is.na(m$ip))),
              format_number(sum(m$coverage)), map_radius()),
      sprintf("Locations of %d nodes are taken from another node with the same public IP address. %d nodes have no known location and are not shown.",
              sum(shown$location_source %in% "same IP"), sum(is.na(shown$location$latitude))),
      if (is.null(chain_cache$data)) paste("No chain data yet, so every height is self-reported",
                                           if (isTRUE(input$mapOnlyStaked)) "and no node is known to be staked." else ".") else "")
  })

  # the marker last clicked (its public IP address, or the overlay of a node without one)
  map_selected <- reactiveVal(NULL)
  observeEvent(input$leafletMap_marker_click, map_selected(input$leafletMap_marker_click$id))
  map_selected_marker <- reactive({
    m <- map_markers_reactive()
    if (is.null(map_selected())) NULL else m[m$key == map_selected(), ]
  })
  output$map_selected <- renderText({
    marker <- map_selected_marker()
    if (is.null(marker) || nrow(marker) == 0) return("Click a marker to list the nodes behind it.")
    sprintf("%s%s: %s nodes, %s neighbourhood coverages, %s distinct neighbourhoods at radius %d.",
            if (is.na(marker$ip)) "A node without a public IP address" else mask_ip(marker$ip),
            if (nzchar(marker$place)) paste0(" (", marker$place, ")") else "",
            format_number(marker$nodes), format_number(marker$coverage), format_number(marker$distinct), map_radius())
  })
  output$map_nodes <- DT::renderDataTable({
    marker <- map_selected_marker()
    shiny::validate(shiny::need(!is.null(marker) && nrow(marker) == 1, ""))
    chain <- chain_cache$data
    marker_nodes_table(filtered_nodes_reactive(), if (is.null(chain)) NULL else chain_stakes(chain), map_radius(),
                       marker$key, isTRUE(input$mapOnlyStaked))
  }, container = header_with_tooltips(map_nodes_columns), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))

  # Leaflet map plot
  output$leafletMap <- renderLeaflet({
    # generate plot
    plot <- 
      leaflet() %>% 
      # Esri's dark grey base map; CARTO's dark tiles now need an API key
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ", options = tileOptions(maxZoom = 16))
    m <- map_markers_reactive()
    count <- m[[if (isTRUE(input$mapCountBy %in% map_count_choices)) input$mapCountBy else "nodes"]]
    # each marker carries its count (options.count) for the cluster label, and shows it in its
    # circle as a permanent label; the circle grows with the count and fits the number
    if (nrow(m) > 0) plot <- plot %>%
      addCircleMarkers(lat = m$lat, lng = m$lng, radius = pmin(10 + 2 * sqrt(count), 26), layerId = m$key,
                       label = format_number(count),
                       labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, className = "marker-count"),
                       options = c(pathOptions(), list(count = count)),
                       clusterOptions = markerClusterOptions(iconCreateFunction = map_cluster_icon),
                       stroke = FALSE, fillColor = swarm_colours$orange, fillOpacity = 0.9)
    
    # return plot
    return(plot)
  })
  
  ##############
  # PREPARE THE TABLES
  #############
  
  # table of counts per bins
  output$stats_table <- DT::renderDataTable({
    overlayAsFactor <-
      factor( x = nodes_data_reactive()$overlay_short,
              levels = nbhood_names(input$storageRadius) )

    # count nodes and nodes with errors per nbhood; subsetting a factor keeps all levels,
    # so both counts cover every nbhood even when no node has an error
    node_counts <- table(overlayAsFactor)
    error_counts <- table(overlayAsFactor[nodes_data_reactive()$error_logical])

    error_stats_per_nbhood <-
      data.frame(
        Freq = as.vector(node_counts),
        Freq.error = as.vector(error_counts),
        row.names = names(node_counts)
      )
    # an empty nbhood has no error percentage; NA shows as an empty cell and keeps the column numeric for sorting
    error_stats_per_nbhood$Percent.error <-
      ifelse(error_stats_per_nbhood$Freq > 0,
             (error_stats_per_nbhood$Freq.error / error_stats_per_nbhood$Freq)*100,
             NA)

    return(error_stats_per_nbhood)
    
  }, 
  colnames = c("Neighbourhoud", "Node count", "Error count", "Error percent") 
  )
  
  # table of all the data
  output$nodes_data <- DT::renderDataTable({
    shown <- nodes_data_reactive()
    # a field swarmscan leaves out entirely shows as an empty column instead of breaking the table.
    # [[ ]] matches names exactly: $ would partially match a missing "error" to "error_logical"
    column <- function(x) if (is.null(x)) rep(NA, nrow(shown)) else x

    # both error fields are shown, because the Nbhoods stats table counts a node with either one
    nodes_info <- data.frame(
      overlay_short = shown[["overlay_short"]],
      overlay_short_next = shown[["overlay_short_next"]],
      overlay = shown[["overlay"]],
      error = column(shown[["error"]]),
      status_error = column(shown[["statusSnapshot"]][["error"]]),
      unreachable = column(shown[["unreachable"]]),
      fullNode = column(shown[["fullNode"]]),
      # add info about country from location list
      location = column(shown[["location"]][["country"]]),
      # "swarmscan", or "same IP" when the location was borrowed from another node with that public IP
      location_source = column(shown[["location_source"]])
    )

    # return table with info
    return(nodes_info)
  },
  colnames = c("Neighbourhood", "Next bit", "Overlay", "Error", "Status error", "Unreachable", "Full node", "Location", "Location source"),
  rownames = FALSE 
  )
  
  ###############
  # NBHOOD MAP
  ###############
  # every node at the set radius, listed once per neighbourhood it counts in
  nbhood_members_reactive <- reactive({
    chain <- chain_data()
    shiny::validate(shiny::need(!is.null(chain),
                                no_data_message(chain_cache, "the Gnosis chain")))
    shiny::validate(shiny::need(isTRUE(input$storageRadius %in% 1:16), "Enter a storage radius from 1 to 16."))
    shiny::validate(shiny::need(isTRUE(input$activeDays > 0 && input$activeDays <= chain_window_days),
                                paste0("Enter an active period of at most ", chain_window_days, " days.")))
    nbhood_members(chain_stakes(chain), last_reveals(chain, input$activeDays), swarm_data_polled()$nodes, input$storageRadius)
  })
  nbhood_tiles_reactive <- reactive(nbhood_tiles(nbhood_members_reactive(), input$storageRadius, isTRUE(input$showUnstaked)))

  # the tile under a point of the plot; tiles sit on whole x and -y positions
  tile_at <- function(point) {
    if (is.null(point)) return(NULL)
    tiles <- nbhood_tiles_reactive()
    tile <- tiles[tiles$x == round(point$x) & tiles$y == round(-point$y), ]
    if (nrow(tile) == 1) tile else NULL
  }

  output$nbhoodMap <- renderPlot({
    tiles <- nbhood_tiles_reactive()
    radius <- input$storageRadius
    plot <- ggplot(tiles, aes(x = x, y = -y, fill = class)) +
      geom_tile(colour = swarm_colours$bg, linewidth = if (radius <= 10) 0.6 else 0) +
      scale_fill_manual(values = c("No node" = swarm_colours$line, "0" = swarm_colours$unreachable, "1" = swarm_colours$orange, "2" = swarm_colours$error,
                                   "3" = "#7aa6c2", "4" = swarm_colours$mint, "5 or more" = "#0a8a68"),
                        drop = FALSE,
                        name = if (isTRUE(input$showUnstaked)) "Staked and active, plus full\nnodes without stake" else "Staked and active") +
      coord_equal(expand = FALSE) +
      swarm_plot_theme +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
            panel.grid.major = element_blank(), legend.background = element_rect(fill = swarm_colours$bg),
            legend.text = element_text(colour = swarm_colours$text, family = "mono"),
            legend.title = element_text(colour = swarm_colours$muted, family = "mono"))
    # the count in each tile while the tiles are big enough to read it (up to 1,024 tiles)
    # dark text on the light tiles, light text on the dark green ones
    if (radius <= 10) plot <- plot + geom_text(aes(label = shown, colour = ifelse(class %in% c("5 or more", "No node"), swarm_colours$text, swarm_colours$bg)),
                                               family = "mono", size = if (radius <= 8) 4 else 3) + scale_colour_identity()
    plot
  # the map keeps square tiles, so it does not fill the whole image; the rest takes the page colour
  }, bg = swarm_colours$bg)

  output$nbhood_hover_text <- renderText({
    tile <- tile_at(input$nbhoodHover)
    if (is.null(tile)) "Point at a neighbourhood to see its counts." else nbhood_summary(tile, isTRUE(input$showUnstaked))
  })

  # the neighbourhood last clicked; cleared when the radius changes, because names change with it
  selected_nbhood <- reactiveVal(NULL)
  observeEvent(input$nbhoodClick, {
    tile <- tile_at(input$nbhoodClick)
    if (!is.null(tile)) selected_nbhood(tile$nbhood)
  })
  observeEvent(input$storageRadius, selected_nbhood(NULL))

  output$nbhood_selected_text <- renderText({
    if (is.null(selected_nbhood())) "Click a neighbourhood to list its nodes." else paste("Nodes in neighbourhood", selected_nbhood())
  })

  # for the clicked neighbourhood: when it was last drawn, what it got, and what a new node staking
  # the set amount could expect to earn there
  nbhood_details <- reactive({
    shiny::validate(shiny::need(!is.null(selected_nbhood()), ""))
    shiny::validate(shiny::need(isTRUE(input$earningsStake >= min_stake_bzz),
                                sprintf("Enter a stake of at least %s xBZZ, the Staking contract's minimum, for the earnings estimate.", format_number(min_stake_bzz))))
    chain <- chain_data()
    paid <- pot_per_day(chain, input$activeDays)
    others <- nbhood_stake_weight(nbhood_members_reactive(), selected_nbhood())
    list(chain = chain, paid = paid, others = others,
         drawn = last_drawn(chain$anchors, chain$truths, selected_nbhood()),
         history = nbhood_history(chain, selected_nbhood(), input$activeDays),
         earn = expected_earnings(paid, input$storageRadius, input$earningsStake, others))
  })

  # first paragraph: the facts and the estimate
  output$nbhood_selected_details <- renderText({
    d <- nbhood_details()
    drawn_text <- if (is.na(d$drawn$round)) {
      sprintf("Not drawn in the last %s days (rounds in which nobody revealed leave no record).", format_number(chain_window_days))
    } else {
      sprintf("Last drawn in round %s, %s UTC (%s).", format_number(d$drawn$round), format(d$drawn$time, "%Y-%m-%d %H:%M", tz = "UTC"),
              if (d$drawn$claimed) "claimed" else "not claimed")
    }
    paste(drawn_text,
      sprintf("Over the last %s days it was drawn in %s rounds, and their winners were paid %s xBZZ in total (the average per neighbourhood is %s xBZZ).",
              format_number(input$activeDays), format_number(d$history$rounds), format_number(round(d$history$paid, 1)),
              format_number(round(d$paid * input$activeDays / 2^input$storageRadius, 1))),
      sprintf("A new node staking %s xBZZ here could expect about %s xBZZ per 30 days (%s%% of its stake a year).",
              format_number(input$earningsStake), format_number(round(d$earn$per_30_days, 2)),
              format_number(round(100 * d$earn$per_30_days * 365 / 30 / input$earningsStake))))
  })

  # second paragraph: how the estimate is made
  output$nbhood_earnings_explainer <- renderText({
    d <- nbhood_details()
    sprintf(paste(
      "How the estimate is made: it uses the whole network's payouts, %s xBZZ a day over the last %s days, shared over %s neighbourhoods at radius %d, each drawn equally often in the long run.",
      "When this neighbourhood is drawn, the node would win %.1f%% of the time, against %s xBZZ of stake (weighted for reserve doubling) from the active staked nodes already here.",
      "It assumes all of them reveal a matching hash, and that payouts stay as they were."),
      format_number(round(d$paid)), format_number(input$activeDays), format_number(2^input$storageRadius), input$storageRadius,
      100 * d$earn$win_share, format_number(round(d$others, 1)))
  })

  output$nbhood_nodes <- DT::renderDataTable({
    shiny::validate(shiny::need(!is.null(selected_nbhood()), ""))
    members <- nbhood_members_reactive()
    shown <- members[members$nbhood %in% selected_nbhood(), ]
    if (!isTRUE(input$showUnstaked)) shown <- shown[shown$kind != node_kinds[["unstaked"]], ]
    shown <- shown[order(match(shown$kind, node_kinds), -shown$effective_stake), ]
    data.frame(overlay = shown$overlay, kind = shown$kind,
               stake = round(shown$stake, 2), effective_stake = round(shown$effective_stake, 2), height = shown$height,
               last_reveal = format(shown$last_reveal, "%Y-%m-%d %H:%M", tz = "UTC"), last_round = shown$last_round,
               matched_truth = shown$matched_truth, in_swarmscan = shown$in_swarmscan, reachable = shown$reachable,
               country = shown$country, user_agent = shown$user_agent)
  },
  container = header_with_tooltips(nbhood_nodes_columns),
  rownames = FALSE
  )

  ###############
  # PRICE PROJECTION
  ###############
  # active staked nodes per neighbourhood at the set radius, as on the Nbhood map
  price_active_counts <- reactive(nbhood_tiles(nbhood_members_reactive(), input$storageRadius, FALSE)$active)
  # the same with the what-if extra nodes in each neighbourhood with fewer than 4
  price_nbhood_counts <- reactive({
    shiny::validate(shiny::need(isTRUE(input$extraNodes >= 0), "Enter 0 or more extra nodes."))
    n <- price_active_counts()
    ifelse(n < 4, n + input$extraNodes, n)
  })
  # the change observed on chain over the sidebar's days, per day; it is measured in real time, so
  # it does not depend on the block time setting
  price_observed <- reactive({
    chain <- chain_data()
    observed_drift(chain$prices, chain$price, chain$head_time, input$activeDays)
  })
  # the participation that reproduces the observed change with the real counts (no extra nodes),
  # at the measured block time; NA if none does
  fitted_participation <- reactive({
    fit_participation(price_active_counts(), price_observed(), chain_data()$oracle, price_block_seconds_now())
  })
  # the participation starts at the fitted value: set once when the data first allows a fit,
  # unless the slider was already moved from 100%
  observeEvent(fitted_participation(), {
    fitted <- fitted_participation()
    if (!is.na(fitted) && isTRUE(input$participation == 100)) updateSliderInput(session, "participation", value = round(100 * fitted))
  }, once = TRUE)
  observeEvent(input$useFittedParticipation, {
    fitted <- fitted_participation()
    if (!is.na(fitted)) updateSliderInput(session, "participation", value = round(100 * fitted))
  })

  # today's block time, measured from the reveals read; 5 seconds, Gnosis's target, until enough are read
  price_block_seconds_now <- reactive({
    measured <- measured_block_seconds(chain_data())
    if (is.na(measured)) 5 else measured
  })
  price_settings <- reactive({
    shiny::validate(shiny::need(isTRUE(input$horizonDays > 0), "Enter a horizon of 1 day or more."))
    shiny::validate(shiny::need(isTRUE(input$participation >= 0 && input$participation <= 100), "Set a participation from 0 to 100%."))
    list(q = input$participation / 100, block_seconds = if (identical(input$blockSeconds, "2")) 2 else price_block_seconds_now(), days = input$horizonDays)
  })
  price_model <- reactive({
    chain <- chain_data()
    settings <- price_settings()
    # while the price oracle is paused, adjustPrice changes nothing, so the price stays where it is
    drift <- if (isTRUE(chain$oracle$paused)) 0 else model_drift(price_nbhood_counts(), settings$q, chain$oracle, settings$block_seconds)
    list(chain = chain, drift = drift, observed = price_observed(), settings = settings,
         projection = project_price(chain$price, chain$head_time, drift, settings$days, chain$oracle$minimum_price))
  })

  output$price_now <- renderText(format_number(price_model()$chain$price))
  output$price_model_change <- renderText(sprintf("%+.2f%%", drift_percent(price_model()$drift)))
  output$price_observed_change <- renderText({
    observed <- price_model()$observed
    if (is.na(observed)) "no data" else sprintf("%+.2f%%", drift_percent(observed))
  })
  output$price_at_horizon <- renderText(format_number(round(tail(price_model()$projection$price, 1))))
  output$price_gib_month <- renderText({
    model <- price_model()
    paste(format_number(gib_month_bzz(model$chain$price, model$settings$block_seconds)), "->",
          format_number(gib_month_bzz(tail(model$projection$price, 1), model$settings$block_seconds)))
  })

  # how many active staked nodes would hold the price flat, with today's nodes (no extra nodes) at
  # the participation set
  price_balance_plan <- reactive(balance_plan(price_active_counts(), price_settings()$q, chain_data()$oracle))
  output$price_balance <- renderText({
    if (isTRUE(chain_data()$oracle$paused)) return("–")
    plan <- price_balance_plan()
    if (is.na(plan$add)) "out of reach" else if (plan$add > 0) paste0("+", format_number(plan$add)) else
      if (plan$leave > 0) paste0("-", format_number(plan$leave)) else "0"
  })
  output$price_balance_note <- renderText({
    if (isTRUE(chain_data()$oracle$paused)) return("the price oracle is paused, so the price does not move")
    plan <- price_balance_plan()
    if (is.na(plan$add)) "no number of nodes stops the rise at this participation" else
      if (plan$add > 0) "active staked nodes to add, at the participation above" else
        if (plan$leave > 0) "active staked nodes that could leave before the price stops falling" else "the price is flat already"
  })
  output$price_balance_text <- renderText({
    if (isTRUE(chain_data()$oracle$paused)) return("")
    plan <- price_balance_plan()
    settings <- price_settings()
    n <- price_active_counts()
    day <- function(per_round) sprintf("%+.2f%%", drift_percent(rounds_per_day(settings$block_seconds) * per_round))
    paste0(
      sprintf("The price is flat when a neighbourhood has about 4 matching reveals per round, which at %.0f%% participation takes about %.2f active staked nodes per neighbourhood, against %.2f today. ",
              100 * settings$q, 4 / max(settings$q, 0.01), mean(n)),
      "Where nodes sit hardly matters for the price: each matching reveal moves it by almost the same step, from 0 up to 8 reveals, so only the total counts (above 8 per neighbourhood, extra reveals are ignored). ",
      if (day(plan$even) == day(plan$now)) {
        sprintf("Spreading today's nodes evenly would take %s moves and leave the change at %s a day. ", format_number(plan$moves), day(plan$now))
      } else {
        sprintf("Spreading today's nodes evenly would take %s moves and change the price by %s a day instead of %s. ",
                format_number(plan$moves), day(plan$even), day(plan$now))
      },
      "Placement matters for keeping data safe instead: put new nodes in the empty and thin neighbourhoods on the Nbhood map first.")
  })

  # how well the model fits: the participation that reproduces the observed change, and what the
  # chain shows directly about the same days
  output$price_calibration <- renderText({
    chain <- chain_data()
    fitted <- fitted_participation()
    rounds <- round_stats(chain, input$activeDays, price_block_seconds_now())
    measured <- measured_block_seconds(chain)
    fit_text <- if (isTRUE(chain$oracle$paused)) "" else if (is.na(fitted)) {
      "No participation reproduces the observed change with these neighbourhood counts."
    } else {
      sprintf("A participation of %.0f%% reproduces the observed change.", 100 * fitted)
    }
    block_text <- if (is.na(measured)) "Not enough blocks read to measure the block time; 5 seconds is assumed." else
      sprintf("Blocks took %.2f seconds on average over the window, %s rounds a day.", measured, format_number(round(rounds_per_day(measured), 1)))
    paste(fit_text, block_text, sprintf(
      "On chain over the same %s days: %s rounds, %s of them claimed (%.1f%% not claimed), with %.2f matching reveals per claimed round on average, against %.2f active staked nodes per neighbourhood.",
      format_number(input$activeDays), format_number(rounds$rounds), format_number(rounds$claimed),
      100 * (1 - rounds$claimed / max(rounds$rounds, 1)), rounds$mean_matching, mean(price_active_counts())),
      if (is.na(rounds$truth_depth) || !isTRUE(input$storageRadius %in% 1:16)) "" else if (rounds$truth_depth == input$storageRadius)
        sprintf("The claimed truths were at depth %d, the radius set in the sidebar.", rounds$truth_depth) else
        sprintf("Warning: the claimed truths were at depth %d, but the sidebar radius is %d. The model counts nodes per neighbourhood at the sidebar radius, so set it to %d for these counts to match the game.",
                rounds$truth_depth, input$storageRadius, rounds$truth_depth),
      if (isTRUE(chain$oracle$paused)) "The price oracle is paused, so the price does not change at all, and the projection holds it flat." else "")
  })

  # zoom state of each plot against time: NULL for the whole plot, or the dragged period
  plot_zoom <- function(name) {
    zoom <- reactiveVal(NULL)
    observeEvent(input[[paste0(name, "_brush")]], { b <- input[[paste0(name, "_brush")]]; zoom(c(b$xmin, b$xmax)) })
    observeEvent(input[[paste0(name, "_dblclick")]], zoom(NULL))
    observeEvent(input[[paste0(name, "_zoomout")]], zoom(NULL))
    zoom
  }
  price_zoom <- plot_zoom("pricePlot")
  growth_zoom <- plot_zoom("growthPlot")
  fullness_zoom <- plot_zoom("fullnessPlot")

  # the price plot's series, as x and y
  price_series <- reactive({
    model <- price_model()
    series <- list(history = data.frame(x = model$chain$prices$time, y = model$chain$prices$price),
                   projection = data.frame(x = model$projection$time, y = model$projection$price))
    if (isTRUE(input$extraNodes > 0)) {
      plain <- project_price(model$chain$price, model$chain$head_time,
                             if (isTRUE(model$chain$oracle$paused)) 0 else
                               model_drift(price_active_counts(), model$settings$q, model$chain$oracle, model$settings$block_seconds),
                             model$settings$days, model$chain$oracle$minimum_price)
      series$plain <- data.frame(x = plain$time, y = plain$price)
    }
    series
  })
  output$pricePlot_hover <- renderText({
    at <- input$pricePlot_pointer$x
    if (is.null(at)) return(time_plot_hint)
    series <- price_series()
    parts <- c(format(as.POSIXct(at, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M UTC"))
    on_chain <- value_at(series$history, at, step = TRUE, max_gap = 0)
    if (!is.na(on_chain) && at <= as.numeric(price_model()$chain$head_time)) parts <- c(parts, paste("price", format_number(on_chain), "PLUR"))
    projected <- value_at(series$projection, at, max_gap = 0)
    if (!is.na(projected)) parts <- c(parts, paste("projection", format_number(round(projected)), "PLUR"))
    if (!is.null(series$plain)) {
      plain <- value_at(series$plain, at, max_gap = 0)
      if (!is.na(plain)) parts <- c(parts, paste("without the extra nodes", format_number(round(plain)), "PLUR"))
    }
    paste(parts, collapse = " | ")
  })

  output$pricePlot <- renderPlot({
    model <- price_model()
    history <- model$chain$prices
    plot <- ggplot() +
      geom_step(data = history, aes(x = time, y = price), colour = swarm_colours$text, linewidth = 1) +
      geom_line(data = model$projection, aes(x = time, y = price), colour = swarm_colours$orange, linetype = "dashed", linewidth = 1.2) +
      geom_vline(xintercept = model$chain$head_time, colour = swarm_colours$muted, linetype = "dotted")
    plain <- price_series()$plain
    if (!is.null(plain)) {
      # the same projection without the extra nodes, for comparison
      plot <- plot + geom_line(data = plain, aes(x = x, y = y), colour = swarm_colours$muted, linetype = "dashed", linewidth = 1)
    }
    plot + scale_y_continuous(labels = function(x) format_number(x)) +
      swarm_plot_theme +
      labs(x = NULL, y = "PLUR per chunk per block",
           caption = paste("White: price updates on chain. Orange, dashed: projection.",
                           if (isTRUE(input$extraNodes > 0)) "Grey, dashed: projection without the extra nodes." else "")) +
      swarm_readable_text +
      zoom_coord(zoom_limits(price_zoom(), price_series()), "datetime")
  }, bg = swarm_colours$bg)

  ###############
  # STORAGE GROWTH
  ###############
  # the saved history plus today's value from the current swarmscan data
  # days with too few reporting nodes are left out
  growth_history <- reactive({
    today <- summarise_dump(swarm_data()$nodes, as.Date(current_time()))
    history <- rbind(storage_history_data[storage_history_data$date != today$date, names(today)], today)
    history <- history[history$nodes_reporting >= min_reporting_nodes, ]
    history[order(history$date), ]
  })
  growth_fit <- reactive({
    shiny::validate(shiny::need(isTRUE(input$fitDays >= 7), "Fit over 7 days or more."))
    shiny::validate(shiny::need(isTRUE(input$growthHorizon >= 1), "Project at least 1 day ahead."))
    shiny::validate(shiny::need(isTRUE(input$storageRadius %in% 1:16), "Enter a storage radius from 1 to 16."))
    history <- growth_history()
    shiny::validate(shiny::need(nrow(history) > 0, "No stored-data history yet, and the current swarmscan data has too few nodes reporting their reserve."))
    fit <- fit_growth(history, input$fitDays)
    radius <- input$storageRadius
    lines <- data.frame(level = c(capacity_tib(radius), capacity_tib(radius + 1), capacity_tib(radius) / 2),
                        label = c(sprintf("radius rises to %d", radius + 1), sprintf("radius rises to %d", radius + 2),
                                  sprintf("radius falls to %d", radius - 1)))
    list(history = history, fit = fit, lines = lines, radius = radius)
  })

  output$growth_summary <- renderText({
    g <- growth_fit()
    now <- tail(g$history, 1)
    head_text <- sprintf("Stored on %s: %s TiB, with the median reserve %.0f%% full. Capacity at radius %d: %s TiB.",
                         format(now$date, "%Y-%m-%d"), format_number(round(now$stored_tib, 2)), 100 * now$fullness_median,
                         g$radius, format_number(capacity_tib(g$radius)))
    if (is.null(g$fit)) return(paste(head_text, "There is not enough history in the fit window to fit a curve."))
    # a line the stored data has already passed has no crossing ahead
    passed <- function(i) if (i == 3) now$stored_tib < g$lines$level[i] else now$stored_tib >= g$lines$level[i]
    crossing_text <- function(i, date) {
      if (passed(i)) return(sprintf("%s: already %s", g$lines$label[i], if (i == 3) "below" else "above"))
      sprintf("%s on %s", g$lines$label[i], if (is.na(date)) "no date (not reached)" else format(date, "%Y-%m-%d"))
    }
    describe <- function(kind, rate_text) {
      crossings <- vapply(seq_len(nrow(g$lines)), function(i) crossing_text(i, crossing_date(g$fit, g$lines$level[i], kind,
                                                                                                        if (i == 3) "down" else "up")), "")
      paste0(rate_text, ": ", paste(crossings, collapse = "; "), ".")
    }
    slope <- stats::coef(g$fit$linear)[2]
    growth <- 100 * (exp(stats::coef(g$fit$exponential)[2]) - 1)
    assumed_text <- if (isTRUE(input$assumedGrowth <= -100)) "The assumed growth must be above -100% a month." else
      if (isTRUE(input$assumedGrowth != 0)) {
        crossings <- vapply(seq_len(nrow(g$lines)), function(i)
          crossing_text(i, assumed_crossing(now$date, now$stored_tib, input$assumedGrowth, g$lines$level[i])), "")
        paste0(sprintf("Assumed %+g%% a month from %s: ", input$assumedGrowth, format(now$date, "%Y-%m-%d")), paste(crossings, collapse = "; "), ".")
      } else ""
    paste(head_text, sprintf("Projection from a fit to %s to %s.", format(g$fit$from, "%Y-%m-%d"), format(g$fit$to, "%Y-%m-%d")),
          describe("linear", sprintf("Straight line, %+.3f TiB a day", slope)),
          describe("exponential", sprintf("Exponential, %+.2f%% a day", growth)), assumed_text)
  })

  # the growth plot's series, as x and y
  growth_series <- reactive({
    g <- growth_fit()
    series <- list(stored = data.frame(x = g$history$date, y = g$history$stored_tib, radius = g$history$radius_mode,
                                       measure = g$history$measure))
    if (!is.null(g$fit)) {
      projection <- project_growth(g$fit, input$growthHorizon)
      for (kind in unique(projection$fit)) series[[kind]] <- data.frame(x = projection$date[projection$fit == kind],
                                                                       y = projection$stored_tib[projection$fit == kind])
    }
    if (isTRUE(input$assumedGrowth != 0 && input$assumedGrowth > -100)) {
      now <- tail(g$history, 1)
      assumed <- assumed_growth(now$date, now$stored_tib, input$assumedGrowth, input$growthHorizon)
      series$Assumed <- data.frame(x = assumed$date, y = assumed$stored_tib)
    }
    series
  })
  output$growthPlot_hover <- renderText({
    at <- input$growthPlot_pointer$x
    if (is.null(at)) return(time_plot_hint)
    series <- growth_series()
    parts <- format(as.Date(round(at), origin = "1970-01-01"), "%Y-%m-%d")
    stored <- series$stored
    near <- which.min(abs(as.numeric(stored$x) - at))
    if (length(near) == 1 && abs(as.numeric(stored$x[near]) - at) <= 1) {
      parts <- c(parts, sprintf("stored %s TiB at radius %s%s", format_number(round(stored$y[near], 2)), stored$radius[near],
                                if (stored$measure[near] == "whole reserve") " (older measure)" else ""))
    }
    for (kind in intersect(c("Straight line", "Exponential", "Assumed"), names(series))) {
      value <- value_at(series[[kind]], at, max_gap = 0)
      label <- if (kind == "Assumed") sprintf("assumed %+g%% a month", input$assumedGrowth) else tolower(kind)
      if (!is.na(value)) parts <- c(parts, sprintf("%s %s TiB", label, format_number(round(value, 2))))
    }
    paste(parts, collapse = " | ")
  })

  output$growthPlot <- renderPlot({
    g <- growth_fit()
    plot <- ggplot() +
      geom_hline(data = g$lines, aes(yintercept = level), colour = swarm_colours$muted, linetype = "dotted", linewidth = 0.8) +
      geom_text(data = g$lines, aes(x = if (is.null(growth_zoom())) min(g$history$date) else as.Date(growth_zoom()[1], origin = "1970-01-01"),
                                    y = level, label = label), colour = swarm_colours$text,
                family = "mono", fontface = "bold", size = 5, hjust = 0, vjust = -0.5) +
      geom_line(data = g$history[g$history$measure == "within radius", ], aes(x = date, y = stored_tib, colour = "Stored data"), linewidth = 1) +
      geom_line(data = g$history[g$history$measure == "whole reserve", ], aes(x = date, y = stored_tib, colour = "Stored data, older measure"),
                linewidth = 1, linetype = "dotdash")
    if (!is.null(g$fit)) {
      plot <- plot + geom_line(data = project_growth(g$fit, input$growthHorizon), aes(x = date, y = stored_tib, colour = fit),
                               linetype = "dashed", linewidth = 1.1)
    }
    assumed_label <- sprintf("Assumed %+g%% a month", input$assumedGrowth)
    assumed <- growth_series()$Assumed
    if (!is.null(assumed)) {
      plot <- plot + geom_line(data = assumed, aes(x = x, y = y, colour = assumed_label), linetype = "longdash", linewidth = 1.1)
    }
    plot + scale_colour_manual(values = setNames(c(swarm_colours$text, swarm_colours$muted, swarm_colours$orange, swarm_colours$mint, "#7aa6c2"),
                                                 c("Stored data", "Stored data, older measure", "Straight line", "Exponential", assumed_label))) +
      scale_y_continuous(labels = function(x) format_number(x)) +
      swarm_plot_theme + swarm_readable_text +
      labs(x = NULL, y = "Stored data (TiB)",
           caption = paste("Dashed: fits of the last days set above, extended to the horizon (projections); long dashes: the assumed growth. Dotted: radius changes.",
                           "Grey: days before nodes reported their reserve within radius; their whole reserve overstates the stored data.",
                           sep = "\n")) +
      zoom_coord(zoom_limits(growth_zoom(), growth_series()), "date")
  }, bg = swarm_colours$bg)

  output$fullnessPlot_hover <- renderText({
    at <- input$fullnessPlot_pointer$x
    if (is.null(at)) return(time_plot_hint)
    history <- growth_fit()$history
    near <- which.min(abs(as.numeric(history$date) - at))
    if (length(near) == 0 || abs(as.numeric(history$date[near]) - at) > 1) return(format(as.Date(round(at), origin = "1970-01-01"), "%Y-%m-%d"))
    sprintf("%s | median %.0f%% full | 90th percentile %.0f%% full | radius %s", format(history$date[near], "%Y-%m-%d"),
            100 * history$fullness_median[near], 100 * history$fullness_p90[near], history$radius_mode[near])
  })

  output$fullnessPlot <- renderPlot({
    history <- growth_fit()$history
    long <- rbind(data.frame(date = history$date, share = 100 * history$fullness_median, series = "Median"),
                  data.frame(date = history$date, share = 100 * history$fullness_p90, series = "90th percentile"))
    ggplot(long, aes(x = date, y = share, colour = series)) +
      geom_hline(yintercept = c(50, 100), colour = swarm_colours$muted, linetype = "dotted", linewidth = 0.8) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c("Median" = swarm_colours$text, "90th percentile" = swarm_colours$orange)) +
      swarm_plot_theme + swarm_readable_text +
      labs(x = NULL, y = "Reserve full (%)") +
      zoom_coord(zoom_limits(fullness_zoom(), list(data.frame(x = long$date, y = long$share))), "date")
  }, bg = swarm_colours$bg)

  ###############
  # CONNECTIVITY
  ###############
  # facts about the network, shown whatever the client
  light_network <- reactive({
    nodes <- swarm_data()$nodes
    full <- nodes$fullNode %in% TRUE
    list(capable = browser_capable_nodes(nodes), full = sum(full), plain_tcp = sum(full & nodes$plain_tcp %in% TRUE),
         both = sum(full & nodes$plain_tcp %in% TRUE & nodes$secure_websocket %in% TRUE),
         other_address = sum(full & nodes$other_address %in% TRUE))
  })
  output$light_capable <- renderText(format_number(light_network()$capable))
  output$light_capable_note <- renderText(paste0("Full nodes with a /tls/.../ws underlay",
    if (is.null(wss_probe_latest)) "" else sprintf("; %.1f%% accepted a browser-style WebSocket connection when probed on %s",
                                                   100 * wss_probe_latest$accepted / wss_probe_latest$probed, substr(wss_probe_latest$date, 1, 10))))
  output$light_places <- renderText(format_number(light_network()$capable * max(input$otherLimit, 0, na.rm = TRUE)))
  output$light_places_note <- renderText(sprintf("%s × %s light peers", format_number(light_network()$capable),
                                                 format_number(max(input$otherLimit, 0, na.rm = TRUE))))
  output$light_transport_note <- renderText({
    n <- light_network()
    sprintf("Browsers cannot dial plain TCP, and pages served over HTTPS can only open wss connections. In swarmscan's current data, of %s full nodes, %s advertise a plain TCP underlay, %s a WSS underlay (%s both), and %s another kind. %s",
            format_number(n$full), format_number(n$plain_tcp), format_number(n$capable), format_number(n$both), format_number(n$other_address),
            wss_probe_text(wss_probe_latest))
  })

  # bootnode load while clients join
  bootnode_addresses <- reactive({
    if (identical(input$bootnodeSource, "pasted")) return(parse_multiaddrs(input$bootnodeList))
    # resolved only while the tab is open (hidden outputs are not computed); refreshed every 6 hours,
    # or tried again sooner after a failed lookup
    addresses <- refresh_bootnode_cache()
    invalidateLater(1000 * if (is.null(bootnode_cache$last_error)) bootnode_refresh_secs else retry_with_data_secs)
    addresses
  })
  bootnode_model <- reactive({
    table <- bootnode_table(bootnode_addresses())
    shiny::validate(shiny::need(nrow(table) > 0, if (identical(input$bootnodeSource, "pasted")) "Paste one or more multiaddresses." else
      paste0("bee's default bootnodes could not be resolved", if (!is.null(bootnode_cache$last_error)) paste0(" (", bootnode_cache$last_error, ")") else "",
             ". Trying again every few minutes.")))
    if (isTRUE(input$bootnodeWssOnly)) table <- table[table$wss, ]
    shiny::validate(shiny::need(nrow(table) > 0, "None of these bootnodes has a WSS address."))
    set <- isTRUE(input$joinsPerMinute >= 0) && isTRUE(input$bootnodeHoldSecs >= 0) && isTRUE(input$bootnodesDialled >= 1) &&
      isTRUE(input$bootnodeLimit >= 1)
    list(table = table, set = set,
         hosts = if (set) bootnode_hosts(table, input$joinsPerMinute, input$bootnodeHoldSecs, input$bootnodesDialled, input$bootnodeLimit) else NULL)
  })
  output$bootnode_list_note <- renderText({
    m <- bootnode_model()
    peers <- length(unique(m$table$peer)); hosts <- length(unique(m$table$host[!is.na(m$table$host)]))
    source <- if (identical(input$bootnodeSource, "pasted")) "pasted" else
      sprintf("resolved from %s at %s UTC", bee_default_bootnode, format(bootnode_cache$fetched_at, "%Y-%m-%d %H:%M", tz = "UTC"))
    sprintf("%s bootnodes (peer IDs) on %s IP addresses or hosts, with %s addresses, %s.", format_number(peers), format_number(hosts),
            format_number(nrow(m$table)), source)
  })
  output$bootnode_concurrent <- renderUI({
    m <- bootnode_model()
    if (!m$set) return("–")
    load <- m$hosts$load
    span(style = sprintf("color: %s;", if (load$load > 1) swarm_colours$unreachable else if (load$load > 0.8) swarm_colours$error else swarm_colours$mint),
         format_number(round(load$concurrent)))
  })
  output$bootnode_concurrent_note <- renderText({
    m <- bootnode_model()
    if (!m$set) return("Set joining clients per minute, seconds each connection is kept and bootnodes dialled.")
    load <- m$hosts$load
    sprintf("%s joins/s × %s s × %s bootnodes dialled ÷ %s bootnodes; %s%% of the light-node-limit of %s",
            format_number(round(input$joinsPerMinute / 60, 2)), format_number(input$bootnodeHoldSecs), format_number(load$per_client),
            format_number(m$hosts$bootnodes), format_number(round(100 * load$load)), format_number(input$bootnodeLimit))
  })
  output$bootnode_max_joins <- renderText({
    m <- bootnode_model()
    if (!m$set || is.na(m$hosts$load$max_joins_per_min)) "–" else format_number(floor(m$hosts$load$max_joins_per_min))
  })
  output$bootnode_max_joins_note <- renderText({
    m <- bootnode_model()
    if (!m$set) return("")
    "Joining clients per minute at which each bootnode reaches its light-node-limit"
  })
  output$bootnode_hosts <- renderTable({
    m <- bootnode_model()
    shiny::req(m$set)
    h <- m$hosts$hosts
    data.frame(`IP address or host` = h$host, Bootnodes = format_number(h$bootnodes),
               `Concurrent connections` = format_number(round(h$concurrent)),
               `Load` = sprintf("%s%%", format_number(round(100 * h$concurrent / (h$bootnodes * input$bootnodeLimit)))),
               check.names = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%")
  output$bootnode_lose_host <- renderText({
    m <- bootnode_model()
    if (!m$set || nrow(m$hosts$hosts) < 2) return("")
    # each bootnode is counted on one host, so with two or more hosts some bootnodes are left
    without <- m$hosts$without_busiest
    sprintf("If %s (%s of the %s bootnodes) is lost, the other bootnodes each take about %s concurrent connections (%s%% of the light-node-limit), and the maximum is about %s joining clients per minute.",
            m$hosts$busiest_host, format_number(m$hosts$hosts$bootnodes[1]), format_number(m$hosts$bootnodes),
            format_number(round(without$concurrent)), format_number(round(100 * without$load)), format_number(floor(without$max_joins_per_min)))
  })

  # the bootnodes' light-node-limit follows the light-node-limit until the reader changes it
  observeEvent(input$otherLimit, {
    if (!isTRUE(input$otherLimit >= 1)) return()
    if (isTRUE(input$listLimit == light_last_limit())) updateNumericInput(session, "listLimit", value = input$otherLimit)
    light_last_limit(input$otherLimit)
  }, ignoreInit = TRUE)
  light_last_limit <- reactiveVal(100)

  # the client's numbers are entered; until then, no result is shown
  light_inputs_set <- reactive(isTRUE(input$clientConnections >= 1) && isTRUE(input$expectedClients >= 0))
  output$light_input_warning <- renderText({
    n <- light_network()
    warnings <- c(
      if (isTRUE(input$listNodes > n$capable)) sprintf("More bootnodes than WSS full nodes; counted as %s.", format_number(n$capable)),
      if (isTRUE(input$listNodes > 0) && isTRUE(input$startDials == 0)) "Bootnodes set but 0 bootnode peers per client: ignored.",
      if (isTRUE(input$startDials > input$clientConnections)) "Bootnode peers per client exceed peers per client: capped.",
      if (isTRUE(input$listNodes > 0) && isTRUE(input$startDials > input$listNodes)) "Bootnode peers per client exceed the bootnodes: capped at one per bootnode.")
    paste(warnings, collapse = " ")
  })
  light_model <- reactive({
    shiny::validate(shiny::need(light_inputs_set(), "Set peers per client and concurrent clients."))
    shiny::validate(shiny::need(isTRUE(input$otherLimit >= 1 && input$listLimit >= 1), "light-node-limit must be 1 or more."))
    shiny::validate(shiny::need(isTRUE(input$listNodes >= 0 && input$startDials >= 0), "Bootnodes and bootnode peers must be 0 or more."))
    capable <- light_network()$capable
    list_used <- input$listNodes > 0 && input$startDials > 0
    list_nodes <- if (list_used) min(input$listNodes, capable) else 0
    start <- if (list_used) input$startDials else 0
    other <- max(0, capable - list_nodes)
    cap <- client_capacity(other, input$otherLimit, input$clientConnections, list_nodes, input$listLimit, start, input$expectedClients)
    c(list(capable = capable, other_nodes = other, list_nodes = list_nodes, start = start, list_used = list_used), cap)
  })
  light_state <- function(load) if (!is.finite(load) || load > 1) "over" else if (load > 0.8) "close" else "fits"
  light_state_colour <- c(over = swarm_colours$unreachable, close = swarm_colours$error, fits = swarm_colours$mint)

  output$light_verdict <- renderUI({
    if (!light_inputs_set()) {
      return(div(style = sprintf("border-left: 6px solid %s; background: %s; padding: 0.8em 1em; margin: 0.5em 0 1em;", swarm_colours$line, swarm_colours$surface),
                 "Set peers per client and concurrent clients."))
    }
    m <- light_model()
    clients <- input$expectedClients
    bound <- if (!m$list_used) "" else if (m$bound_by == "list") ", bootnodes saturate first" else ", non-bootnode peers saturate first"
    text <- switch(light_state(m$load),
      over = if (m$most == 0) "Over capacity: there are no WSS full nodes to connect to." else
        sprintf("Over capacity: %s concurrent clients, but the light-node limits allow at most %s (%s× over)%s. When a new light peer takes a node over its limit, bee disconnects a random light peer.",
                format_number(clients), format_number(m$most), format_number(round(m$load, 1)), bound),
      close = sprintf("Near capacity: %s concurrent clients use %.0f%% of the maximum of %s%s.", format_number(clients), 100 * m$load, format_number(m$most), bound),
      fits = sprintf("Within capacity: %s concurrent clients use %.0f%% of the maximum of %s%s.", format_number(clients), 100 * m$load, format_number(m$most), bound))
    div(style = sprintf("border-left: 6px solid %s; background: %s; padding: 0.8em 1em; margin: 0.5em 0 1em; font-size: 1.15rem; font-weight: bold;",
                        light_state_colour[[light_state(m$load)]], swarm_colours$surface), text)
  })
  output$light_most <- renderText(if (light_inputs_set()) format_number(light_model()$most) else "–")
  output$light_most_note <- renderText({
    if (!light_inputs_set()) return("")
    m <- light_model()
    if (!m$list_used) return(sprintf("%s light peers ÷ %s peers per client; an upper bound", format_number(m$other_nodes * input$otherLimit), format_number(m$elsewhere)))
    if (m$bound_by == "list") sprintf("Limited by the bootnodes: %s light peer connections ÷ %s bootnode peers per client", format_number(m$list_nodes * input$listLimit), format_number(m$on_list)) else
      sprintf("Limited by the other WSS full nodes: %s light peer connections ÷ %s peers per client", format_number(m$other_nodes * input$otherLimit), format_number(m$elsewhere))
  })
  output$light_load <- renderUI({
    if (!light_inputs_set()) return("–")
    load <- light_model()$load
    span(style = sprintf("color: %s;", light_state_colour[[light_state(load)]]),
         if (!is.finite(load)) "no room" else sprintf("%s%%", format_number(round(100 * load))))
  })
  output$light_load_note <- renderText({
    if (!light_inputs_set()) return("")
    sprintf("%s concurrent clients ÷ maximum of %s", format_number(input$expectedClients), format_number(light_model()$most))
  })
  output$light_list_note <- renderText({
    if (!light_inputs_set()) return("")
    m <- light_model()
    if (!m$list_used) return("")
    on_list <- sprintf("Each client keeps %s peers on the %s bootnodes, which allow at most %s clients", format_number(m$on_list),
                       format_number(m$list_nodes), format_number(m$list_clients))
    if (m$elsewhere == 0) return(paste0(on_list, ", and no peers on other nodes."))
    sprintf("%s, and %s peers on the other %s WSS full nodes, which allow at most %s.", on_list,
            format_number(m$elsewhere), format_number(m$other_nodes), format_number(m$other_clients))
  })

  output$light_takes <- renderTable({
    shiny::req(light_inputs_set())
    m <- light_model()
    t <- what_it_takes(m$capable, input$otherLimit, input$clientConnections, input$expectedClients, m$list_nodes, input$listLimit, m$start)
    labels <- c(peers = "Peers per client", limit = if (m$list_used) "light-node-limit (non-bootnodes)" else "light-node-limit",
                nodes = if (m$list_used) "WSS full nodes (non-bootnodes)" else "WSS full nodes",
                bootnode_peers = "Bootnode peers per client", bootnode_limit = "light-node-limit on bootnodes",
                bootnodes = "Bootnodes", other = "Other WSS full nodes", list = "Bootnodes", network = "All WSS full nodes")
    current <- c(peers = input$clientConnections, limit = input$otherLimit, nodes = if (m$list_used) m$other_nodes else m$capable,
                 bootnode_peers = m$on_list, bootnode_limit = input$listLimit, bootnodes = m$list_nodes)
    needed <- vapply(seq_len(nrow(t)), function(i) {
      key <- t$change[i]
      if (t$status[i] == "already enough") return(sprintf("none needed (they allow %s clients)", format_number(t$value[i])))
      if (key == "nodes" && t$status[i] == "not possible") return("no WSS full nodes")
      if (key == "bootnodes" && t$status[i] == "not possible")
        return(sprintf("%s (only %s WSS full nodes)", format_number(t$value[i]), format_number(m$capable)))
      if (is.na(t$value[i]) || t$status[i] == "not possible") return("–")
      sprintf("%s → %s", format_number(current[[key]]), format_number(t$value[i]))
    }, "")
    data.frame(Parameter = unname(labels[t$change]), Change = needed,
               Result = c("enough" = "sufficient", "not enough on its own" = "not sufficient on its own", "already enough" = "already sufficient",
                          "not possible" = "not possible")[t$status],
               check.names = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%")
  output$light_start_burst <- renderText({
    if (!light_inputs_set()) return("")
    m <- light_model()
    if (!m$list_used) return("")
    sprintf("If all %2$s clients start at the same time, each bootnode gets about %1$s dial attempts, against a light-node-limit of %3$s.",
            format_number(round(start_attempts_per_node(input$expectedClients, m$list_nodes, m$on_list))), format_number(input$expectedClients),
            format_number(input$listLimit))
  })

  output$lightPlot <- renderPlot({
    n <- light_network()
    set <- light_inputs_set()
    m <- if (set) light_model() else NULL
    list_nodes <- if (set) m$list_nodes else 0
    start <- if (set) m$start else 0
    limit <- max(input$otherLimit, 1, na.rm = TRUE)
    top <- max(300, if (set) input$clientConnections * 1.5 else 0)
    curve <- data.frame(connections = unique(round(seq(max(1, start + 1), top, length.out = 300))))
    curve$clients <- vapply(curve$connections, function(k)
      client_capacity(max(0, n$capable - list_nodes), limit, k, list_nodes, max(input$listLimit, 1, na.rm = TRUE), start)$most, numeric(1))
    plot <- ggplot(curve, aes(x = connections, y = clients)) + geom_line(colour = swarm_colours$mint, linewidth = 1.2)
    if (set) {
      now <- data.frame(connections = input$clientConnections, clients = m$most)
      plot <- plot +
        geom_hline(yintercept = input$expectedClients, colour = swarm_colours$orange, linetype = "dashed", linewidth = 0.9) +
        annotate("text", x = max(curve$connections), y = input$expectedClients, label = sprintf("%s concurrent clients", format_number(input$expectedClients)),
                 colour = swarm_colours$orange, family = "mono", fontface = "bold", size = 5, hjust = 1, vjust = -0.6) +
        geom_point(data = now, colour = swarm_colours$text, size = 4) +
        geom_text(data = now, aes(label = sprintf("%s peers → %s clients", format_number(connections), format_number(clients))),
                  colour = swarm_colours$text, family = "mono", fontface = "bold", size = 5, vjust = -0.8,
                  hjust = if (input$clientConnections > top / 2) 1.05 else -0.05)
    }
    ymax <- if (set) max(input$expectedClients, m$most, 1) * 2.2 else max(curve$clients[curve$connections >= 20], 1) * 1.1
    plot + scale_y_continuous(labels = function(x) format_number(x)) +
      coord_cartesian(ylim = c(0, ymax)) +
      swarm_plot_theme + swarm_readable_text +
      labs(x = "Peers per client", y = "Max concurrent clients") +
      theme(axis.title.x = element_text(colour = swarm_colours$text, family = "mono", face = "bold", size = 16))
  }, bg = swarm_colours$bg)

  # table of staked overlays from the chain, with each one's latest reveal in the window
  output$stakes_table <- DT::renderDataTable({
    chain <- chain_data()
    shiny::validate(shiny::need(!is.null(chain),
                                no_data_message(chain_cache, "the Gnosis chain")))
    shiny::validate(shiny::need(isTRUE(input$storageRadius %in% 1:16), "Enter a storage radius from 1 to 16."))
    shiny::validate(shiny::need(isTRUE(input$activeDays > 0 && input$activeDays <= chain_window_days),
                                paste0("Enter an active period of at most ", chain_window_days, " days.")))
    stakes <- chain_stakes(chain)
    latest <- last_reveals(chain, input$activeDays)
    reveal <- latest[match(stakes$overlay, latest$overlay), ]
    # the swarmscan dump, when there is one, says whether swarmscan sees the node at all
    dump_overlays <- swarm_data_polled()$nodes$overlay

    stakes_info <- data.frame(
      nbhood = first_n_places(overlay_to_bits(stakes$overlay), input$storageRadius),
      overlay = stakes$overlay,
      stake = round(stakes$stake_bzz, 2),
      effective_stake = round(stakes$effective_stake_bzz, 2),
      height = stakes$height,
      frozen = stakes$frozen,
      can_play = stakes$can_play,
      last_reveal = format(reveal$time, "%Y-%m-%d %H:%M", tz = "UTC"),
      last_round = reveal$round,
      matched_truth = reveal$matched_truth,
      in_swarmscan = if (is.null(dump_overlays)) NA else stakes$overlay %in% dump_overlays
    )
    stakes_info[order(stakes_info$nbhood, -stakes_info$effective_stake), ]
  },
  # column headers with an explanation shown when the pointer rests on them (the title attribute)
  container = stakes_table_header(),
  rownames = FALSE
  )

  # table of reachability
  # swarmscan's own flag (it sets unreachable when it could not reach the node, and fullNode only
  # on nodes it reached) next to what each node reports about itself
  output$reachability_status <- DT::renderDataTable({
    nodes <- filtered_nodes_reactive()
    yes_no <- function(x) ifelse(is.na(x), "unknown", ifelse(x, "yes", "no"))
    reached <- if (is.null(nodes$unreachable)) rep(NA, nrow(nodes)) else !(nodes$unreachable %in% TRUE)
    # a snapshot that failed (its error is set) has every field zeroed, so isReachable = false there
    # is not the node's answer
    self <- nodes[["statusSnapshot"]][["isReachable"]]
    if (is.null(self)) self <- rep(NA, nrow(nodes))
    self[has_text(nodes[["statusSnapshot"]][["error"]])] <- NA
    counts <- as.data.frame(table(full = yes_no(nodes$fullNode), reached = yes_no(reached), self = yes_no(self)),
                            stringsAsFactors = FALSE)
    counts <- counts[counts$Freq > 0, ]
    counts[order(-counts$Freq), ]
  },
  colnames = c("Full node", "Reached by swarmscan", "Reachable (self-reported)", "Count"),
  rownames = FALSE
  )
  
  
  # # table of node counts per nbhood
  # output$nbhood_counts <- DT::renderDataTable({
  #   browser()
  #   overlayAsFactor <- 
  #     factor( x = nodes_data_reactive()$overlay_short,
  #             levels = generate_short_overlay(radius = input$storageRadius) )
  #   return(data.frame(sort(table(overlayAsFactor, useNA = "always")) ))
  # }, colnames = c("Neighbourhood", "Count"), rownames = FALSE)
  
  # reactive function finding max storage capacity
  max_capacity_radius <- reactive({
    # an empty or zero minimum would break the comparison or never end the loop
    # (shiny:: because jsonlite also has a validate function)
    shiny::validate(shiny::need(isTRUE(input$minNodesPerNbhood >= 1), "Enter a minimum of 1 or more nodes per nbhood."))
    # radius 0 is a single nbhood holding every node; if even that is too small, no radius works
    shiny::validate(shiny::need(nrow(filtered_nodes_reactive()) >= input$minNodesPerNbhood,
                  "There are fewer nodes than the minimum per nbhood, so no radius has enough nodes."))

    # node count of the smallest nbhood at radius r; there are 2^r nbhoods, so if fewer
    # of them appear in the data, at least one is empty and the smallest count is 0
    smallest_nbhood_count <- function(r) {
      counts <- table(first_n_places(filtered_nodes_reactive()$overlay_binary, r))
      if (length(counts) < 2^r) 0 else min(counts)
    }

    # largest radius at which every nbhood has at least the required minimum number of nodes
    radius <- 0
    while (radius < 256 && smallest_nbhood_count(radius + 1) >= input$minNodesPerNbhood) {
      radius <- radius + 1
    }

    # return storage radius with required minimum number of nodes
    return(storageRadius = radius)
  })
  
  # the network's radius as the nodes report it, as a cross-check for the computed maximum
  output$reported_radius <- renderText({
    radius <- typical_storage_radius(swarm_data()$nodes)
    if (is.null(radius)) "–" else format_number(radius)
  })

  # return max radius
  output$max_radius <- renderText({
    format_number(max_capacity_radius())
  })
  
  
  # return max capacity as text
  output$max_capacity <- renderText({
    max_capacity <- 2 ^ 22 * 4096 * (2 ^ max_capacity_radius()) / (1024 * 1024 * 1024 * 1024)
    paste(format_number(max_capacity), "TiB")
  })
   
  
  #############
  # PREPARE TEXT OUTPUT
  #############
  
  # info on total and unreachable nodes
  output$nodes_count <- renderText({
    # swarmscan's own counts; if it leaves them out, count the nodes table instead
    nodes <- swarm_data()$nodes
    total <- swarm_data()$counts$count
    unreachable <- swarm_data()$counts$unreachableCount
    if (is.null(total)) total <- nrow(nodes)
    if (is.null(unreachable)) unreachable <- sum(nodes[["unreachable"]] %in% TRUE)
    sprintf("Total nodes: %s. Unreachable nodes: %s.", format_number(total), format_number(unreachable))
  })
  
  output$explainer_text_1 <- renderText({
    paste("Set the neighbourhood size with Storage radius in the sidebar. Grey : all nodes in neighbourhood; yellow : nodes reporting an error, including errors swarmscan got when fetching the node's status (could be benign); red : unreachable nodes, drawn over yellow (could be benign). Dashed line: average nodes per neighbourhood, counting empty ones. A neighbourhood without nodes shows as a gap.")
    
  })
  
  ##############
  # Calculate amount of storage on Swarm
  ##############
  output$storage_taken <- renderText({
    stored <- estimate_stored_tib(swarm_data()$nodes)
    shiny::validate(shiny::need(!is.na(stored), "No node reports its reserve size, so the stored data cannot be estimated."))
    paste(format_number(stored), "TiB")
  })
}

########################################
# Run the application 
shinyApp(ui = ui, server = server)
