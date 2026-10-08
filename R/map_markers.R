### the Map tab: one marker per public IP address, counted by nodes, machines or neighbourhoods covered
# Nodes on one machine share a public IP address and so a location. Grouping them before drawing
# gives one marker per IP; its popup lists the nodes behind it (issue #66). A node with reserve
# doubling (height d) stores 2^d neighbourhoods, so counting 2^d per node shows where the stored
# data sits, not only where the nodes are (issue #65)

map_count_choices <- c("Nodes" = "nodes", "Machines (public IP addresses)" = "machines",
                       "Neighbourhoods covered" = "coverage")
map_popup_lines <- 10   # nodes listed in one popup; the rest are summed up

# the public IP address used to group a node: its first public IPv4 address, else its first public
# IPv6 address, else NA
group_ip <- function(ips) {
  if (length(ips) == 0) return(NA_character_)
  v4 <- ips[!grepl(":", ips, fixed = TRUE)]
  if (length(v4)) v4[1] else ips[1]
}

# an IP address with the part that identifies the host hidden: the last octet of an IPv4 address,
# the last 64 bits (the interface part) of an IPv6 address. Grouping uses the full address
mask_ip <- function(ip) {
  vapply(ip, function(a) {
    if (is.na(a)) return(NA_character_)
    if (!grepl(":", a, fixed = TRUE)) return(sub("\\.[0-9]+$", ".x", a))
    halves <- strsplit(a, "::", fixed = TRUE)[[1]]
    groups <- function(s) if (length(s) == 0 || is.na(s) || !nzchar(s)) character(0) else strsplit(s, ":", fixed = TRUE)[[1]]
    head <- groups(halves[1]); tail <- if (length(halves) > 1) groups(halves[2]) else character(0)
    full <- c(head, rep("0", max(8 - length(head) - length(tail), 0)), tail)
    paste0(paste(full[1:4], collapse = ":"), ":x:x:x:x")
  }, "", USE.NAMES = FALSE)
}

# each node's reserve doubling: the staked height from the chain where the node is staked, else
# committedDepth - storageRadius from its status snapshot (self-reported), else 0
node_heights <- function(nodes, stakes = NULL) {
  status <- nodes[["statusSnapshot"]]
  committed <- status[["committedDepth"]]; radius <- status[["storageRadius"]]
  reported <- if (is.null(committed) || is.null(radius)) rep(NA_real_, nrow(nodes)) else
    ifelse(!is.na(committed) & !is.na(radius) & committed > 0 & radius > 0, pmax(committed - radius, 0), NA_real_)
  staked <- if (is.null(stakes)) rep(NA_integer_, nrow(nodes)) else match(nodes$overlay, stakes$overlay)
  data.frame(height = ifelse(!is.na(staked), stakes$height[staked], ifelse(is.na(reported), 0, reported)),
             source = ifelse(!is.na(staked), "chain", ifelse(is.na(reported), "none", "self-reported")),
             stake = if (is.null(stakes)) NA_real_ else stakes$stake_bzz[staked])
}

# the neighbourhoods at `radius` one node covers: its first radius - height bits, followed by every
# combination of the remaining bits (the Nbhood map's rule)
covered_nbhoods <- function(bits, height, radius) {
  shared <- max(radius - height, 0)
  rest <- radius - shared
  paste0(substr(bits, 1, shared), if (rest > 0) nbhood_names(rest) else "")
}

# one row per marker. nodes: the prepared nodes table (public_ip, location, overlay_binary);
# stakes: chain_stakes() or NULL; radius: the network's radius; only_staked: keep staked nodes only.
# Nodes without a public IP address keep one marker each and count as one machine each
map_markers <- function(nodes, stakes, radius, only_staked = FALSE) {
  heights <- node_heights(nodes, stakes)
  keep <- !is.na(nodes$location$latitude) & (!only_staked | heights$source == "chain")
  nodes <- nodes[keep, ]; heights <- heights[keep, ]
  empty <- data.frame(lat = numeric(0), lng = numeric(0), ip = character(0), nodes = numeric(0), machines = numeric(0),
                      coverage = numeric(0), distinct = numeric(0), popup = character(0))
  if (nrow(nodes) == 0) return(empty)
  key <- ifelse(is.na(nodes$public_ip), paste0("overlay:", nodes$overlay), nodes$public_ip)
  rows <- split(seq_len(nrow(nodes)), factor(key, levels = unique(key)))
  bits <- nodes$overlay_binary
  markers <- lapply(rows, function(i) {
    covered <- unique(unlist(lapply(i, function(k) covered_nbhoods(bits[k], heights$height[k], radius))))
    list(lat = nodes$location$latitude[i[1]], lng = nodes$location$longitude[i[1]], ip = nodes$public_ip[i[1]],
         nodes = length(i), machines = 1, coverage = sum(2^heights$height[i]), distinct = length(covered),
         popup = marker_popup(nodes[i, ], heights[i, ], radius, length(covered)))
  })
  out <- do.call(rbind, lapply(markers, as.data.frame, stringsAsFactors = FALSE))
  rownames(out) <- NULL
  out
}

# the popup of one marker: the masked IP address, the counts, then one line per node. All text from
# swarmscan is escaped, because the dump is third-party input
marker_popup <- function(nodes, heights, radius, distinct) {
  esc <- htmltools::htmlEscape
  ip <- nodes$public_ip[1]
  place <- paste(stats::na.omit(c(nodes$location$city[1], nodes$location$country[1])), collapse = ", ")
  yes_no <- function(x) ifelse(is.na(x), "unknown", ifelse(x, "yes", "no"))
  # a field the dump leaves out reads as NA
  field <- function(name) if (is.null(nodes[[name]])) rep(NA, nrow(nodes)) else nodes[[name]]
  agent <- field("userAgent"); full <- field("fullNode"); unreachable <- field("unreachable")
  lines <- vapply(seq_len(min(nrow(nodes), map_popup_lines)), function(k) {
    stake <- if (heights$source[k] == "chain") sprintf("staked %s xBZZ, height %d", format_number(round(heights$stake[k], 1)), heights$height[k]) else
      if (heights$source[k] == "self-reported") sprintf("not staked, height %d (self-reported)", heights$height[k]) else "not staked"
    sprintf("<code>%s</code> %s; full node: %s; reached: %s; %s; nbhood %s; location: %s",
            esc(substr(nodes$overlay[k], 1, 8)), esc(ifelse(is.na(agent[k]), "no user agent", agent[k])),
            yes_no(full[k]), yes_no(!(unreachable[k] %in% TRUE)), stake,
            substr(nodes$overlay_binary[k], 1, radius), esc(nodes$location_source[k]))
  }, "")
  more <- if (nrow(nodes) > map_popup_lines) sprintf("<br>and %d more", nrow(nodes) - map_popup_lines) else ""
  paste0("<b>", if (is.na(ip)) "No public IP address" else esc(mask_ip(ip)), "</b>", if (nzchar(place)) paste0(" (", esc(place), ")") else "",
         sprintf("<br>%d nodes; %s neighbourhood coverages, %s distinct neighbourhoods at radius %d", nrow(nodes),
                 format_number(sum(2^heights$height)), format_number(distinct), radius),
         "<br>", paste(lines, collapse = "<br>"), more)
}

# the cluster icon: the sum of the chosen count over the markers in the cluster (each marker carries
# it as options.count), instead of the number of markers
map_cluster_icon <- htmlwidgets::JS("function(cluster) {
  var n = 0;
  cluster.getAllChildMarkers().forEach(function(m) { n += m.options.count || 0; });
  var size = n < 10 ? 'small' : n < 100 ? 'medium' : 'large';
  return new L.DivIcon({ html: '<div><span>' + n.toLocaleString('en-US') + '</span></div>',
                         className: 'marker-cluster marker-cluster-' + size, iconSize: new L.Point(40, 40) });
}")
