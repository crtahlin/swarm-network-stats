### the Map tab: one marker per public IP address, counted by nodes, machines or neighbourhoods covered
# Nodes on one machine share a public IP address and so a location. Grouping them before drawing
# gives one marker per IP; clicking it lists the nodes behind it under the map (issue #66). A node with reserve
# doubling (height d) stores 2^d neighbourhoods, so counting 2^d per node shows where the stored
# data sits, not only where the nodes are (issue #65)

map_count_choices <- c("Nodes" = "nodes", "Machines (public IP addresses)" = "machines",
                       "Neighbourhoods covered" = "coverage")

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

# the marker each node belongs to: its public IP address, or its own overlay without one
marker_key <- function(nodes) ifelse(is.na(nodes$public_ip), paste0("overlay:", nodes$overlay), nodes$public_ip)

# the located nodes the map shows, with their heights. only_staked: keep staked nodes only
map_nodes <- function(nodes, stakes, only_staked = FALSE) {
  heights <- node_heights(nodes, stakes)
  keep <- !is.na(nodes$location$latitude) & (!only_staked | heights$source == "chain")
  list(nodes = nodes[keep, ], heights = heights[keep, ])
}

# one row per marker. nodes: the prepared nodes table (public_ip, location, overlay_binary);
# stakes: chain_stakes() or NULL; radius: the network's radius; only_staked: keep staked nodes only.
# Nodes without a public IP address keep one marker each and count as one machine each
map_markers <- function(nodes, stakes, radius, only_staked = FALSE) {
  shown <- map_nodes(nodes, stakes, only_staked)
  nodes <- shown$nodes; heights <- shown$heights
  empty <- data.frame(key = character(0), lat = numeric(0), lng = numeric(0), ip = character(0), place = character(0),
                      nodes = numeric(0), machines = numeric(0), coverage = numeric(0), distinct = numeric(0))
  if (nrow(nodes) == 0) return(empty)
  key <- marker_key(nodes)
  rows <- split(seq_len(nrow(nodes)), factor(key, levels = unique(key)))
  bits <- nodes$overlay_binary
  out <- do.call(rbind, lapply(names(rows), function(k) {
    i <- rows[[k]]
    covered <- unique(unlist(lapply(i, function(n) covered_nbhoods(bits[n], heights$height[n], radius))))
    data.frame(key = k, lat = nodes$location$latitude[i[1]], lng = nodes$location$longitude[i[1]], ip = nodes$public_ip[i[1]],
               place = paste(stats::na.omit(c(nodes$location$city[i[1]], nodes$location$country[i[1]])), collapse = ", "),
               # a height above the radius covers every neighbourhood once: capped, so one bogus
               # self-reported height cannot inflate the counts
               nodes = length(i), machines = 1, coverage = sum(2^pmin(heights$height[i], radius)), distinct = length(covered),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# the nodes behind one marker, as the table under the map shows them
marker_nodes_table <- function(nodes, stakes, radius, key, only_staked = FALSE) {
  shown <- map_nodes(nodes, stakes, only_staked)
  mine <- marker_key(shown$nodes) == key
  nodes <- shown$nodes[mine, ]; heights <- shown$heights[mine, ]
  # a field the dump leaves out reads as NA
  field <- function(name) if (is.null(nodes[[name]])) rep(NA, nrow(nodes)) else nodes[[name]]
  yes_no <- function(x) ifelse(is.na(x), "unknown", ifelse(x, "yes", "no"))
  data.frame(overlay = nodes$overlay, user_agent = field("userAgent"), full_node = yes_no(field("fullNode")),
             # swarmscan sets unreachable only on nodes it could not reach, so NA means reached
             reached = yes_no(!(field("unreachable") %in% TRUE)), stake = round(heights$stake, 2),
             height = heights$height, height_from = heights$source, nbhood = substr(nodes$overlay_binary, 1, radius),
             location_source = field("location_source"), stringsAsFactors = FALSE)
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
