### how many browser tabs running weeb-3 the network can hold at once
# Verified facts the model is built on (bee and weeb-3 sources, read 2026-10-07):
# - bee: each full node takes up to light-node-limit light clients (defaultLightNodeLimit = 100,
#   --light-node-limit since bee 2.8.2). A peer that says it is not a full node counts as a light
#   client; over the limit bee disconnects a random light client (pkg/p2p/libp2p/libp2p.go).
# - weeb-3 says it is not a full node in the handshake (src/handlers.rs, full_node: false).
# - weeb-3 aims for 200 connections per browser (CONNECTION_BUILDUP_LIMIT, src/accounting.rs) and lowers
#   that target by a fifth for every 100 connections it loses, down to 30.
# - weeb-3 starts each browser on a random 160 (INITIAL_BOOTNODE_BURST) of a hard-coded list of 319
#   nodes (MAINNET_BOOTNODES, src/network_profile.rs, commit 243eff5). A page can pass its own list
#   instead (bootstrapNodes in the start options, src/library.rs).
# - weeb-3 runs in a SharedWorker: all its tabs of one site in one browser share one node and one set
#   of connections (README.md, "Connected tabs share that node's peer identity, connections ..."),
#   so the unit here is a browser, not a tab.
# - weeb-3 does not close idle connections (idle timeout 36,000,000 s, src/lib.rs). It closes
#   connections that fail (dial, handshake, ping, no pricing within 20 s), duplicates, and some after
#   a failed payment refresh; when a node drops it, it dials the same node again after a few seconds
#   (src/lib.rs, from reading the code). So start-up nodes stay in use and the start-up list fills first.
# - A page's own bootstrapNodes list is used in order, first 160 entries (src/library.rs,
#   src/worker_runtime.rs): the page has to shuffle it for each browser to spread the load.
# What happens once nodes are full (bee drops clients, weeb-3 dials the same nodes again and lowers
# its target) has not been measured; the model stops at the point where they fill.

weeb3_connections <- 200
weeb3_start_dials <- 160
weeb3_start_list <- 319
# of the 319 built-in nodes, those swarmscan listed as browser-capable full nodes on 2026-10-07
# (matched by peer ID); they are part of the browser-capable count, so the rest of the network is
# the browser-capable nodes less these
weeb3_list_in_data <- 303

# the number of browser-capable full nodes: full nodes that advertise a secure WebSocket address
# (/tls/.../ws), which browsers need. Whether each of them really accepts browsers is not tested
browser_capable_nodes <- function(nodes) sum(nodes[["fullNode"]] %in% TRUE & nodes[["secure_websocket"]] %in% TRUE)

# how many tabs fit before nodes fill. Each tab puts min(start_dials, list_nodes) connections on
# the start-up list and the rest of its `connections` on the other browser-capable nodes, assumed
# spread evenly over them. The answer is the smaller of the two limits
tab_capacity <- function(list_nodes, list_limit, other_nodes, other_limit, connections = weeb3_connections,
                         start_dials = weeb3_start_dials, tabs = 0) {
  on_list <- min(start_dials, list_nodes, connections)
  elsewhere <- max(0, connections - on_list)
  list_tabs <- if (on_list > 0) floor(list_nodes * list_limit / on_list) else Inf
  other_tabs <- if (elsewhere > 0) floor(other_nodes * other_limit / elsewhere) else Inf
  most <- min(list_tabs, other_tabs)
  list(on_list = on_list, elsewhere = elsewhere, list_tabs = list_tabs, other_tabs = other_tabs, most = most,
       load = if (is.finite(most) && most > 0) tabs / most else if (tabs > 0) Inf else 0,
       bound_by = if (list_tabs <= other_tabs) "list" else "network")
}

# what would let `tabs` browsers fit, each change on its own, and whether it is enough once the rest
# of the network is counted too: list_nodes and list_limit only change the start-up list; fewer
# start-up connections moves the rest of each browser's connections to the rest of the network
what_it_takes <- function(list_nodes, list_limit, other_nodes, other_limit, tabs,
                          connections = weeb3_connections, start_dials = weeb3_start_dials) {
  on_list <- min(start_dials, list_nodes)
  more_nodes <- if (tabs * on_list <= list_nodes * list_limit) list_nodes else max(start_dials + 1, ceiling(tabs * start_dials / list_limit))
  more_places <- ceiling(tabs * on_list / list_nodes)
  fewer_dials <- if (tabs > 0) floor(list_nodes * list_limit / tabs) else NA_real_
  fits <- function(n, l, d) tab_capacity(n, l, other_nodes, other_limit, connections, d)$most >= tabs
  list(list_nodes = more_nodes, list_limit = more_places, start_dials = fewer_dials,
       list_nodes_enough = fits(more_nodes, list_limit, start_dials),
       list_limit_enough = fits(list_nodes, more_places, start_dials),
       start_dials_enough = !is.na(fewer_dials) && fewer_dials >= 1 && fits(list_nodes, list_limit, fewer_dials),
       start_dials_network = tab_capacity(list_nodes, list_limit, other_nodes, other_limit, connections, max(fewer_dials, 1))$other_tabs)
}

# start-up burst: if `tabs` browsers start together, each listed node gets this many connection attempts
start_attempts_per_node <- function(tabs, list_nodes, start_dials = weeb3_start_dials) tabs * min(start_dials, list_nodes) / list_nodes
