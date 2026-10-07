### how many browser (ultra-light) clients the network's full nodes can hold at once
# A client holds one place on each full node it connects to. Each full node takes up to
# light-node-limit light clients (bee defaultLightNodeLimit = 100, --light-node-limit since bee
# 2.8.2), and browser (ultra-light) clients count against the same limit. bee enforces the limit
# on each node: when it is exceeded, the node disconnects a random light client to make room
# (pkg/p2p/libp2p, KickedOutPeersCount), so overload shows as clients being dropped and
# reconnecting, not as refused connections.

# ways to decide which full nodes count. Browser clients connect over secure WebSockets, so only
# full nodes offering a /tls/.../ws address can take them. The other two rules are for native
# (bee) light nodes. Every full node in swarmscan's dump has been reached by swarmscan (it learns
# that a node is full from the handshake). Many do not report reachability, because swarmscan
# could not fetch their status, so "reports itself reachable" undercounts
light_node_rules <- c("offers a secure WebSocket address (browsers need this)" = "websocket",
                      "was reached by swarmscan (native light nodes, not browsers)" = "reached",
                      "reports itself reachable (native light nodes, not browsers)" = "reachable")

# the number of full nodes that count under a rule
accepting_full_nodes <- function(nodes, rule = c("websocket", "reached", "reachable")) {
  full <- nodes[["fullNode"]] %in% TRUE
  accepting <- switch(match.arg(rule),
    websocket = full & nodes[["secure_websocket"]] %in% TRUE,
    reached = full & !(nodes[["unreachable"]] %in% TRUE),
    reachable = full & nodes[["statusSnapshot"]][["isReachable"]] %in% TRUE)
  sum(accepting)
}

# places, the most clients at once, and demand against capacity.
# accepting nodes take `limit` clients each. Edge nodes (shown as "our extra nodes": full nodes we
# would run only for browser clients, with a raised light-node limit) take `edge_limit` each, but only if
# the browser client is set to prefer them (prefer_edge): with clients spread evenly every node
# gets the same share, so an edge node's higher limit is never reached and it counts like any
# other node. A client cannot connect to more nodes than there are. All of it assumes an even
# spread and no other light clients already connected, so max_clients is an upper bound
light_capacity <- function(accepting, limit, connections, edge_nodes = 0, edge_limit = 0, clients = 0, prefer_edge = TRUE) {
  places <- if (prefer_edge) accepting * limit + edge_nodes * edge_limit else (accepting + edge_nodes) * min(limit, edge_limit)
  per_client <- min(connections, accepting + edge_nodes)
  list(places = places, per_client = per_client,
       max_clients = if (per_client > 0) floor(places / per_client) else 0,
       load = if (places > 0) clients * per_client / places else NA_real_)
}

# what it would take for today's nodes (without edge nodes) to hold `clients` at once; each line
# is one change on its own
what_it_takes <- function(accepting, limit, connections, edge_limit, clients) {
  needed <- clients * connections
  list(needed = needed,
       connections = if (clients > 0) floor(accepting * limit / clients) else NA_real_,
       limit = if (accepting > 0) ceiling(needed / accepting) else NA_real_,
       more_nodes = max(0, ceiling(needed / limit) - accepting),
       edge_with_network = max(0, ceiling((needed - accepting * limit) / edge_limit)),
       edge_alone = max(connections, ceiling(needed / edge_limit)))
}

# weeb-3 (commit 243eff5, src/network_profile.rs) dials a random 160 of its 319 built-in mainnet
# nodes when a tab starts; if `clients` tabs start together, each built-in node gets this many
# connection attempts
weeb3_bootnodes <- 319
weeb3_initial_dials <- 160
cold_start_per_node <- function(clients) clients * weeb3_initial_dials / weeb3_bootnodes
