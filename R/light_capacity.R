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
# accepting nodes take `limit` clients each. Extra nodes we run for browser clients count as more
# nodes, but with clients spread evenly every node gets the same share, so a node's higher limit is
# never reached: they add places at the common limit (the lower of the two). weeb-3 (commit 243eff5)
# has no setting that keeps a client's connections on chosen nodes; it can only start on them (see
# start_up below). A client cannot connect to more nodes than there are. All of it assumes an even
# spread and no other light clients already connected, so max_clients is an upper bound
light_capacity <- function(accepting, limit, connections, extra_nodes = 0, extra_limit = limit, clients = 0) {
  places <- (accepting + extra_nodes) * min(limit, extra_limit)
  per_client <- min(connections, accepting + extra_nodes)
  list(places = places, per_client = per_client,
       max_clients = if (per_client > 0) floor(places / per_client) else 0,
       load = if (places > 0) clients * per_client / places else NA_real_)
}

# what it would take for today's nodes to hold `clients` at once; each line is one change on its own
what_it_takes <- function(accepting, limit, connections, clients) {
  needed <- clients * connections
  list(needed = needed,
       connections = if (clients > 0) floor(accepting * limit / clients) else NA_real_,
       limit = if (accepting > 0) ceiling(needed / accepting) else NA_real_,
       more_nodes = max(0, ceiling(needed / limit) - accepting))
}

# start-up: weeb-3 (commit 243eff5) dials up to 160 start-up nodes when a tab opens: by default a
# random 160 of its 319 built-in mainnet nodes (src/network_profile.rs), or the page's own list if it
# passes one (bootstrapNodes in the start options, src/library.rs), which replaces the built-in list.
# If `clients` tabs open together, each start-up node gets this many connection attempts
weeb3_bootnodes <- 319
weeb3_initial_dials <- 160
cold_start_per_node <- function(clients, start_nodes = weeb3_bootnodes) clients * min(weeb3_initial_dials, start_nodes) / start_nodes
# how many start-up nodes, each with `limit` places, take a start-up burst of `clients` tabs. With
# 160 or fewer, every tab dials every one of them, so each gets all `clients` attempts; above that
# each tab dials 160, so each node gets clients x 160 / n
start_nodes_needed <- function(clients, limit) {
  if (clients <= limit) return(1)
  ceiling(clients * weeb3_initial_dials / limit)
}
