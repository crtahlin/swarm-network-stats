### how many ultra-light (browser) clients the network's full nodes can hold at once
# Each full node accepts up to light-node-limit light peers (bee defaultLightNodeLimit = 100,
# --light-node-limit since bee 2.8.2), and ultra-light peers count against the same limit. When
# the limit is exceeded, bee disconnects a random light peer to make room (pkg/p2p/libp2p,
# KickedOutPeersCount), so overload shows as churn, not as refused connections.

# ways to decide which full nodes accept clients, as shown in the app. Browser clients connect
# over secure WebSockets, so only full nodes offering a /tls/.../ws address can take them. Every
# full node in swarmscan's dump has been reached by swarmscan (it learns that a node is full from
# the handshake). Many of them do not report reachability, because swarmscan could not fetch their
# status, so "report themselves reachable" undercounts
light_node_rules <- c("Offer a secure WebSocket address (needed by browser clients)" = "websocket",
                      "Reached by swarmscan" = "reached", "Report themselves reachable" = "reachable")

# the number of full nodes that accept light clients under a rule
accepting_full_nodes <- function(nodes, rule = c("websocket", "reached", "reachable")) {
  full <- nodes[["fullNode"]] %in% TRUE
  accepting <- switch(match.arg(rule),
    websocket = full & nodes[["secure_websocket"]] %in% TRUE,
    reached = full & !(nodes[["unreachable"]] %in% TRUE),
    reachable = full & nodes[["statusSnapshot"]][["isReachable"]] %in% TRUE)
  sum(accepting)
}

# the light-peer slots on the accepting full nodes plus any extra edge nodes, how many
# connections a client really opens (no more than there are nodes to connect to), the most
# clients the slots hold, and how full they are with the expected clients. Assumes clients spread
# evenly over the nodes, which kademlia does not guarantee
light_capacity <- function(accepting, limit, connections, edge_nodes = 0, edge_limit = 0, clients = 0) {
  slots <- accepting * limit + edge_nodes * edge_limit
  per_client <- min(connections, accepting + edge_nodes)
  list(slots = slots, per_client = per_client,
       max_clients = if (per_client > 0) floor(slots / per_client) else 0,
       utilisation = if (slots > 0) clients * per_client / slots else NA_real_)
}
