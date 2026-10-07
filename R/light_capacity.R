### how many browser clients the network's full nodes can hold at once
# Network facts (verified 2026-10-07):
# - bee: each full node takes up to light-node-limit light clients (defaultLightNodeLimit = 100,
#   --light-node-limit since bee 2.8.2). A peer that says it is not a full node counts as a light
#   client; over the limit bee disconnects a random light client (pkg/p2p/libp2p/libp2p.go).
# - Browsers can only open secure WebSocket connections to full nodes: in swarmscan's dump, full
#   nodes advertise plain TCP addresses (which browsers cannot open) and /tls/.../ws addresses, and
#   no other transport.
# How a client spreads its connections is not a network fact, so it is an input: connections per
# client, and optionally a fixed list of start-up nodes each client connects to first.

# full nodes a browser can connect to: full nodes that advertise a secure WebSocket address.
# Whether each of them really accepts browsers is not tested
browser_capable_nodes <- function(nodes) sum(nodes[["fullNode"]] %in% TRUE & nodes[["secure_websocket"]] %in% TRUE)

# how many clients fit before nodes fill. Each client makes `connections` connections; `start` of
# them (at most one per node) go to a fixed start-up list of `list_nodes` nodes with `list_limit`
# places each, and the rest spread evenly over the other `other_nodes` browser-capable nodes with
# `other_limit` places each. The answer is the smaller of the two limits. Without a start-up list
# (list_nodes or start = 0) all connections spread evenly. A client cannot connect to more nodes
# than there are
client_capacity <- function(other_nodes, other_limit, connections, list_nodes = 0, list_limit = other_limit,
                            start = 0, clients = 0) {
  on_list <- min(start, list_nodes, connections)
  elsewhere <- min(connections - on_list, other_nodes)
  list_clients <- if (on_list > 0) floor(list_nodes * list_limit / on_list) else Inf
  other_clients <- if (elsewhere > 0) floor(other_nodes * other_limit / elsewhere) else Inf
  most <- min(list_clients, other_clients)
  if (!is.finite(most)) most <- 0
  list(on_list = on_list, elsewhere = elsewhere, list_clients = list_clients, other_clients = other_clients, most = most,
       load = if (most > 0) clients / most else if (clients > 0) Inf else 0,
       bound_by = if (list_clients <= other_clients) "list" else "network")
}

# what would let `clients` fit, each change on its own. `capable` is the number of nodes browsers can
# reach; start-up nodes are counted among them, so the rest of the network is capable - list_nodes.
# Returns one row per change: what, the value needed, and a status: "enough", "not enough on its own"
# (it fixes one side, but the other side still holds fewer clients), "already enough" (that side
# already holds them) or "not possible"
what_it_takes <- function(capable, limit, connections, clients, list_nodes = 0, list_limit = limit, start = 0) {
  holds <- function(list_n = list_nodes, list_l = list_limit, s = start, l = limit, c = connections, cap = capable)
    client_capacity(max(0, cap - list_n), l, c, list_n, list_l, s)$most
  now <- client_capacity(max(0, capable - list_nodes), limit, connections, list_nodes, list_limit, start)
  status <- function(most) if (most >= clients) "enough" else "not enough on its own"
  row <- function(change, needed, value, status) data.frame(change = change, needed = needed, value = value, status = status)
  other <- max(0, capable - list_nodes)
  rows <- list()
  if (now$on_list == 0) {
    c_new <- floor(capable * limit / clients)
    rows[[1]] <- if (c_new >= 1) row("connections", "nodes per client", c_new, status(holds(c = c_new))) else
      row("connections", "nodes per client", NA, "not possible")
    l_new <- ceiling(clients * now$elsewhere / capable)
    rows[[2]] <- row("limit", "places per node", l_new, status(holds(l = l_new)))
    n_new <- ceiling(clients * now$elsewhere / limit)
    rows[[3]] <- row("nodes", "nodes browsers can reach", n_new, status(holds(cap = n_new)))
  } else {
    # the rest of the network
    if (now$other_clients >= clients) {
      rows[[1]] <- row("other", "", now$other_clients, "already enough")
    } else {
      l_new <- ceiling(clients * now$elsewhere / other)
      rows[[1]] <- row("limit", "places per other node", l_new, status(holds(l = l_new)))
      n_new <- ceiling(clients * now$elsewhere / limit)
      rows[[2]] <- row("nodes", "other nodes browsers can reach", n_new, status(holds(cap = list_nodes + n_new)))
    }
    # the start-up list
    if (now$list_clients >= clients) {
      rows[[length(rows) + 1]] <- row("list", "", now$list_clients, "already enough")
    } else {
      s_new <- floor(list_nodes * list_limit / clients)
      rows[[length(rows) + 1]] <- if (s_new >= 1) row("start", "start-up connections per client", s_new, status(holds(s = s_new))) else
        row("start", "start-up connections per client", 0, "not possible")
      ll_new <- ceiling(clients * now$on_list / list_nodes)
      rows[[length(rows) + 1]] <- row("list_limit", "places per start-up node", ll_new, status(holds(list_l = ll_new)))
      # a longer list: each client makes `start` connections to it, so each node gets clients x start / nodes
      ln_new <- ceiling(clients * min(start, connections) / list_limit)
      rows[[length(rows) + 1]] <- if (ln_new > capable) row("list_nodes", "start-up nodes", ln_new, "not possible") else
        row("list_nodes", "start-up nodes", ln_new, status(holds(list_n = ln_new)))
    }
  }
  do.call(rbind, rows)
}

# if all clients start at about the same time, each start-up node gets this many connection attempts
start_attempts_per_node <- function(clients, list_nodes, start) if (list_nodes > 0) clients * min(start, list_nodes) / list_nodes else 0
