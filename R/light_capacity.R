### how many browser light clients the network's full nodes can hold at once
# Network facts (verified 2026-10-07):
# - bee: each full node accepts up to --light-node-limit light peers (defaultLightNodeLimit = 100,
#   flag since bee 2.8.2). A peer with FullNode = false counts as a light peer; over the limit bee
#   disconnects a random light peer (pkg/p2p/libp2p/libp2p.go).
# - Pages served over HTTPS can only dial WSS: in swarmscan's dump, full nodes advertise plain TCP
#   underlays and /tls/.../ws underlays, and no other kind.
# How many peers a client keeps, and whether it dials a fixed list of bootnodes first, depends on
# the client, so these are inputs.

# WSS full nodes: full nodes that advertise a /tls/.../ws underlay. Whether each of them really
# accepts browser connections is not tested
browser_capable_nodes <- function(nodes) sum(nodes[["fullNode"]] %in% TRUE & nodes[["secure_websocket"]] %in% TRUE)

# max concurrent clients before nodes reach their light-node-limit. Each client keeps `peers`
# peers; `bootnode_peers` of them (at most one per node) go to `bootnodes` bootnodes with
# `bootnode_limit` each, and the rest spread evenly over the other `other_nodes` WSS full nodes with
# `other_limit` each. The answer is the smaller of the two. A client cannot keep more peers than
# there are nodes
client_capacity <- function(other_nodes, other_limit, peers, bootnodes = 0, bootnode_limit = other_limit,
                            bootnode_peers = 0, clients = 0) {
  on_list <- min(bootnode_peers, bootnodes, peers)
  elsewhere <- min(peers - on_list, other_nodes)
  list_clients <- if (on_list > 0) floor(bootnodes * bootnode_limit / on_list) else Inf
  other_clients <- if (elsewhere > 0) floor(other_nodes * other_limit / elsewhere) else Inf
  most <- min(list_clients, other_clients)
  if (!is.finite(most)) most <- 0
  list(on_list = on_list, elsewhere = elsewhere, list_clients = list_clients, other_clients = other_clients, most = most,
       load = if (most > 0) clients / most else if (clients > 0) Inf else 0,
       bound_by = if (list_clients <= other_clients) "list" else "network")
}

# what would let `clients` fit, one parameter changed at a time. `capable` is the number of WSS full
# nodes; bootnodes are counted among them. Returns one row per change: the parameter, the value
# needed and a status: "enough", "not enough on its own" (it fixes one side, but the other side
# still holds fewer clients), "already enough" (that side, or the whole network, already holds them)
# or "not possible"
what_it_takes <- function(capable, limit, peers, clients, bootnodes = 0, bootnode_limit = limit, bootnode_peers = 0) {
  holds <- function(b = bootnodes, bl = bootnode_limit, bp = bootnode_peers, l = limit, p = peers, cap = capable)
    client_capacity(max(0, cap - b), l, p, b, bl, bp)$most
  now <- client_capacity(max(0, capable - bootnodes), limit, peers, bootnodes, bootnode_limit, bootnode_peers)
  status <- function(most) if (most >= clients) "enough" else "not enough on its own"
  row <- function(change, value, status) data.frame(change = change, value = value, status = status)
  if (now$most >= clients) return(row("network", now$most, "already enough"))
  other <- max(0, capable - bootnodes)
  # peers per client on the non-bootnodes, before the cap at the number of nodes
  wanted_elsewhere <- peers - now$on_list
  rows <- list()
  if (now$on_list == 0) {
    p_new <- floor(capable * limit / clients)
    rows[[1]] <- if (p_new >= 1) row("peers", p_new, status(holds(p = p_new))) else row("peers", NA, "not possible")
    l_new <- ceiling(clients * min(peers, capable) / capable)
    rows[[2]] <- row("limit", l_new, status(holds(l = l_new)))
    # more nodes also raise the peers each client can keep, up to its peers per client
    n_new <- max(capable + 1, ceiling(clients * peers / limit))
    rows[[3]] <- row("nodes", n_new, status(holds(cap = n_new)))
  } else {
    # the other WSS full nodes; skipped when no peers go there
    if (wanted_elsewhere > 0) {
      if (now$other_clients >= clients) {
        rows[[1]] <- row("other", now$other_clients, "already enough")
      } else {
        l_new <- ceiling(clients * now$elsewhere / other)
        rows[[1]] <- row("limit", l_new, status(holds(l = l_new)))
        n_new <- max(other + 1, ceiling(clients * wanted_elsewhere / limit))
        rows[[2]] <- row("nodes", n_new, status(holds(cap = bootnodes + n_new)))
      }
    }
    # the bootnodes
    if (now$list_clients >= clients) {
      rows[[length(rows) + 1]] <- row("list", now$list_clients, "already enough")
    } else {
      bp_new <- floor(bootnodes * bootnode_limit / clients)
      rows[[length(rows) + 1]] <- if (bp_new >= 1) row("bootnode_peers", bp_new, status(holds(bp = bp_new))) else
        row("bootnode_peers", 0, "not possible")
      bl_new <- ceiling(clients * now$on_list / bootnodes)
      rows[[length(rows) + 1]] <- row("bootnode_limit", bl_new, status(holds(bl = bl_new)))
      # more bootnodes: each client keeps `bootnode_peers` of them, so each gets clients x bootnode_peers / bootnodes
      b_new <- ceiling(clients * min(bootnode_peers, peers) / bootnode_limit)
      rows[[length(rows) + 1]] <- if (b_new > capable) row("bootnodes", b_new, "not possible") else
        row("bootnodes", b_new, status(holds(b = b_new)))
    }
  }
  do.call(rbind, rows)
}

# if all clients start at about the same time, each bootnode gets this many dial attempts;
# `bootnode_peers` is the number each client actually keeps (already capped at its peers)
start_attempts_per_node <- function(clients, bootnodes, bootnode_peers) if (bootnodes > 0) clients * min(bootnode_peers, bootnodes) / bootnodes else 0
