### read stake, redistribution reveals and the storage price from the Gnosis chain
# swarmscan's network dump has no stake and no record of who plays the redistribution game, so
# that comes from the storage-incentives contracts over JSON-RPC. Addresses are from
# mainnet_deployed.json in github.com/ethersphere/storage-incentives; event layouts from its
# src/Redistribution.sol, src/Staking.sol and src/PriceOracle.sol

chain_contracts <- list(
  redistribution = list(address = "0x5069cdfB3D9E56d23B1cAeE83CE6109A7E4fd62d", deployed = 41105199),
  staking        = list(address = "0xda2a16EE889E7F04980A8d597b48c8D51B9518F4", deployed = 40430237),
  price_oracle   = list(address = "0x47EeF336e7fE5bED98499A4696bce8f28c1B0a8b", deployed = 37339168)
)
# keccak256 of the event signatures (topic0) and the 4-byte selectors of the view functions
chain_topics <- list(
  revealed      = "0x13fc17fd71632266fe82092de6dd91a06b4fa68d8dc950492e5421cbed55a6a5", # Revealed(uint256,bytes32,uint256,uint256,bytes32,uint8)
  truth         = "0x34e8eda4cd857cd2865becf58a47748f31415f4a382cbb2cc0c64b9a27c717be", # TruthSelected(bytes32,uint8)
  stake_updated = "0x8fb3da6e133de1007392b10d429548e9c2565c791c52e1498b01b65d8797e74c", # StakeUpdated(address,uint256,uint256,bytes32,uint256,uint8)
  price_update  = "0xae46785019700e30375a5d7b4f91e32f8060ef085111f896ebf889450aa2ab5a"  # PriceUpdate(uint256)
)
chain_selectors <- list(current_price = "0x9d1b464a",  # currentPrice()
                        stakes = "0x16934fc4",         # stakes(address)
                        change_rate = "0x74e7493b",    # changeRate(uint256)
                        price_base = "0x7310561b",     # priceBase()
                        minimum_price = "0x7f386b6c",  # minimumPrice()
                        is_paused = "0xb187bd26")      # isPaused()

round_length_blocks <- 152           # Redistribution.ROUND_LENGTH
bzz_base_units <- 1e16               # BZZ has 16 decimals
min_stake_bzz <- 10                  # Staking.MIN_STAKE (1e17 base units), times 2^height
chain_window_days <- 30              # reveals and price changes are kept for this many days
chain_confirmations <- 10            # blocks this close to the head may still be reorganised; read later
chain_refresh_secs <- 10 * 60
rpc_timeout_secs <- 30
rpc_batch_size <- 250                # eth_calls per batch request
logs_min_span <- 1000                # a log query that fails is split in halves down to this many blocks

# the RPC endpoint; SWARM_RPC_URL overrides the public Gnosis endpoint
chain_rpc_url <- function() {
  url <- Sys.getenv("SWARM_RPC_URL")
  if (nzchar(url)) url else "https://rpc.gnosischain.com"
}

# send one JSON-RPC request (a single call, or a list of calls as a batch) and return the parsed
# answer; stops with a readable message on a network error or a non-200 response
rpc_post <- function(body) {
  response <- httr::POST(chain_rpc_url(), body = jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA),
                         httr::content_type_json(), httr::timeout(rpc_timeout_secs))
  if (httr::status_code(response) != 200) {
    stop("the Gnosis RPC answered with HTTP status ", httr::status_code(response))
  }
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
}

# an error the RPC answered with (class rpc_refused) differs from a network error or timeout:
# only a refusal is worth retrying with a smaller query
rpc_call <- function(method, params = list()) {
  answer <- rpc_post(list(jsonrpc = "2.0", id = 1, method = method, params = params))
  if (!is.null(answer$error)) {
    stop(structure(class = c("rpc_refused", "error", "condition"),
                   list(message = paste0("the Gnosis RPC refused ", method, ": ", answer$error$message), call = NULL)))
  }
  answer$result
}

# eth_call the same contract with many call data strings, in batches; returns the results in order.
# The public endpoint now and then refuses some calls of a batch (rate limiting), so calls left
# without an answer are sent again, up to rpc_batch_attempts times, after a short pause
rpc_batch_attempts <- 3
rpc_eth_calls <- function(to, data) {
  results <- rep(NA_character_, length(data))
  last_error <- ""
  for (attempt in seq_len(rpc_batch_attempts)) {
    open <- which(is.na(results))
    if (length(open) == 0) break
    if (attempt > 1) Sys.sleep(attempt - 1)
    for (start in seq(1, length(open), by = rpc_batch_size)) {
      idx <- open[start:min(start + rpc_batch_size - 1, length(open))]
      batch <- lapply(idx, function(i) list(jsonrpc = "2.0", id = i, method = "eth_call",
                                            params = list(list(to = to, data = data[i]), "latest")))
      answer <- rpc_post(batch)
      if (!is.data.frame(answer)) { last_error <- "the answer was not a list of results"; next }
      if (!is.null(answer$error)) last_error <- paste(unique(stats::na.omit(answer$error$message)), collapse = "; ")
      if (!is.null(answer$result)) results[idx] <- answer$result[match(idx, answer$id)]
    }
  }
  if (anyNA(results)) {
    stop("the Gnosis RPC did not answer ", sum(is.na(results)), " of ", length(results), " calls (", last_error, ")")
  }
  results
}

### hex decoding
# hex strings (with or without 0x) to numbers. Values above 2^53 lose precision, which is fine
# for stake and price figures but not for identifiers, so overlays and hashes stay strings
hex_to_number <- function(hex) {
  if (length(hex) == 0) return(numeric(0))
  hex <- sub("^0x", "", hex)
  width <- 7 * ceiling(max(nchar(hex), 1) / 7)   # 7 hex digits fit strtoi's 31-bit integers
  hex <- formatC(hex, width = width, flag = "0")
  hex <- gsub(" ", "0", hex, fixed = TRUE)
  value <- numeric(length(hex))
  for (k in seq_len(width / 7)) value <- value * 16^7 + strtoi(substr(hex, 7 * k - 6, 7 * k), 16L)
  value
}

# the n-th 32-byte word (64 hex digits) of ABI-encoded data
abi_word <- function(data, n) substr(sub("^0x", "", data), 64 * (n - 1) + 1, 64 * n)

### log queries
# logs of one event from one contract between two blocks. A query the RPC refuses (most often
# for returning too many results) is split in halves and retried. A network error or timeout
# stops the read at once: splitting would only repeat it, each time waiting for the timeout
fetch_logs <- function(address, topic, from_block, to_block) {
  if (from_block > to_block) return(list())
  result <- tryCatch(
    rpc_call("eth_getLogs", list(list(address = address, topics = list(topic),
                                      fromBlock = sprintf("0x%x", from_block), toBlock = sprintf("0x%x", to_block)))),
    rpc_refused = function(e) e)
  if (!inherits(result, "error")) return(if (length(result) == 0) list() else list(result))
  if (to_block - from_block < logs_min_span) stop(conditionMessage(result))
  middle <- from_block + (to_block - from_block) %/% 2
  c(fetch_logs(address, topic, from_block, middle), fetch_logs(address, topic, middle + 1, to_block))
}

# the pieces fetch_logs returns as one table: block, block time (NA when the RPC does not give
# it), the first indexed topic and the data
logs_table <- function(pieces) {
  empty <- data.frame(block = numeric(0), time = as.POSIXct(character(0), tz = "UTC"),
                      topic1 = character(0), data = character(0))
  if (length(pieces) == 0) return(empty)
  rows <- lapply(pieces, function(logs) {
    data.frame(block = hex_to_number(logs$blockNumber),
               time = if (is.null(logs$blockTimestamp)) as.POSIXct(NA, tz = "UTC") else
                 as.POSIXct(hex_to_number(logs$blockTimestamp), origin = "1970-01-01", tz = "UTC"),
               topic1 = vapply(logs$topics, function(t) if (length(t) >= 2) t[2] else NA_character_, ""),
               data = logs$data)
  })
  do.call(rbind, rows)
}

### decoders, one per event; overlays and hashes are lowercase hex without 0x, as in swarmscan's dump
decode_revealed <- function(logs) {
  data.frame(block = logs$block, time = logs$time,
             round = hex_to_number(abi_word(logs$data, 1)),
             overlay = tolower(abi_word(logs$data, 2)),
             stake_bzz = hex_to_number(abi_word(logs$data, 3)) / bzz_base_units,
             stake_density = hex_to_number(abi_word(logs$data, 4)),
             reserve_commitment = tolower(abi_word(logs$data, 5)),
             depth = hex_to_number(abi_word(logs$data, 6)))
}

# TruthSelected is emitted by the claim, in the round it closes
decode_truth <- function(logs) {
  data.frame(round = logs$block %/% round_length_blocks,
             truth_hash = tolower(abi_word(logs$data, 1)),
             truth_depth = hex_to_number(abi_word(logs$data, 2)))
}

decode_price_update <- function(logs) {
  data.frame(block = logs$block, time = logs$time, price = hex_to_number(abi_word(logs$data, 1)))
}

# (paste0 would turn an empty vector into "0x", hence the check)
stake_owners <- function(logs) {
  if (nrow(logs) == 0) return(character(0))
  unique(tolower(paste0("0x", substr(logs$topic1, 27, 66))))
}

# Staking.stakes(owner) for every owner: overlay, committedStake, potentialStake,
# lastUpdatedBlockNumber, height. Owners whose stake was deleted (migrated or slashed away)
# return zeros and are dropped
read_stakes <- function(owners, price, head_block) {
  calls <- paste0(chain_selectors$stakes, strrep("0", 24), sub("^0x", "", owners))
  answers <- if (length(owners)) rpc_eth_calls(chain_contracts$staking$address, calls) else character(0)
  stakes <- data.frame(owner = owners,
                       overlay = tolower(abi_word(answers, 1)),
                       committed_stake = hex_to_number(abi_word(answers, 2)),
                       stake_bzz = hex_to_number(abi_word(answers, 3)) / bzz_base_units,
                       last_updated_block = hex_to_number(abi_word(answers, 4)),
                       height = hex_to_number(abi_word(answers, 5)))
  stakes <- stakes[stakes$last_updated_block > 0, ]
  # a frozen stake has its last update moved into the future (Staking.freezeDeposit)
  stakes$frozen <- stakes$last_updated_block >= head_block
  # Staking.calculateEffectiveStake: the committed stake at today's price, capped at the deposit;
  # nodeEffectiveStake is 0 while frozen
  committed_bzz <- 2^stakes$height * stakes$committed_stake * price / bzz_base_units
  stakes$effective_stake_bzz <- ifelse(stakes$frozen, 0, pmin(committed_bzz, stakes$stake_bzz))
  stakes$minimum_stake_bzz <- min_stake_bzz * 2^stakes$height
  # Redistribution.commit: staked, not frozen, and the last update at least 2 rounds ago
  stakes$can_play <- stakes$last_updated_block < head_block - 2 * round_length_blocks
  stakes$committed_stake <- NULL
  rownames(stakes) <- NULL
  stakes
}

# the PriceOracle's parameters, read from the deployed contract: changeRate[0..8] (the price is
# multiplied by changeRate[redundancy] / priceBase once per claimed round), the minimum price
# and whether price changes are paused
read_oracle <- function() {
  calls <- c(paste0(chain_selectors$change_rate, sprintf("%064x", 0:8)),
             chain_selectors$price_base, chain_selectors$minimum_price, chain_selectors$is_paused)
  answers <- hex_to_number(rpc_eth_calls(chain_contracts$price_oracle$address, calls))
  list(change_rate = answers[1:9], price_base = answers[10], minimum_price = answers[11], paused = answers[12] != 0)
}

### one complete read, or an update of the previous one
# previous: the last chain data, or NULL. Only blocks after previous$to_block are read again;
# stakes are re-read in full every time, because freezes, withdrawals and height changes do not
# all emit events
fetch_chain_data <- function(previous = NULL) {
  head_block <- hex_to_number(rpc_call("eth_blockNumber")) - chain_confirmations
  head <- rpc_call("eth_getBlockByNumber", list(sprintf("0x%x", head_block), FALSE))
  head_time <- as.POSIXct(hex_to_number(head$timestamp), origin = "1970-01-01", tz = "UTC")
  # Gnosis makes a block about every 5 seconds; used to find the window start and to date logs
  # when the RPC leaves out the block time
  seconds_per_block <- 5
  window_start <- max(head_block - ceiling(chain_window_days * 86400 / seconds_per_block),
                      chain_contracts$redistribution$deployed)
  from <- if (is.null(previous)) window_start else max(previous$to_block + 1, window_start)
  date_logs <- function(logs) {
    missing <- is.na(logs$time)
    logs$time[missing] <- head_time - (head_block - logs$block[missing]) * seconds_per_block
    logs
  }
  redistribution <- chain_contracts$redistribution$address

  reveals <- decode_revealed(date_logs(logs_table(fetch_logs(redistribution, chain_topics$revealed, from, head_block))))
  truths <- decode_truth(logs_table(fetch_logs(redistribution, chain_topics$truth, from, head_block)))
  prices <- decode_price_update(date_logs(logs_table(
    fetch_logs(chain_contracts$price_oracle$address, chain_topics$price_update, from, head_block))))
  owner_from <- if (is.null(previous)) chain_contracts$staking$deployed else previous$to_block + 1
  owners <- stake_owners(logs_table(fetch_logs(chain_contracts$staking$address, chain_topics$stake_updated,
                                               owner_from, head_block)))

  if (!is.null(previous)) {
    keep <- previous$reveals$block >= window_start
    reveals <- rbind(previous$reveals[keep, names(reveals)], reveals)
    truths <- unique(rbind(previous$truths, truths))
    prices <- rbind(previous$prices[previous$prices$block >= window_start, ], prices)
    owners <- unique(c(previous$owners, owners))
  }
  truths <- truths[truths$round >= window_start %/% round_length_blocks, ]

  # a reveal matches when its commitment and depth equal the truth chosen in its round; reveals
  # in rounds not yet claimed (or never claimed) have no truth and stay NA
  truth <- truths[match(reveals$round, truths$round), ]
  reveals$matched_truth <- reveals$reserve_commitment == truth$truth_hash & reveals$depth == truth$truth_depth

  price <- hex_to_number(rpc_call("eth_call", list(list(to = chain_contracts$price_oracle$address,
                                                        data = chain_selectors$current_price), "latest")))
  list(to_block = head_block, head_time = head_time, window_start = window_start,
       price = price, prices = prices, reveals = reveals, truths = truths, owners = owners,
       stakes = read_stakes(owners, price, head_block), oracle = read_oracle())
}

### accessors for the views
# one row per reveal in the window: round, block time, overlay, stake, depth, matched_truth
chain_reveals <- function(chain) chain$reveals
# one row per staked overlay: stake, effective stake, height, frozen, whether it may play
chain_stakes <- function(chain) chain$stakes
# the current price and the price updates in the window, in PLUR per chunk per block, and the
# oracle's parameters
chain_price <- function(chain) list(current = chain$price, history = chain$prices, oracle = chain$oracle)

# the latest reveal of each overlay within the last `days` days
last_reveals <- function(chain, days) {
  reveals <- chain$reveals[chain$reveals$time >= chain$head_time - days * 86400, ]
  reveals <- reveals[order(reveals$block, decreasing = TRUE), ]
  reveals[!duplicated(reveals$overlay), c("overlay", "round", "time", "matched_truth")]
}

### shared cache, the same pattern as swarm_cache in app.R
chain_cache <- new.env()
chain_cache$data <- NULL
chain_cache$version <- 0
chain_cache$fetched_at <- NULL
chain_cache$last_attempt <- NULL
chain_cache$last_error <- NULL
chain_cache$next_attempt <- -Inf

refresh_chain_cache <- function() {
  now <- current_time()
  if (as.numeric(now) >= as.numeric(chain_cache$next_attempt)) {
    chain_cache$last_attempt <- now
    result <- tryCatch(fetch_chain_data(chain_cache$data), error = function(e) e)
    if (inherits(result, "error")) {
      chain_cache$last_error <- conditionMessage(result)
      chain_cache$next_attempt <- now + if (is.null(chain_cache$data)) retry_interval_secs else retry_with_data_secs
    } else {
      chain_cache$data <- result
      chain_cache$fetched_at <- now
      chain_cache$last_error <- NULL
      chain_cache$version <- chain_cache$version + 1
      chain_cache$next_attempt <- now + chain_refresh_secs
    }
  }
  chain_cache$version
}

# one line saying how fresh the chain data is and whether the last read failed
chain_status_text <- function() {
  stamp <- function(t) format(t, "%Y-%m-%d %H:%M UTC", tz = "UTC")
  if (is.null(chain_cache$data)) {
    return(paste0("No data from the Gnosis chain yet (", chain_cache$last_error, "). Retrying every minute."))
  }
  chain <- chain_cache$data
  status <- sprintf("Chain data up to Gnosis block %s (%s): %s staked overlays, %s revealed in the last %s days.",
                    format_number(chain$to_block), stamp(chain$head_time), format_number(nrow(chain$stakes)),
                    format_number(length(unique(chain$reveals$overlay))), format_number(chain_window_days))
  if (!is.null(chain_cache$last_error)) {
    status <- paste0(status, " The last read failed at ", stamp(chain_cache$last_attempt),
                     " (", chain_cache$last_error, "), so the data shown is older.")
  }
  status
}
