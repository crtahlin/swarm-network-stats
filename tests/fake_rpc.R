# A stand-in for the Gnosis chain: replaces rpc_post() from R/chain.R with one that answers
# from tests/fixtures/chain-sample.json, so no test reaches the network. Source it after app.R.
#
# The fixture was recorded from rpc.gnosischain.com at block 48632531 (2026-10-07): the
# Revealed and TruthSelected logs of Redistribution and the PriceUpdate logs of the PriceOracle
# for the 5,320 blocks before that block (6 hours plus 1,000 blocks), the StakeUpdated history of
# every owner whose overlay revealed in that span plus 20 other owners, those owners' stakes()
# answers, the CurrentRevealAnchor logs of Redistribution and the PotWithdrawn logs of PostageStamp
# for the same span, currentPrice(), and the PriceOracle's changeRate(0..8), priceBase(), minimumPrice() and
# isPaused() answers. Logs keep only the fields the app reads.

chain_fixture <- jsonlite::fromJSON("tests/fixtures/chain-sample.json", simplifyVector = FALSE)

fake_rpc <- new.env()
fake_rpc$head <- chain_fixture$head_block  # the head the fake chain reports, before confirmations
fake_rpc$max_logs <- Inf                   # a log query with more results than this is refused
fake_rpc$empty_over <- Inf
fake_rpc$seconds_per_block <- 5            # block headers are this many seconds apart                 # a log query with more results than this gets an empty list, without an error
fake_rpc$fail <- FALSE                     # TRUE: every request fails as an HTTP error
fake_rpc$drop_calls <- 0                   # this many eth_calls in batches are refused, then answered
fake_rpc$requests <- 0

fake_answer <- function(call) {
  params <- call$params
  answer <- function(result) list(jsonrpc = "2.0", id = call$id, result = result)
  refuse <- function(message) list(jsonrpc = "2.0", id = call$id, error = list(code = -32005, message = message))
  switch(call$method,
    eth_blockNumber = answer(sprintf("0x%x", fake_rpc$head + chain_confirmations)),
    eth_getBlockByNumber = {
      block <- hex_to_number(params[[1]])
      answer(list(number = params[[1]], timestamp = sprintf("0x%x",
        round(hex_to_number(chain_fixture$head_timestamp) - fake_rpc$seconds_per_block * (chain_fixture$head_block - block)))))
    },
    eth_getLogs = {
      filter <- params[[1]]
      from <- hex_to_number(filter$fromBlock); to <- hex_to_number(filter$toBlock)
      logs <- Filter(function(l) {
        block <- hex_to_number(l$blockNumber)
        l$topics[[1]] == filter$topics[[1]] && block >= from && block <= to
      }, chain_fixture$logs[[tolower(filter$address)]])
      if (length(logs) > fake_rpc$max_logs) refuse("query returned more than 10000 results") else
        if (length(logs) > fake_rpc$empty_over) answer(list()) else answer(logs)
    },
    eth_call = {
      to <- tolower(params[[1]]$to); data <- params[[1]]$data
      if (to == tolower(chain_contracts$price_oracle$address)) {
        if (data == chain_selectors$current_price) return(answer(chain_fixture$current_price))
        recorded <- chain_fixture$oracle_calls[[data]]
        return(if (is.null(recorded)) refuse("execution reverted") else answer(recorded))
      }
      if (fake_rpc$drop_calls > 0) { fake_rpc$drop_calls <- fake_rpc$drop_calls - 1; return(refuse("rate limit")) }
      stake <- chain_fixture$stakes[[paste0("0x", substr(data, 35, 74))]]
      answer(if (is.null(stake)) paste0("0x", strrep("0", 320)) else stake)
    },
    refuse(paste("the fake chain does not know", call$method)))
}

rpc_post <- function(body) {
  fake_rpc$requests <- fake_rpc$requests + 1
  if (fake_rpc$fail) stop("the Gnosis RPC answered with HTTP status 503")
  answer <- if (is.null(names(body))) lapply(body, fake_answer) else fake_answer(body)
  jsonlite::fromJSON(jsonlite::toJSON(answer, auto_unbox = TRUE, digits = NA), simplifyVector = TRUE)
}
