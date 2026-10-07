#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### load libraries
# httr and jsonlite are used as httr:: and jsonlite::, so they are not attached
library(shiny)
library(ggplot2)
library(DT)
library(leaflet)
library(forstringr)  # str_right
library(SwarmR)      # first_n_places
library(dplyr)
library(bslib)


### load data from swarmscan.io
swarmscan_dump_url <- "https://api.swarmscan.io/v1/network/dump"
refresh_interval_secs <- 10 * 60  # download new data this often
retry_interval_secs <- 60         # after a failed download with no data yet, try again this soon
retry_with_data_secs <- 5 * 60    # after a failed refresh while older data is shown, try again this soon
download_timeout_secs <- 20       # the download normally takes 1-2 s; a hanging server blocks every session until this runs out
current_time <- function() Sys.time()

# download the network dump; stops with a readable message on a network error,
# a non-200 response, or a response that does not contain the nodes table
fetch_swarmscan_data <- function() {
  response <- httr::GET(swarmscan_dump_url, httr::timeout(download_timeout_secs))
  if (httr::status_code(response) != 200) {
    stop("swarmscan answered with HTTP status ", httr::status_code(response))
  }
  data <- jsonlite::parse_json(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  if (!is.data.frame(data$nodes) || nrow(data$nodes) == 0 || is.null(data$nodes$overlay)) {
    stop("swarmscan's response does not contain a list of nodes")
  }
  data
}

# a number as page text: thousands separator and at most two decimals, e.g. 4,693 or 7.32;
# values below 1 keep 3 significant digits, so 16 GiB shows as 0.0156 TiB rather than 0.02
format_number <- function(x) {
  x <- ifelse(abs(x) < 1, signif(x, 3), round(x, 2))
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

# overlay addresses as bit strings. The app never needs more than the first few bits (the radius
# input goes up to 16, and the radius search stops long before 64), so only the first 16 hex digits
# (64 bits) are converted, one vectorised lookup per digit, instead of SwarmR's hexadecimal2binary,
# which converts all 256 bits one character at a time
hex_bits <- c("0" = "0000", "1" = "0001", "2" = "0010", "3" = "0011", "4" = "0100", "5" = "0101",
              "6" = "0110", "7" = "0111", "8" = "1000", "9" = "1001", "a" = "1010", "b" = "1011",
              "c" = "1100", "d" = "1101", "e" = "1110", "f" = "1111")
overlay_to_bits <- function(overlay, hex_digits = 16) {
  hex <- tolower(substr(overlay, 1, hex_digits))
  bits <- character(length(hex))
  for (k in seq_len(hex_digits)) bits <- paste0(bits, hex_bits[substr(hex, k, k)])
  unname(bits)
}

# names of all 2^radius nbhoods in order ("00", "01", "10", "11" for radius 2), built by doubling
# the list and cached per radius; replaces SwarmR's generate_short_overlay, which loops over
# 2^radius numbers on every call
nbhood_name_cache <- new.env()
nbhood_names <- function(radius) {
  # 2^radius names are built and kept, so the radius must stay small (the input allows 1 to 16)
  stopifnot(length(radius) == 1, radius %in% 0:16)
  key <- as.character(radius)
  if (is.null(nbhood_name_cache[[key]])) {
    names <- ""
    for (k in seq_len(radius)) names <- as.vector(t(outer(names, c("0", "1"), paste0)))
    nbhood_name_cache[[key]] <- names
  }
  nbhood_name_cache[[key]]
}

# TRUE where a string field is present and not empty; a missing field gives FALSE
has_text <- function(x) {
  if (is.null(x)) return(FALSE)
  !is.na(x) & nchar(x) > 0
}

# TRUE for IPv4 addresses reachable on the public internet (not private, loopback, link-local, CGNAT or multicast)
is_public_ip4 <- function(ip) {
  vapply(strsplit(ip, ".", fixed = TRUE), function(o) {
    o <- suppressWarnings(as.integer(o))
    if (length(o) != 4 || anyNA(o)) return(FALSE)
    !(o[1] %in% c(0, 10, 127) || o[1] >= 224 ||
        (o[1] == 172 && o[2] >= 16 && o[2] <= 31) || (o[1] == 192 && o[2] == 168) ||
        (o[1] == 169 && o[2] == 254) || (o[1] == 100 && o[2] >= 64 && o[2] <= 127))
  }, logical(1))
}

# TRUE for IPv6 addresses reachable on the public internet (not loopback, unique local, link-local, multicast or IPv4-mapped)
is_public_ip6 <- function(ip) {
  ip <- tolower(ip)
  !is.na(ip) & grepl(":", ip, fixed = TRUE) &
    !(ip %in% c("::", "::1") | grepl("^(fc|fd|fe[89ab]|ff)", ip) | startsWith(ip, "::ffff:"))
}

# public IP addresses of one node, taken from its underlay multiaddresses (/ip4/<address>/... or /ip6/<address>/...)
public_ips <- function(underlays) {
  if (is.null(underlays) || NROW(underlays) == 0) return(character(0))
  parts <- strsplit(underlays$address, "/", fixed = TRUE)
  family <- vapply(parts, `[`, "", 2)
  ip <- vapply(parts, `[`, "", 3)
  keep <- (family %in% "ip4" & is_public_ip4(ip)) | (family %in% "ip6" & is_public_ip6(ip))
  unique(ip[keep])
}

### prepare data
# turn the downloaded dump into the nodes table the app works with
prepare_nodes_data <- function(swarmscan_data) {
  # extract data about nodes
  nodes_data <- swarmscan_data$nodes
  # calculate binary overlay address (first 64 bits) and add it to data
  nodes_data$overlay_binary <- overlay_to_bits(nodes_data$overlay)
  # if unreachable column does not exist, fill it with NAs (to avoid corner case)
  if (is.null(nodes_data$unreachable)) {nodes_data$unreachable <- NA}

  # the location table; [[ ]] matches names exactly, because $ would partially match a missing
  # "location" to the "location_source" column added below. A dump without any location gets an
  # all-missing table, so the map and the Nodes info tab still work
  location <- nodes_data[["location"]]
  if (!is.data.frame(location)) location <- data.frame(row.names = seq_len(nrow(nodes_data)))
  # missing fields are added with the right type: the map needs numeric coordinates even when all are missing
  field_or_na <- function(field) if (is.null(location[[field]])) NA else location[[field]]
  for (field in c("latitude", "longitude")) location[[field]] <- as.numeric(field_or_na(field))
  for (field in c("country", "city")) location[[field]] <- as.character(field_or_na(field))
  
  # swarmscan gives nodes it failed to geolocate the placeholder location 0,0 with no (or an empty)
  # country, which would put them in the ocean off West Africa; treat it as missing, and treat a
  # location with only one of the two coordinates as missing too
  no_country <- is.na(location$country) | location$country == ""
  placeholder <- !is.na(location$latitude) & !is.na(location$longitude) &
    location$latitude == 0 & location$longitude == 0 & no_country
  unusable <- placeholder | is.na(location$latitude) | is.na(location$longitude)
  location$latitude[unusable] <- NA
  location$longitude[unusable] <- NA
  location$country[no_country] <- NA
  nodes_data[["location"]] <- location
  nodes_data$location_source <- ifelse(is.na(location$latitude), NA, "swarmscan")

  # the same public IP is often located for one node and not for another, so borrow the location
  # from a located node that shares a public IP; private addresses (e.g. Docker's 172.17.0.1) are
  # shared by unrelated nodes and are never used. If candidates differ, the most common location wins
  underlays <- nodes_data[["underlays"]]
  if (is.null(underlays)) underlays <- vector("list", nrow(nodes_data))
  node_ips <- lapply(underlays, public_ips)
  located <- which(!is.na(location$latitude))
  ip_rows <- data.frame(ip = unlist(node_ips[located]), row = rep(located, lengths(node_ips[located])))
  location_key <- do.call(paste, c(location, sep = "|"))
  for (i in which(is.na(location$latitude))) {
    candidates <- ip_rows$row[ip_rows$ip %in% node_ips[[i]]]
    if (length(candidates) == 0) next
    keys <- location_key[candidates]
    best <- candidates[match(names(which.max(table(keys))), keys)]
    nodes_data[["location"]][i, ] <- location[best, ]
    nodes_data$location_source[i] <- "same IP"
  }
  
  # the underlay addresses were only needed for borrowing locations; dropping them halves the cache
  nodes_data[["underlays"]] <- NULL

  nodes_data
}

# the storage radius most nodes report, used as the default radius; NULL if no node reports one
typical_storage_radius <- function(nodes_data) {
  radius <- nodes_data[["statusSnapshot"]][["storageRadius"]]
  radius <- radius[!is.na(radius) & radius > 0]
  if (length(radius) == 0) return(NULL)
  as.integer(names(which.max(table(radius))))
}

### shared data cache
# one copy of the data for all sessions of this R process. refresh_swarm_cache() downloads new
# data when it is due; if a download or its preparation fails, the last good data is kept and
# the download is retried (every minute with no data, every 5 minutes with older data shown).
# version changes only when new data arrives
swarm_cache <- new.env()
swarm_cache$data <- NULL          # list(counts = swarmscan's node counts, nodes = prepared nodes table)
swarm_cache$version <- 0
swarm_cache$fetched_at <- NULL
swarm_cache$last_attempt <- NULL
swarm_cache$last_error <- NULL
swarm_cache$next_attempt <- -Inf

refresh_swarm_cache <- function() {
  now <- current_time()
  if (as.numeric(now) >= as.numeric(swarm_cache$next_attempt)) {
    swarm_cache$last_attempt <- now
    result <- tryCatch({
      raw <- fetch_swarmscan_data()
      # keep only what the app reads, so the raw nodes table is not held twice
      list(counts = list(count = raw$count, unreachableCount = raw$unreachableCount),
           nodes = prepare_nodes_data(raw))
    }, error = function(e) e)
    if (inherits(result, "error")) {
      swarm_cache$last_error <- conditionMessage(result)
      swarm_cache$next_attempt <- now + if (is.null(swarm_cache$data)) retry_interval_secs else retry_with_data_secs
    } else {
      swarm_cache$data <- result
      swarm_cache$fetched_at <- now
      swarm_cache$last_error <- NULL
      swarm_cache$version <- swarm_cache$version + 1
      swarm_cache$next_attempt <- now + refresh_interval_secs
    }
  }
  swarm_cache$version
}

# one line saying how fresh the data is and whether the last download failed
data_status_text <- function() {
  stamp <- function(t) format(t, "%Y-%m-%d %H:%M UTC", tz = "UTC")
  if (is.null(swarm_cache$data)) {
    return(paste0("No data from swarmscan yet (", swarm_cache$last_error, "). Retrying every minute."))
  }
  status <- paste0("Data from swarmscan, fetched ", stamp(swarm_cache$fetched_at),
                   ". Refreshed every ", refresh_interval_secs / 60, " minutes.")
  if (!is.null(swarm_cache$last_error)) {
    status <- paste0(status, " The last refresh failed at ", stamp(swarm_cache$last_attempt),
                     " (", swarm_cache$last_error, "), so the data shown is older. Next attempt at ",
                     stamp(swarm_cache$next_attempt), ".")
  }
  status
}

# stake, reveals and price from the Gnosis chain, with their own cache (chain_cache)
# local = TRUE: runApp evaluates app.R in its own environment, which chain.R must see
source("R/chain.R", local = TRUE)


### LOOK AND FEEL
# dark slate, orange and mint, after the colours of ethswarm.org (not an exact copy).
# Space Grotesk for text; JetBrains Mono for labels, figures, tables and overlay bit strings.
# The fonts load from Google Fonts in the browser, so the server needs no font files
swarm_colours <- list(
  bg = "#0d1216", surface = "#151c22", line = "#2d3843", text = "#e7eaee", muted = "#8b909a",
  orange = "#ff6b26", mint = "#14fec0",
  bars = "#5b6b7a", error = "#f2c14e", unreachable = "#ff4d5e"  # plot: grey, yellow, red
)
mono_font <- font_google("JetBrains Mono", local = FALSE)
swarm_theme <- bs_theme(
  version = 5,
  bg = swarm_colours$bg, fg = swarm_colours$text,
  primary = swarm_colours$orange, secondary = swarm_colours$line, success = swarm_colours$mint,
  base_font = font_google("Space Grotesk", local = FALSE),
  heading_font = mono_font, code_font = mono_font,
  "border-color" = swarm_colours$line
)
swarm_css <- paste0("
  :root { --mono: 'JetBrains Mono', ui-monospace, monospace; }
  .navbar { border-bottom: 1px solid ", swarm_colours$line, "; }
  .navbar-brand { font-family: var(--mono); letter-spacing: 0.04em; }
  .navbar-brand .hex { color: ", swarm_colours$orange, "; margin-right: 0.4em; }
  .navbar .nav-link { font-family: var(--mono); font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.08em; }
  .navbar .nav-link.active { color: ", swarm_colours$orange, " !important; box-shadow: inset 0 -2px 0 ", swarm_colours$orange, "; }
  .bslib-sidebar-layout > .sidebar { background: ", swarm_colours$surface, "; border-right: 1px solid ", swarm_colours$line, "; }
  .sidebar-title, .section-label { font-family: var(--mono); font-size: 0.75rem; text-transform: uppercase;
                                   letter-spacing: 0.1em; color: ", swarm_colours$muted, "; margin: 0.25rem 0 0.75rem; }
  .section-label::before { content: '> '; color: ", swarm_colours$orange, "; }
  #data_status, #chain_status, #map_note { font-family: var(--mono); font-size: 0.8rem; color: ", swarm_colours$muted, "; }
  #nodes_count { font-family: var(--mono); font-weight: bold; color: ", swarm_colours$mint, "; margin-bottom: 1em; }
  .bslib-value-box { background: ", swarm_colours$surface, " !important; border: 1px solid ", swarm_colours$line, "; }
  .bslib-value-box .value-box-title { font-family: var(--mono); font-size: 0.8rem; text-transform: uppercase;
                                      letter-spacing: 0.08em; color: ", swarm_colours$muted, "; }
  .bslib-value-box .value-box-value { font-family: var(--mono); color: ", swarm_colours$mint, "; }
  .bslib-value-box .value-box-value .shiny-output-error-validation { font-size: 0.9rem; color: ", swarm_colours$error, "; }
  .bslib-value-box p { font-size: 0.8rem; color: ", swarm_colours$muted, "; }
  table.dataTable, .dataTables_wrapper { font-family: var(--mono); font-size: 0.8rem; }
  .table { --bs-table-striped-bg: rgba(139, 144, 154, 0.07); --bs-table-hover-bg: rgba(255, 107, 38, 0.10); }
  #explainer_text_1 { font-size: 0.85rem; color: ", swarm_colours$muted, "; }
  .marker-cluster-small, .marker-cluster-medium, .marker-cluster-large { background-color: rgba(255, 107, 38, 0.25); }
  .marker-cluster-small div, .marker-cluster-medium div, .marker-cluster-large div {
    background-color: rgba(255, 107, 38, 0.85); color: ", swarm_colours$bg, "; font-family: var(--mono); font-weight: bold; }
")

# the same look for the neighbourhood plot (R graphics use the system's monospace font)
swarm_plot_theme <- theme(
  plot.background = element_rect(fill = swarm_colours$bg, colour = NA),
  panel.background = element_rect(fill = swarm_colours$surface, colour = NA),
  panel.grid.major = element_line(colour = swarm_colours$line),
  panel.grid.minor = element_blank(),
  text = element_text(size = 14),
  axis.text = element_text(colour = "#aab2bc", family = "mono", size = 11),
  axis.title = element_text(colour = swarm_colours$text, family = "mono", size = 13),
  axis.ticks = element_line(colour = swarm_colours$line)
)


### APPLICATION
### UI part
ui <- 
  page_navbar(
    title = span(span(class = "hex", HTML("&#x2B22;")), "swarm network stats"),
    theme = swarm_theme,
    # tabs keep their fixed heights and the page scrolls; filling the window squeezed the
    # 800px plot so the text under it overlapped the axis
    fillable = FALSE,
    header = tags$style(HTML(swarm_css)),
    # settings part
    sidebar = sidebar(title = "Settings",
                      # 9 until data arrives; then the radius most nodes report (see server)
                      numericInput("storageRadius", "Storage radius",
                                   value = 9, min = 1, max = 16, step = 1),
                      numericInput("minNodesPerNbhood", "Minimum nodes per nbhood",
                                   value = 2, min = 1, max = 8),
                      checkboxInput("onlyFullNodes", "Show only full nodes",
                                    value = TRUE),
                      textOutput("data_status"),
                      textOutput("chain_status")),
    # panels part
    ###
    nav_panel("Map", 
              div(class = "section-label", "Map of nodes"),
              textOutput("map_note"),
              leafletOutput("leafletMap", height = "800px")),
    ###
    nav_panel("Data", 
              div(class = "section-label", "Storage on the network"),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "Stored data", value = textOutput("storage_taken"),
                          p("Estimated total amount of stored data, in TiB (2^40 bytes), from all nodes whatever the filter")),
                value_box(title = "Maximum storage radius", value = textOutput("max_radius"),
                          p("With the set minimum required nodes per neighbourhood")),
                value_box(title = "Maximum capacity", value = textOutput("max_capacity"),
                          p("Of storage at that radius, in TiB (2^40 bytes)"))
              )
              ), 
    
    ### 
    nav_panel("Reachability",
              div(class = "section-label", "Reachability of nodes"),
              DT::dataTableOutput("reachability_status")),
    
    ###
    nav_panel("Nbhood plot", 
              div(class = "section-label", "Number of nodes (all nodes, whatever the filter)"),
              textOutput("nodes_count"),
              div(class = "section-label", "Count of nodes per neighbourhood"),
              plotOutput("distPlot", height = "800px"),
              br(),
              textOutput("explainer_text_1")),

    # ###
    # nav_panel("Nbhood counts",
    #           "Nodes per neighborhood, sorted by least numerous based on selected radius",
    #           DT::dataTableOutput("nbhood_counts")),
 
    ###
    nav_panel("Nbhoods stats",
              div(class = "section-label", "Neighbourhoods statistics"),
              p("A node counts as an error node when swarmscan reports an error contacting it, or an error ",
                "fetching its status (the Error and Status error columns on the Nodes info tab). Most status ",
                "errors are \"peer not found\", meaning swarmscan could not fetch the status, and can be benign."),
              DT::dataTableOutput("stats_table")),
    
    ###
    nav_panel("Nodes info",
              div(class = "section-label", "Individual nodes statistics"),
              DT::dataTableOutput("nodes_data"),
              br(),
              div(class = "section-label", "Staked nodes (Gnosis chain)"),
              p("Every overlay with stake in the staking contract, and its latest reveal in the redistribution game ",
                "within the last 30 days. Effective stake is what the game weighs: the committed stake at today's ",
                "price, capped at the deposit, and 0 while frozen. A node can play once its stake is at least 2 rounds ",
                "old and not frozen. The truth match is empty for a round that has not been claimed."),
              DT::dataTableOutput("stakes_table"))
  )


# server logic: data, plots, tables and text outputs
server <- function(input, output, session) {

  ###############
  # PREPARE THE DATA (reactive function)
  ###############
  # check once a minute whether new data is due; outputs recompute only when new data arrives.
  # while there is no data yet, every failed attempt also counts as a change, so the
  # "no data" message below always shows the latest error
  swarm_data_polled <- reactivePoll(60 * 1000, session,
                                    checkFunc = function() {
                                      version <- refresh_swarm_cache()
                                      if (is.null(swarm_cache$data)) paste(version, format(swarm_cache$last_attempt)) else version
                                    },
                                    valueFunc = function() swarm_cache$data)

  # the current data; until the first download succeeds, outputs show why there is no data
  swarm_data <- reactive({
    data <- swarm_data_polled()
    shiny::validate(shiny::need(!is.null(data),
                                paste0("No data from swarmscan yet (", swarm_cache$last_error, "). Retrying every minute.")))
    data
  })

  # when the first data arrives, set the radius to the one most nodes report, within the 1-16 the
  # app accepts; a radius the user already changed (while waiting for data) is left alone
  observeEvent(swarm_data_polled(), {
    radius <- typical_storage_radius(swarm_data_polled()$nodes)
    if (!is.null(radius) && isTRUE(input$storageRadius == 9)) {
      updateNumericInput(session, "storageRadius", value = min(max(radius, 1), 16))
    }
  }, once = TRUE)
  
  # data freshness, shown in the sidebar; re-read every minute so a failed refresh shows up
  output$data_status <- renderText({
    invalidateLater(60 * 1000)
    swarm_data_polled()
    data_status_text()
  })

  # chain data (stake, reveals, price), polled like the swarmscan data; outputs that use it
  # read chain_data_polled() and recompute when a new read arrives
  chain_data_polled <- reactivePoll(60 * 1000, session,
                                    checkFunc = function() {
                                      version <- refresh_chain_cache()
                                      if (is.null(chain_cache$data)) paste(version, format(chain_cache$last_attempt)) else version
                                    },
                                    valueFunc = function() chain_cache$data)

  output$chain_status <- renderText({
    invalidateLater(60 * 1000)
    chain_data_polled()
    chain_status_text()
  })

  # based on the storage radius set, take the first n chars of the overlay address and add to the data
  nodes_data_reactive <- reactive({
    # the input's max = 16 is not enforced on typed values, and outputs build 2^radius
    # nbhood names, so only whole radii from 1 to 16 are accepted
    shiny::validate(shiny::need(isTRUE(input$storageRadius %in% 1:16), "Enter a storage radius from 1 to 16."))
    nodes_data <- swarm_data()$nodes
    nodes_data$overlay_short <- first_n_places(nodes_data$overlay_binary, input$storageRadius)
    nodes_data$overlay_short_next <- str_right( first_n_places(nodes_data$overlay_binary, (input$storageRadius + 1)), 1 )
    # set TRUE if an error string is found in the top-level or the status snapshot error field
    nodes_data$error_logical <- has_text(nodes_data$error) | has_text(nodes_data$statusSnapshot$error)
    
    # if user sets to only display full nodes, filter out the rest
    if (input$onlyFullNodes) {
      nodes_data <- nodes_data[!is.na(nodes_data$fullNode), ]
      nodes_data <- nodes_data[nodes_data$fullNode, ]
    }
    
    # return data
    return(nodes_data)
  })
  
  ###############
  # PREPARE THE PLOTS
  ###############
  
  # barchart nodes per neighbourhood
  output$distPlot <- renderPlot({
    # output the distribution by neighborhoods

    # all nodes count towards their nbhood, with or without a known location (same as the Nbhoods stats table)
    nodes_data <- nodes_data_reactive()
    # every nbhood is a level, so empty nbhoods keep their place on the x axis (as gaps)
    nodes_data$overlay_short <- factor(nodes_data$overlay_short, levels = nbhood_names(input$storageRadius))
    # average nodes per nbhood, counting empty nbhoods too
    average_per_nbhood <- nrow(nodes_data) / 2^input$storageRadius
    
    # generate plot
    plot <-
      ggplot(data = nodes_data, aes(x = overlay_short)) +
      geom_bar(fill = swarm_colours$bars) +
      geom_hline(yintercept = average_per_nbhood, colour = swarm_colours$mint, linetype = "dashed") +
      # yellow: nodes with an error in either error field (same rule as the Nbhoods stats table),
      # plus unreachable nodes, so the yellow visible above red is always reachable nodes with an error;
      # red: unreachable nodes, drawn on top. Both subsets come from the plotted data, so rows line up
      geom_bar(data = nodes_data[nodes_data$error_logical | nodes_data$unreachable %in% TRUE, ], fill = swarm_colours$error, width = 1) +
      geom_bar(data = nodes_data[nodes_data$unreachable %in% TRUE, ], fill = swarm_colours$unreachable, width = 1) +
      scale_x_discrete(drop = FALSE) +
      swarm_plot_theme +
      # more than 128 labels overlap into an unreadable band (and 65,536 at radius 16 slow the
      # drawing), so above that the labels are hidden and the axis title gives the count instead
      theme(axis.text.x = if (2^input$storageRadius <= 128) element_text(angle = -90, hjust = 0) else element_blank()) +
      labs(y = "Node count",
           x = if (2^input$storageRadius <= 128) "Neighbourhood" else
             paste0("Neighbourhood (", format_number(2^input$storageRadius), ", labels hidden; see Nbhoods stats)"))
    
    # return plot
    return(plot)
  })
  
  # how many shown nodes have a borrowed location, and how many are left off the map
  output$map_note <- renderText({
    shown <- nodes_data_reactive()
    sprintf("Locations of %d nodes are taken from another node with the same public IP address. %d nodes have no known location and are not shown.",
            sum(shown$location_source %in% "same IP"), sum(is.na(shown$location$latitude)))
  })

  # Leaflet map plot
  output$leafletMap <- renderLeaflet({
    # generate plot
    plot <- 
      leaflet() %>% 
      # Esri's dark grey base map; CARTO's dark tiles now need an API key
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ", options = tileOptions(maxZoom = 16)) %>%
      # addAwesomeMarkers(lat = nodes_data$location$latitude, lng = nodes_data$location$longitude) %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(), lat = nodes_data_reactive()$location$latitude, lng = nodes_data_reactive()$location$longitude,
                       radius = 5, stroke = FALSE, fillColor = swarm_colours$orange, fillOpacity = 0.9)
    
    # return plot
    return(plot)
  })
  
  ##############
  # PREPARE THE TABLES
  #############
  
  # table of counts per bins
  output$stats_table <- DT::renderDataTable({
    overlayAsFactor <-
      factor( x = nodes_data_reactive()$overlay_short,
              levels = nbhood_names(input$storageRadius) )

    # count nodes and nodes with errors per nbhood; subsetting a factor keeps all levels,
    # so both counts cover every nbhood even when no node has an error
    node_counts <- table(overlayAsFactor)
    error_counts <- table(overlayAsFactor[nodes_data_reactive()$error_logical])

    error_stats_per_nbhood <-
      data.frame(
        Freq = as.vector(node_counts),
        Freq.error = as.vector(error_counts),
        row.names = names(node_counts)
      )
    # an empty nbhood has no error percentage; NA shows as an empty cell and keeps the column numeric for sorting
    error_stats_per_nbhood$Percent.error <-
      ifelse(error_stats_per_nbhood$Freq > 0,
             (error_stats_per_nbhood$Freq.error / error_stats_per_nbhood$Freq)*100,
             NA)

    return(error_stats_per_nbhood)
    
  }, 
  colnames = c("Neighbourhoud", "Node count", "Error count", "Error percent") 
  )
  
  # table of all the data
  output$nodes_data <- DT::renderDataTable({
    shown <- nodes_data_reactive()
    # a field swarmscan leaves out entirely shows as an empty column instead of breaking the table.
    # [[ ]] matches names exactly: $ would partially match a missing "error" to "error_logical"
    column <- function(x) if (is.null(x)) rep(NA, nrow(shown)) else x

    # both error fields are shown, because the Nbhoods stats table counts a node with either one
    nodes_info <- data.frame(
      overlay_short = shown[["overlay_short"]],
      overlay_short_next = shown[["overlay_short_next"]],
      overlay = shown[["overlay"]],
      error = column(shown[["error"]]),
      status_error = column(shown[["statusSnapshot"]][["error"]]),
      unreachable = column(shown[["unreachable"]]),
      fullNode = column(shown[["fullNode"]]),
      # add info about country from location list
      location = column(shown[["location"]][["country"]]),
      # "swarmscan", or "same IP" when the location was borrowed from another node with that public IP
      location_source = column(shown[["location_source"]])
    )

    # return table with info
    return(nodes_info)
  },
  colnames = c("Neighbourhood", "Next bit", "Overlay", "Error", "Status error", "Unreachable", "Full node", "Location", "Location source"),
  rownames = FALSE 
  )
  
  # table of staked overlays from the chain, with each one's latest reveal in the window
  output$stakes_table <- DT::renderDataTable({
    chain <- chain_data_polled()
    shiny::validate(shiny::need(!is.null(chain),
                                paste0("No data from the Gnosis chain yet (", chain_cache$last_error, "). Retrying every minute.")))
    shiny::validate(shiny::need(isTRUE(input$storageRadius %in% 1:16), "Enter a storage radius from 1 to 16."))
    stakes <- chain_stakes(chain)
    latest <- last_reveals(chain, chain_window_days)
    reveal <- latest[match(stakes$overlay, latest$overlay), ]
    # the swarmscan dump, when there is one, says whether swarmscan sees the node at all
    dump_overlays <- swarm_data_polled()$nodes$overlay

    stakes_info <- data.frame(
      nbhood = first_n_places(overlay_to_bits(stakes$overlay), input$storageRadius),
      overlay = stakes$overlay,
      stake = round(stakes$stake_bzz, 2),
      effective_stake = round(stakes$effective_stake_bzz, 2),
      height = stakes$height,
      frozen = stakes$frozen,
      can_play = stakes$can_play,
      last_reveal = format(reveal$time, "%Y-%m-%d %H:%M", tz = "UTC"),
      last_round = reveal$round,
      matched_truth = reveal$matched_truth,
      in_swarmscan = if (is.null(dump_overlays)) NA else stakes$overlay %in% dump_overlays
    )
    stakes_info[order(stakes_info$nbhood, -stakes_info$effective_stake), ]
  },
  colnames = c("Neighbourhood", "Overlay", "Stake (BZZ)", "Effective stake (BZZ)", "Height", "Frozen", "Can play",
               "Last reveal (UTC)", "Last round", "Matched truth", "In swarmscan"),
  rownames = FALSE
  )

  # table of reachability
  output$reachability_status <- DT::renderDataTable({
    reachability_table <- 
      nodes_data_reactive() %>% 
      group_by(overlay) %>%  
      group_by(fullNode, statusSnapshot$isReachable) %>%
      summarise(count = n())
    
    # return table
    return(reachability_table)
  }, 
  colnames = c("Full node", "Reachable", "Count"),
  rownames = FALSE
  )
  
  
  # # table of node counts per nbhood
  # output$nbhood_counts <- DT::renderDataTable({
  #   browser()
  #   overlayAsFactor <- 
  #     factor( x = nodes_data_reactive()$overlay_short,
  #             levels = generate_short_overlay(radius = input$storageRadius) )
  #   return(data.frame(sort(table(overlayAsFactor, useNA = "always")) ))
  # }, colnames = c("Neighbourhood", "Count"), rownames = FALSE)
  
  # reactive function finding max storage capacity
  max_capacity_radius <- reactive({
    # an empty or zero minimum would break the comparison or never end the loop
    # (shiny:: because jsonlite also has a validate function)
    shiny::validate(shiny::need(isTRUE(input$minNodesPerNbhood >= 1), "Enter a minimum of 1 or more nodes per nbhood."))
    # radius 0 is a single nbhood holding every node; if even that is too small, no radius works
    shiny::validate(shiny::need(nrow(nodes_data_reactive()) >= input$minNodesPerNbhood,
                  "There are fewer nodes than the minimum per nbhood, so no radius has enough nodes."))

    # node count of the smallest nbhood at radius r; there are 2^r nbhoods, so if fewer
    # of them appear in the data, at least one is empty and the smallest count is 0
    smallest_nbhood_count <- function(r) {
      counts <- table(first_n_places(nodes_data_reactive()$overlay_binary, r))
      if (length(counts) < 2^r) 0 else min(counts)
    }

    # largest radius at which every nbhood has at least the required minimum number of nodes
    radius <- 0
    while (radius < 256 && smallest_nbhood_count(radius + 1) >= input$minNodesPerNbhood) {
      radius <- radius + 1
    }

    # return storage radius with required minimum number of nodes
    return(storageRadius = radius)
  })
  
  # return max radius
  output$max_radius <- renderText({
    format_number(max_capacity_radius())
  })
  
  
  # return max capacity as text
  output$max_capacity <- renderText({
    max_capacity <- 2 ^ 22 * 4096 * (2 ^ max_capacity_radius()) / (1024 * 1024 * 1024 * 1024)
    paste(format_number(max_capacity), "TiB")
  })
   
  
  #############
  # PREPARE TEXT OUTPUT
  #############
  
  # info on total and unreachable nodes
  output$nodes_count <- renderText({
    # swarmscan's own counts; if it leaves them out, count the nodes table instead
    nodes <- swarm_data()$nodes
    total <- swarm_data()$counts$count
    unreachable <- swarm_data()$counts$unreachableCount
    if (is.null(total)) total <- nrow(nodes)
    if (is.null(unreachable)) unreachable <- sum(nodes[["unreachable"]] %in% TRUE)
    sprintf("Total nodes: %s. Unreachable nodes: %s.", format_number(total), format_number(unreachable))
  })
  
  output$explainer_text_1 <- renderText({
    paste("Set the neighbourhood size with Storage radius in the sidebar. Grey : all nodes in neighbourhood; yellow : nodes reporting an error, including errors swarmscan got when fetching the node's status (could be benign); red : unreachable nodes, drawn over yellow (could be benign). Dashed line: average nodes per neighbourhood, counting empty ones. A neighbourhood without nodes shows as a gap.")
    
  })
  
  ##############
  # Calculate amount of storage on Swarm
  ##############
  output$storage_taken <- renderText({
    
    # Save reserve within radius and radius to data frame
    nodes_data <- swarm_data()$nodes
    storage_data <- data.frame(
      reserveWithinRadius = nodes_data$statusSnapshot$reserveSizeWithinRadius,
      storageradius = nodes_data$statusSnapshot$storageRadius)
    
    # Remove empty lines (NA or 0)
    tmp <- storage_data[!(is.na(storage_data$reserveWithinRadius) | 
                            is.na(storage_data$storageradius)), ]
    clean_storage_data <- tmp[!(tmp$reserveWithinRadius == 0 | 
                                  tmp$storageradius == 0), ]
    
    # Sum up all the storage data and take the average
    bytesStored <- (clean_storage_data$reserveWithinRadius * 4096) * 
      (2 ^ clean_storage_data$storageradius)
    
    # Take median value
    shiny::validate(shiny::need(length(bytesStored) > 0,
                                "No node reports its reserve size, so the stored data cannot be estimated."))
    medianBytesStored <- median(bytesStored)
    
    # Convert to TiB (2^40 bytes)
    medianTiBStored <- medianBytesStored / (1024 * 1024 * 1024 * 1024)
    
    # return value
    paste(format_number(medianTiBStored), "TiB")
  })
}

########################################
# Run the application 
shinyApp(ui = ui, server = server)
