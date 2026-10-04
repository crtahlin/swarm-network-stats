#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### load libraries
library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(stringi)
library(ggplot2)
library(DT)
library(leaflet)
library(forstringr)
library(SwarmR)
library(DescTools)
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
  # calculate binary overlay address and add it to data
  nodes_data$overlay_binary <- sapply(nodes_data$overlay, FUN = hexadecimal2binary)
  # if unreachable column does not exist, fill it with NAs (to avoid corner case)
  if (is.null(nodes_data$unreachable)) {nodes_data$unreachable <- NA}

  # swarmscan gives nodes it failed to geolocate the placeholder location 0,0 with no country,
  # which would put them in the ocean off West Africa; treat it as missing
  country <- if (is.null(nodes_data$location$country)) rep(NA, nrow(nodes_data)) else nodes_data$location$country
  placeholder <- !is.na(nodes_data$location$latitude) & nodes_data$location$latitude == 0 &
    nodes_data$location$longitude == 0 & is.na(country)
  nodes_data$location$latitude[placeholder] <- NA
  nodes_data$location$longitude[placeholder] <- NA
  nodes_data$location_source <- ifelse(is.na(nodes_data$location$latitude), NA, "swarmscan")

  # the same public IP is often located for one node and not for another, so borrow the location
  # from a located node that shares a public IP; private addresses (e.g. Docker's 172.17.0.1) are
  # shared by unrelated nodes and are never used. If candidates differ, the most common location wins
  node_ips <- lapply(nodes_data$underlays, public_ips)
  located <- which(!is.na(nodes_data$location$latitude))
  ip_rows <- data.frame(ip = unlist(node_ips[located]), row = rep(located, lengths(node_ips[located])))
  location_key <- do.call(paste, c(nodes_data$location, sep = "|"))
  for (i in which(is.na(nodes_data$location$latitude))) {
    candidates <- ip_rows$row[ip_rows$ip %in% node_ips[[i]]]
    if (length(candidates) == 0) next
    keys <- location_key[candidates]
    best <- candidates[match(names(which.max(table(keys))), keys)]
    nodes_data$location[i, ] <- nodes_data$location[best, ]
    nodes_data$location_source[i] <- "same IP"
  }

  nodes_data
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


### APPLICATION
### UI part
ui <- 
  page_navbar(
    # the figures under each heading stand out from it
    header = tags$style("#storage_taken, #max_radius, #max_capacity, #nodes_count { font-weight: bold; margin-bottom: 1em; }"),
    # settings part
    sidebar = sidebar("Settings",
                      numericInput("storageRadius", "Storage radius",
                                   value = 11, min = 1, max = 16, step = 1),
                      numericInput("minNodesPerNbhood", "Minimum nodes per nbhood",
                                   value = 2, min = 1, max = 8),
                      checkboxInput("onlyFullNodes", "Show only full nodes",
                                    value = TRUE),
                      textOutput("data_status")),
    # panels part
    ###
    nav_panel("Map", 
              "Map of nodes",
              textOutput("map_note"),
              leafletOutput("leafletMap", height = "800px")),
    ###
    nav_panel("Data", 
              "Estimated total amount of stored data in TiB (2^40 bytes)",
              textOutput("storage_taken"),
              "Maximum storage radius with set minimum required nodes per neighbourhood",
              textOutput("max_radius"),
              "Maximum capacity of storage with set minimum required nodes per neighbourhood in TiB (2^40 bytes)",
              textOutput("max_capacity")
              ), 
    
    ### 
    nav_panel("Reachability",
              "Reachability of nodes",
              DT::dataTableOutput("reachability_status")),
    
    ###
    nav_panel("Nbhood plot", 
              "Number of nodes",
              textOutput("nodes_count"),
              "Count of nodes per neighbourhood",
              plotOutput("distPlot", height = "800px"),
              br(),
              textOutput("explainer_text_1")),

    # ###
    # nav_panel("Nbhood counts",
    #           "Nodes per neighborhood, sorted by least numerous based on selected radius",
    #           DT::dataTableOutput("nbhood_counts")),
 
    ###
    nav_panel("Nbhoods stats",
              "Neighborhoods statistics",
              p("A node counts as an error node when swarmscan reports an error contacting it, or an error ",
                "fetching its status (the Error and Status error columns on the Nodes info tab). Most status ",
                "errors are \"peer not found\", meaning swarmscan could not fetch the status, and can be benign."),
              DT::dataTableOutput("stats_table")),
    
    ###
    nav_panel("Nodes info",
              "Individual nodes statistics",
              DT::dataTableOutput("nodes_data"))
  )


# Define server logic required to draw a histogram
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

  # data freshness, shown in the sidebar; re-read every minute so a failed refresh shows up
  output$data_status <- renderText({
    invalidateLater(60 * 1000)
    swarm_data_polled()
    data_status_text()
  })

  # based on the storage radius set, take the first n chars of the overlay address and add to the data
  nodes_data_reactive <- reactive({
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
    
    # generate plot
    plot <-
      ggplot(data = nodes_data, aes(x = overlay_short)) +
      geom_bar() +
      geom_hline(yintercept = mean(table(nodes_data$overlay_short))) +
      # yellow: nodes with an error in either error field (same rule as the Nbhoods stats table),
      # plus unreachable nodes, so the yellow visible above red is always reachable nodes with an error;
      # red: unreachable nodes, drawn on top. Both subsets come from the plotted data, so rows line up
      geom_bar(data = nodes_data[nodes_data$error_logical | nodes_data$unreachable %in% TRUE, ], fill = "yellow", width = 1) +
      geom_bar(data = nodes_data[nodes_data$unreachable %in% TRUE, ], fill = "red", width = 1) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
      labs(y = "Node count", x = "Neighbourhood")
    
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
      addTiles() %>%
      # addAwesomeMarkers(lat = nodes_data$location$latitude, lng = nodes_data$location$longitude) %>%
      addMarkers(clusterOptions = markerClusterOptions(), lat = nodes_data_reactive()$location$latitude, lng = nodes_data_reactive()$location$longitude)
    
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
              levels = generate_short_overlay(radius = input$storageRadius) )

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
    # (shiny:: is needed because library(jsonlite) masks shiny's validate)
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
    paste("Select desired neighbourhood size in dropdown. Grey : all nodes in neighbourhood; yellow : nodes reporting an error, including errors swarmscan got when fetching the node's status (could be benign); red : unreachable nodes, drawn over yellow (could be benign). NOTE: If a neighbourhood has no nodes, it is not shown on graph!")
    
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
