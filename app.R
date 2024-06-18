#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

################## load needed libraries
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


################# load data from swarmscan.io
# load data from swarmscan - nodes
swarmscan_data <- load_swarmscan_data()
########### COUNTRY DATA
# load data from swarmscan - countries
swarmscan_countries <- load_swarmscan_countries()

################# prepare data
# extract data about nodes
nodes_data <- swarmscan_data$nodes
# calculate binary overlay address and add it to data
nodes_data$overlay_binary <- sapply(nodes_data$overlay, FUN = hexadecimal2binary)
# if unreachable column does not exist, fill it with NAs (to avoid corner case)
if (is.null(nodes_data$unreachable)) {nodes_data$unreachable <- NA} 


################## APPLICATION

ui <- 
  page_navbar(
  sidebar = sidebar("Sidebar"),
  nav_panel("Page 1", "Page 1 content"),
  nav_panel("Page 2", "Page 2 content")
)


# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Swarm nodes stats, ver 0.36"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#       # Show a plot of the generated distribution
#       mainPanel(
#         leafletOutput("leafletMap", height = "800px"),
#         verbatimTextOutput("nodes_count"),
#         plotOutput("distPlot"),
#         verbatimTextOutput("explainer_text_1"),
#         DT::dataTableOutput("reachability_status"),
#         DT::dataTableOutput("stats_table"),
#         DT::dataTableOutput("nodes_data"),
#         verbatimTextOutput("nbhood_counts"),
#         # DT::dataTableOutput("reserveSizes"),
# 
#         width = 10
#       ),
#       sidebarPanel(
#             # checkboxInput("show_nodes_w_errors", "Show nodes with any errors", value = TRUE),
#             # checkboxInput("show_unreachable_nodes", "Show unreachable nodes", value = TRUE)
#         numericInput("storageRadius", "Storage radius", value = 10, min = 1, max = 16, step = 1),
#         checkboxInput("onlyFullNodes", "Show only full nodes", value = TRUE),
#         width = 2
#          )
#       )
#     )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###############
  # PREPARE THE DATA (reactive function)
  ###############
  # based on the storage radius set, take the first n chars of the overlay address and add to the data
  nodes_data_reactive <- reactive(
    {#browser()
      nodes_data$overlay_short <- first_n_places(nodes_data$overlay_binary, input$storageRadius)
      nodes_data$overlay_short_next <- str_right( first_n_places(nodes_data$overlay_binary, (input$storageRadius + 1)), 1 )
      nodes_data$error_logical <- nchar(nodes_data$error)>0 # set TRUE if string found in error field
      # print(input$storageRadius)
      if (input$onlyFullNodes) {
        nodes_data <- nodes_data[!is.na(nodes_data$fullNode), ]
        nodes_data <- nodes_data[nodes_data$fullNode, ]
      }
      # browser()
      return(nodes_data)
      })
  
  ###############
  # PREPARE THE PLOTS
  ###############
  
  # barchart nodes per neighbourhood
    output$distPlot <- renderPlot({
      # output the distribution by neighborhoods
      # browser()
      
      # remove data if not longitude and latitude is present
      nodes_data <- nodes_data_reactive()[-(is.na(nodes_data$location$latitude) | is.na(nodes_data$location$latitude)), ]
      
      plot <-
        ggplot(data = nodes_data, aes(x = overlay_short)) +
        geom_bar() +
        geom_hline(yintercept = mean(table(nodes_data$overlay_short))) +
        geom_bar(data = nodes_data_reactive()[ !is.na(nodes_data$error) , ], fill = "yellow", width = 1) +
        geom_bar(data = nodes_data_reactive()[ !is.na(nodes_data$unreachable) , ], fill = "red", width = 1) +
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) 

      # return plot
      return(plot)
      })
    
    # Leaflet map
    output$leafletMap <- renderLeaflet({
      plot <- 
        leaflet() %>% 
        addTiles() %>%
        # addAwesomeMarkers(lat = nodes_data$location$latitude, lng = nodes_data$location$longitude) %>%
        addMarkers(clusterOptions = markerClusterOptions(), lat = nodes_data_reactive()$location$latitude, lng = nodes_data_reactive()$location$longitude)
    return(plot)
      })
    
  ##############
  # PREPARE THE TABLES
  #############
    
    # table of counts per bins
    output$stats_table <- DT::renderDataTable({
      # browser()
      # TODO: sometimes there are no unreachable nodes and the app crashes bcs of that; do a more robust merge of data 
      error_stats_per_nbhood <- 
        cbind( 
          Freq = table(Freq=nodes_data_reactive()$overlay_short),
          # Freq.unreachable = table(nodes_data_reactive()$overlay_short, Freq.unreachable=nodes_data_reactive()$unreachable)[,1],
          Freq.error = table(nodes_data_reactive()$overlay_short, Freq.error=nodes_data_reactive()$error_logical)[, 1]
        )
      error_stats_per_nbhood <- as.data.frame(error_stats_per_nbhood)
      error_stats_per_nbhood$Percent.error <- (error_stats_per_nbhood$Freq.error / error_stats_per_nbhood$Freq)*100
      # error_stats_per_nbhood$Percent.unreachable <- (error_stats_per_nbhood$Freq.unreachable / error_stats_per_nbhood$Freq)*100
      return(error_stats_per_nbhood)
      
      # as.data.frame(table(nodes_data_reactive()$overlay_short))
    } #, server = TRUE, extensions = c("Buttons"), 
    #options = list(dom = 'Bfrtip',
    #               buttons = c('copy', 'csv', 'excel'))
    )
    
    # table of all the data
    output$nodes_data <- DT::renderDataTable({
      # browser()
      nodes_info <- nodes_data_reactive()[, 
                                          c("overlay_short",
                                            "overlay_short_next",
                                            "overlay",
                                            "error",
                                            "unreachable",
                                            "fullNode"
                                            )]
      # add info about country from location list
      nodes_info$location <- nodes_data_reactive()$location$country
      
      # return table with info
      return(nodes_info)
    } #, server = TRUE, extensions = c("Buttons"), 
    #options = list(dom = 'Bfrtip',
    #               buttons = c('copy', 'csv', 'excel'))
    )
    
    # table of reachability
    output$reachability_status <- DT::renderDataTable({
      # browser()
      
      reachability_table <- 
        nodes_data_reactive() %>% 
        # select(overlay, lastCheckTimeUTC) %>% 
        group_by(overlay) %>% # slice_max(lastCheckTimeUTC) %>% slice_head(n=1) %>% 
        group_by(fullNode, statusSnapshot$isReachable) %>%  # , statusSnapshot.beeMode
        summarise(count = n())
      
      return(reachability_table)
    })
    
    
    # output$reserveSizes <- DT::renderDataTable({
    #   # browser()
    #   # TODO the reserveSize is no longer the proper endpoint/data for calculating storage taken
    #   # some other endpoint will have to be used, once it is documented
    #   storageRadiuses <- nodes_data_reactive()$statusSnapshot$storageRadius
    #   reserveSizes <- nodes_data_reactive()$statusSnapshot$reserveSize
    #   
    #   mostCommonRadius <- Mode(storageRadiuses, na.rm = TRUE)
    #   reservesForMostCommonRadius <- reserveSizes[which(storageRadiuses == mostCommonRadius)]
    #   avgReserveSize <- mean(reservesForMostCommonRadius)
    #   storageTakenTB <- (2 ^ mostCommonRadius) * (avgReserveSize * 4096)/(1024*1024*1024*1024) # in TBtes
    #   storageUntilSplitTB <- (2 ^ mostCommonRadius) * ((2 ^ 22 - avgReserveSize)  * 4096)/(1024*1024*1024*1024) # in TBytes
    #   
    #   result <- data.frame(mostCommonRadius, avgReserveSize, storageTakenTB, storageUntilSplitTB)
    #   
    #   return(result)
    # })
    
    # table of node counts per nbhood
    output$nbhood_counts <- renderPrint({
      overlayAsFactor <- factor(x = nodes_data_reactive()$overlay_short, levels = generate_short_overlay(radius = input$storageRadius) )
      return(sort(table(overlayAsFactor)))
    })
    
    
    #############
    # PREPARE TEXT OUTPUT
    #############
    
    # info on total and unreachable nodes
    output$nodes_count <- renderPrint({
      print(paste("Total nodes:", swarmscan_data$count))
      print(paste("Unreachable nodes:", swarmscan_data$unreachableCount))
    })
    
    output$explainer_text_1 <- renderPrint({
      print("Select desired neighbourhood size in dropdown. Grey : all nodes in neighbourhood; yellow : nodes reporting error (could be benign); red : unreachable nodes (could be benign). NOTE: If a neighbourhood has no nodes, it is not shown on graph!")
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
