#load packages
#for shiny app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(DT)
#for network visualisation and clustering
library(visNetwork)
library(RColorBrewer)
library(igraph)
library(fdapace)
library(fda)
#for plotting
library(ggplot2)
library(plotly)
library(reshape)
library(gridExtra)
library(tidyverse)
#for outlier detection
library(mrfDepth)
library(MASS)
library(POT)

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/RShiny/Detecting_Network_Outliers_Online_1day")
#load functions
source("depth.R")
source("correlation_matrix_function.R")
source("depth_threshold.R")
source("merge_differences.R")
source("gpd_probs.R")
source("col_choice.R")
source("invert_graph.R")
source("mst_clustering_threshold.R")
source("line_clustering.R")
#load network structure data
load("nodes.RData")
load("edges.RData")
#load graph info
leg_names <- c("A-B_blue", "B-C_blue", "C-D_blue", "D-E_blue", "F-B_red", "B-C_red", "C-G_red", "G-H_red")
corr_matrix <- correlation_matrix_function(leg_names)
colnames(corr_matrix) <- leg_names
rownames(corr_matrix) <- leg_names
connections <- c("F-B_red--B-C_blue", "B-C_red--C-D_blue")

today_date <- as.Date("2019-07-25", origin="1970-01-01")

#processing function
#forecasts for online version
################################################## UI ###########################################################
ui <- dashboardPage(
  dashboardHeader(title = ""),
  
  dashboardSidebar(
    h3(textOutput("date_text"), align = "center"),
      #choose level of clustering on the network
      radioButtons("rb", "Display legs by:",
               c("Network" = "Network",
                 "Line" = "Line",
                 "Cluster" = "Cluster",
                 "Leg" = "Leg")),
      #if choose clustering, allow a choice of threshold
      uiOutput("threshold_check"),
      #outlier threshold
      sliderInput("perc_out", "Show outliers with probability higher than:",
                min = 0, max = 1, value = 0),
      #max number of outliers
    sliderInput("max_outs", "Maximum number of outliers:",
                min = 1, max = 50, value = 10)
    ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    fluidRow(
      #visualise network
      box(title = "Network", height=600, status = "primary", width = 6, visNetworkOutput("network"), textOutput("txt")),
      #plot outliers
      uiOutput("tabUI")
    ),
    box(
      title = "Outliers", width=NULL, height=400, status = "primary", div(style = 'height:330px; overflow-y: scroll', checkboxInput("upcoming", label="Show only upcoming departures"), DT::dataTableOutput('view_data'))
    )
  )
)

################################################## SERVER ####################################################

server <- function(input, output, session) {
  #choose correlation threshold
  output$threshold_check = renderUI({
    if (input$rb == "Cluster") {
      sliderInput("threshold", "Correlation Threshold:", min = 0, max = 1, value = 0.7)
    }
  })
  
  clusters_corr <- reactive({
    if (input$rb == "Cluster"){
      clusters_corr <- mst_clustering_threshold(corr_matrix=corr_matrix, leg_names=leg_names, connections=connections, corr_threshold=input$threshold)$cluster_list
    }  
  })
  
  output$network <- renderVisNetwork({
    #calculate clusters if necessary
    if (input$rb == "Cluster") {
      edges$cluster <- as.numeric(unlist(sapply(1:length(clusters_corr()), function(x) rep(x, length(clusters_corr()[[x]])))))
    }
    #choose colours
    edges$color <- col_choice(type=input$rb, edges)
    edges$id <- edges$leg
    #visualise network
    visNetwork(nodes, edges) %>% 
      visEdges(smooth = list(enabled = T, type = 'dynamic'), arrows = 'to') %>%
      visLayout(randomSeed = 16) %>%
      visOptions(highlightNearest = F) %>%
      visPhysics(solver = "barnesHut", barnesHut = list(springConstant = 0.002)) %>%
      visEvents(select = "function(data) {
                Shiny.onInputChange('current_edges_selection', data.edges);
                ;}")
    
  })
  
  leg_selected <- reactive({
      input$current_edges_selection[1]
  })
  
  all_legs_selected <- reactive({
    if (!is.null(leg_selected())){
      if (input$rb == "Leg"){
        e <- leg_selected()
      } 
      if (input$rb == "Cluster"){
        e <- clusters_corr()[[which(lapply(clusters_corr(), function(x) leg_selected() %in% x) == TRUE)]]
      }
      if (input$rb == "Line"){
        line_clusters <- line_clustering(leg_names)
        e <- line_clusters[[which(lapply(line_clusters, function(x) leg_selected() %in% x) == TRUE)]]
      } 
      if (input$rb == "Network"){
        e <- edges$leg
      } 
    e
    }
  }) 
  
  leg_selected_name <- reactive({
    if (!is.null(leg_selected())){
      paste(edges$from[which(as.vector(edges$leg) == leg_selected())], "to", edges$to[which(as.vector(edges$leg) == leg_selected())])
    }
  })
  

  leg_selected_data <- reactive({
    d <- readRDS(paste(input$leg_plot,"_extrap_data_20190725.rds", sep=""))
    d
  })
  
  leg_selected_status <- reactive({
    dcol <- readRDS(paste(input$leg_plot,"_which_extrap_20190725.rds", sep=""))
    dcol
  })
  
  leg_zn <- reactive({
    if (!is.null(leg_selected())){
      zn <- lapply(all_legs_selected(), function(x) readRDS(paste(x,"_diffs_20190725.rds", sep="")))
      names(zn) <- all_legs_selected()
      zn
    }
  }) 
  
  clusters_zn <- reactive({
    if (!is.null(leg_selected())){
        b <- merge_differences(leg_zn())
        colnames(b) <- all_legs_selected()
        b
    }
  })
  
  probs <- reactive({
    if (!is.null(leg_selected())){
      gpd_probs(clusters_zn())
    }
  })
  
  outliers <- reactive({
    if (!is.null(leg_selected())){
      if (input$upcoming == FALSE){
        outliers <- which(probs()[,2] > input$perc_out)[1:min(input$max_outs, length(which(probs()[,2] > input$perc_out)))]
      }
      if (input$upcoming == TRUE){
        outliers <- which(probs()[,2] > input$perc_out & as.Date(as.numeric(probs()[,1]),origin="1970-01-01") > today_date)[1:min(input$max_outs, length(which(probs()[,2] > input$perc_out)))]
      }
      outliers
    }
  })
  
  probs_table <- reactive({
    if (!is.null(leg_selected())){
      tab <- probs()[outliers(),]
      tab2 <- matrix(tab, ncol=3, nrow=length(outliers())) 
      tab2[,2] <- round(as.numeric(tab2[,2]), 3)*100
      tab3 <- data.frame(as.Date(as.numeric(tab2[,1]),origin="1970-01-01"), tab2[,2:3], sapply(as.Date(as.numeric(tab2[,1]),origin="1970-01-01"), function(x) c("Bookings Open", "Departed")[as.numeric(x <= today_date)+1]))
      colnames(tab3) <-  c("Dep. Date", "Outlier Prob (%)", "Leg(s) Affected", "Status")
      tab3
    }
  })
  
  output$view_data<-DT::renderDataTable({
    DT::datatable(probs_table(), options = list(dom = 'ft'))
  })
  

   output$tabSelected <- renderText({
    paste("You have selected:", input$tab)
  })
 
   output$date_text <- renderText({
     paste("Date:", today_date)
   })
  
  output$txt <- renderText({
    paste("You have selected:", leg_selected_name())
  })
  
  output$outliers <- renderPlotly({
    if (!is.null(leg_selected())){
      if (input$extrap_outliers == "extrap"){
        df <- leg_selected_data()
        colnames(df) <- 1:18
        df <- melt(df)  
        dcol <- leg_selected_status()
        dfcol <- melt(dcol)
        df$depid <- dfcol$value
        dat <- tibble(
          time = df$X2, 
          Bookings = ceiling(df$value), 
          Departure = as.Date(df$X1,origin="1970-01-01"),
          depid = df$depid
        )
        p <- ggplot(data = dat, mapping = aes(x = rev(time), y = Bookings, group = Departure, col=depid)) +
          geom_line() + theme_light() + xlab("Booking Intervals before Departure") +
          ylab("Bookings") + scale_x_reverse() + 
          scale_colour_manual("",values=c("Extrapolated"="purple4", "Observed"=alpha(c("grey"),0.3))) +
          theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position=c(0,1),legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
        ggplotly(p, tooltip = c("Departure", "y")) %>% layout(legend = list(orientation = "h"))
      }
      else {
        df <- leg_selected_data()
        colnames(df) <- 1:18
        df <- melt(df)
        choose_colours <- rep(NA, nrow(leg_selected_data()))
        probs_selected <- which(rownames(leg_selected_data()) %in% probs_table()[,1])
        choose_colours[probs_selected] <- na.omit(as.numeric(probs_table()[,2])) 
        df$depid <- choose_colours
        dat <- tibble(
          time = df$X2,
          Bookings = ceiling(df$value),
          Departure = as.Date(df$X1,origin="1970-01-01"),
          depid = df$depid
        )
        p <- ggplot(data = dat, mapping = aes(x = rev(time), y = Bookings, group = Departure, col=depid)) +
          geom_line() + theme_light() + xlab("Booking Intervals before Departure") +
          ylab("Bookings") + scale_x_reverse() +
          scale_colour_gradient2(low="yellow", mid="orange", high="red", midpoint=50, limits = c(0.1, 100), na.value = alpha(c("grey"),0.3)) +
          theme(axis.text=element_text(size=14),axis.title=element_text(size=14),legend.text=element_text(size=14),plot.background = element_rect(fill = "transparent", color = NA),legend.background = element_rect(color = NA,fill="transparent"),legend.box.background = element_rect(fill = "transparent",color=NA),legend.position="none",legend.justification=c(0,1),legend.title=element_blank(),legend.key = element_blank())
        ggplotly(p, tooltip = c("Departure", "y"))
      }
    }
  })
  
  output$tabUI <- renderUI({
    if (length(all_legs_selected()) >= 1){
      box(title="Bookings", width = 6, height=600, status = "primary",
          selectInput("leg_plot", label="View bookings for leg:", choices=all_legs_selected()),
          radioButtons("extrap_outliers", label="Highlight:", choices=c("Extrapolation"="extrap", "Outliers"="plot_outs"), inline=TRUE),
          plotlyOutput("outliers")
      )
    }
    else{
      box(title="Bookings", width = 6, height=600, status = "primary")
    }
  })
  
  
}

shinyApp(ui, server)



#setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 2 - Overlapping/R/Networks/RShiny")