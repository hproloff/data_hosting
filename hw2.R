library(shiny)
library(ggplot2)
library(dplyr)
golf_data <- read.csv("https://raw.githubusercontent.com/hproloff/data_hosting/main/lpcc_golf_results.csv")
golf_data$day <- as.Date(golf_data$day, format="%m-%d-%y")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #f5f5dc;
      }
      .sidebar {  
        background-color: #faf0e6; 
        padding: 10px;
        border-radius: 5px;
      }
      .shiny-plot-output {
        background-color: #daf1d0;
      }
    "))
  ),
  titlePanel("Golf Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("dateSlider",
                  "Date Range:",
                  min = min(golf_data$day),
                  max = max(golf_data$day),
                  value = c(min(golf_data$day), max(golf_data$day)),
                  timeFormat = "%B %d, %Y",
                  step = 1),
      
      actionButton("selectAllPlayers", "Select/Deselect All Players"),
      checkboxGroupInput("selectedPlayers", "Choose Players:",
                         choices = unique(golf_data$person),
                         selected = unique(golf_data$person)),
      actionButton("selectAllHoles", "Select/Deselect All Holes"),
      checkboxGroupInput("selectedHoles", "Choose Holes:",
                         choices = sort(unique(golf_data$hole)),
                         selected = sort(unique(golf_data$hole))[1:3]),
      helpText("Differentiate players by colors for histograms"),
      checkboxInput("colorToggle", "Player Colors", value = FALSE),
      
      helpText("Rearrange the holes from lowest to highest average strokes."),
      checkboxInput("averageOrdering", "Order by Avg Strokes", value = FALSE)
    ),
    mainPanel(
      helpText("Player's progress over time."),
      plotOutput("progressPlot"), # Line graph for player's progress
      helpText("Histograms of strokes per hole."),
      uiOutput("dynamicPlotOutput")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$selectAllPlayers, {
    currentSelection <- input$selectedPlayers
    isSelectedAll <- length(currentSelection) == length(unique(golf_data$person))
    updateCheckboxGroupInput(session, "selectedPlayers", selected = if(isSelectedAll) character(0) else unique(golf_data$person))
  })
  
  observeEvent(input$selectAllHoles, {
    currentSelection <- input$selectedHoles
    isSelectedAll <- length(currentSelection) == length(unique(golf_data$hole))
    updateCheckboxGroupInput(session, "selectedHoles", selected = if(isSelectedAll) character(0) else sort(unique(golf_data$hole)))
  })
  
  # Common filtered data reactive for both plots
  filtered_data <- reactive({
    golf_data %>%
      filter(day >= input$dateSlider[1], day <= input$dateSlider[2],
             person %in% input$selectedPlayers, hole %in% input$selectedHoles)
  })
  
  # Line graph for player's progress
  output$progressPlot <- renderPlot({
    data_to_plot <- filtered_data() %>%
      group_by(day, person) %>%
      summarise(average_strokes = mean(strokes), .groups = 'drop') %>%
      arrange(day)
    
    ggplot(data_to_plot, aes(x = day, y = average_strokes, group = person, color = person)) +
      geom_line() + 
      geom_point() + 
      labs(title = "Player's Progress Over Time",
           x = "Date",
           y = "Average Strokes per Round") +
      theme_minimal() +
      theme(legend.title = element_blank(), plot.background = element_rect(fill = "#daf1d0", color = NA)) 
  })
  
  # Dynamic plot output for histograms
  output$dynamicPlotOutput <- renderUI({
    selectedHoles <- input$selectedHoles
    numHoles <- length(selectedHoles)
    numRows <- ceiling(numHoles / 3)  
    
    plotHeight <- 200 * numRows
    
    plotOutput("histogramsPlot", width = "100%", height = paste0(plotHeight, "px"))
  })
  
  # Histograms plot
  output$histogramsPlot <- renderPlot({
    filtered_data <- filtered_data() %>%
      filter(strokes >= 2, strokes <= 6)
    
    # Y axis upper limit calculation
    max_bins_count <- max(filtered_data %>% 
                            group_by(hole, strokes) %>% 
                            summarise(count = n(), .groups = 'drop') %>%
                            ungroup() %>%
                            summarise(max_count = max(count)) %>%
                            pull(max_count), na.rm = TRUE)
    
    avg_strokes_data <- filtered_data %>%
      group_by(hole) %>%
      summarise(avg_strokes = mean(strokes), .groups = 'drop')
    
    if(input$averageOrdering) {
      avg_strokes_data <- avg_strokes_data %>% arrange(avg_strokes)
    } else {
      avg_strokes_data <- avg_strokes_data %>% arrange(as.numeric(hole))
    }
    
    filtered_data$hole <- factor(filtered_data$hole, levels = avg_strokes_data$hole)
    
    vibrant_palette <- colorRampPalette(c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"))
    
    p <- ggplot(filtered_data, aes(x = strokes, group = person)) +
      geom_histogram(aes(fill = if(input$colorToggle) as.factor(person) else "All"), position = "stack", binwidth = 1) +
      scale_fill_manual(values = if(input$colorToggle) vibrant_palette(length(unique(filtered_data$person))) else "darkgreen",
                        name = if(input$colorToggle) "Players" else NULL) +
      facet_wrap(~hole, scales = "free", ncol = 3, labeller = as_labeller(function(hole) paste("Hole", hole, ":", round(avg_strokes_data$avg_strokes[as.character(avg_strokes_data$hole) == hole], 2), "avg strokes"))) +
      labs(title = "Strokes Distribution per Hole", x = "Strokes", y = "Frequency") +
      theme_minimal() +
      theme(legend.position = if(input$colorToggle) "right" else "none") +
      coord_cartesian(xlim = c(1, 7), ylim = c(0, max_bins_count + 1)) 
    
    print(p + theme(plot.background = element_rect(fill = "#daf1d0", color = NA)))
  })
}

shinyApp(ui = ui, server = server)
