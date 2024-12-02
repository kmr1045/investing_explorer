# index_tab.R

# UI for the Index Fund tab
index_tab_ui = function(data) {
  tabPanel("Index Funds",
           sidebarLayout(
             sidebarPanel(
               selectInput("Index", 'Select Index Fund', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               dateRangeInput("dateRangeIndex", "Select Date Range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date), 
                              startview = 'decade', format = "mm/dd/yyyy"),
               selectInput("metricIndex", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeIndex", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                            selected = "price"),
               actionButton("resetIndex", "Reset Filters")
             ),
             mainPanel(
               plotlyOutput("indexPlot"),
               p("Click on a company's line in the chart to get more information about that company."),
               uiOutput("summaryIndex")
             )
           )
  )
}

index_tab_server = function(data, input, output, session) {
  output$indexPlot = renderPlotly({
    if(length(input$Index) == 0){
      plot_ly() %>%
        layout(
          title = list(
            text = paste(
              "<span style='font-size: 18px;'>Welcome to the Investing Explorer!\n\n</span><br>",
              "<span style='font-size: 14px;'>Select one or more companies to start exploring.\n",
              "Filter companies by sector and industry to refine your options.\n",
              "Explore various price metrics and set a date range to analyze trends.\n",
              "View actual stock prices or the percentage change over time.\n",
              "Data covers ", 
              format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
              " to ", 
              format(max(data$Date, na.rm = TRUE), "%B %d, %Y"),
              ".\n\n", 
              "Tip: Use the reset button anytime to clear your selections.</span>", sep = ""
            )
          ),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    } else {
      min_date = input$dateRangeIndex[1]
      max_date = input$dateRangeIndex[2]
      filtered_data = filterData(data, input$Index, min_date, max_date)
      
      generatePlot(filtered_data, input$plotTypeIndex, min_date, max_date, input$metricIndex, "indexPlot")
    }
  })
  # Reset Filters for Index Fund Page
  observeEvent(input$resetIndex, {
    updateSelectInput(session, "Index", selected = NULL)
    updateDateRangeInput(session, "dateRangeIndex", 
                         start = min(data$Date), end = max(data$Date))
    
    updateDateRangeInput(session, "dateRangeIndex", 
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    
    updateSelectInput(session, "Index", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  
  # Create a reactive expression for the company summary
  eventIndex = reactive({
    event_data("plotly_click", source = "indexPlot")
  })
  
  output$summaryIndex = renderUI({
    event = eventIndex()
    createInvestmentSummary(event, input, data, length(input$Index))
  })
  }
