# mutual_tab.R

# UI for the Mutual Fund tab
mutual_tab_ui = function(data) {
  tabPanel("Mutual Funds",
           sidebarLayout(
             sidebarPanel(
               selectInput("Mutual", 'Select Mutual Fund', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               dateRangeInput("dateRangeMutual", "Select Date Range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date), 
                              startview = 'decade', format = "mm/dd/yyyy"),
               selectInput("metricMutual", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeMutual", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                            selected = "price"),
               actionButton("resetMutual", "Reset Filters")
             ),
             mainPanel(
               plotlyOutput("mutualPlot"),
               p("Click on a company's line in the chart to get more information about that company."),
               uiOutput("summaryMutual")
             )
           )
  )
}

# Server logic for the Mutual Fund tab.
mutual_tab_server = function(data, input, output, session) {
  output$mutualPlot = renderPlotly({
    if(length(input$Mutual) == 0){
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
      min_date = input$dateRangeMutual[1]
      max_date = input$dateRangeMutual[2]
      filtered_data = filterData(data, input$Mutual, min_date, max_date)
      
      generatePlot(filtered_data, input$plotTypeMutual, min_date, max_date, input$metricMutual, "mutualPlot")
    }
  })
  # Reset Filters for Mutual Page
  observeEvent(input$resetMutual, {
    updateSelectInput(session, "Mutual", selected = NULL)
    updateDateRangeInput(session, "dateRangeMutual", 
                         start = min(data$Date), end = max(data$Date))
    
    updateDateRangeInput(session, "dateRangeMutual", 
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    
    updateSelectInput(session, "Mutual", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  # Create a reactive expression for the company summary
  eventMutual = reactive({
    event_data("plotly_click", source = "mutualPlot")
  })
  
  output$summaryMutual = renderUI({
    event = eventMutual()
    createInvestmentSummary(event, input, data, length(input$Mutual))
  })
  }

