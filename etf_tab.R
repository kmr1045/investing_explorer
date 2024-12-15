# etf_tab.R

# UI for the ETF tab
etf_tab_ui = function(data) {
  tabPanel("ETFs",
           sidebarLayout(
             sidebarPanel(
               selectInput("ETF", 'Select ETF', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               selectInput("metricETF", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeETF", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change", "Waterfall" = "waterfall"),
                            selected = "price"),
               
               # Dropdown for Predefined Date Range
               selectInput("dateRangeETF", "Select Date Range", 
                           choices = c("7 Days", "1 Month", "6 Months", "1 Year", "Year-to-Date", "5 Years", "Custom Date Range"), 
                           selected = "1 Month"),
               
               # Conditional UI for Custom Date Range
               uiOutput("customDateRangeETF"),
               
               actionButton("resetETF", "Reset Filters")
             ),
             mainPanel(
               # Show introduction text when no ETF is selected
               uiOutput("introTextETF"),
               plotlyOutput("etfPlot"),
               # Only show the "Click on an ETF's line" text when an ETF is selected
               uiOutput("clickTextETF"),
               uiOutput("summaryETF")
             )
           )
  )
}

# Server logic for the ETF tab
etf_tab_server = function(data, input, output, session) {
  
  # Show introduction text when no ETF is selected
  output$introTextETF = renderUI({
    if(length(input$ETF) == 0) {
      HTML(
        paste(
          "<div style='font-size: 16px; width: 80%; margin: 0 auto; white-space: normal; word-wrap: break-word; overflow-wrap: break-word;'>",
          
          "ETFs (Exchange-Traded Funds) are investment funds that hold a diversified portfolio of assets like stocks, bonds, commodities, or other securities. ",
          "Unlike mutual funds, ETFs trade on exchanges just like individual stocks. This tool allows you to explore their price trends over time.<br><br>",
          
          "You can track various price metrics like Open, Close, Low, High, and Adjusted prices. ",
          "Choose a specific date range to analyze price movements over different periods—whether you're interested in short-term fluctuations or long-term trends.<br><br>",
          
          "Select one or more ETFs, adjust the filters, and gain insights into how these investment funds perform. ",
          "The data presented here is from ", 
          format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          " to ", 
          format(max(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          ".<br><br>",
          
          "Tip: Use the filters to narrow down your ETF selection and explore different price metrics. You can reset your filters anytime to start fresh.<br><br>",
          
          "Investing in ETFs can help diversify your portfolio while allowing exposure to various sectors or markets. Be sure to consider your investment objectives and risk tolerance.<br><br>",
          
          "</div>"
        )
      )
    } else {
      return(NULL)  # Hide the text when an ETF is selected
    }
  })
  
  # Show the "Click on an ETF's line" text only when an ETF is selected
  output$clickTextETF = renderUI({
    if(length(input$ETF) > 0) {
      p("Click on an ETF's line in the chart to get more information about that ETF.")
    } else {
      return(NULL)  # Hide the text when no ETF is selected
    }
  })
  
  # Show custom date range picker when "Custom" is selected
  output$customDateRangeETF = renderUI({
    if (input$dateRangeETF == "Custom Date Range") {
      dateRangeInput("customDateRangeETF", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date),
                     format = "mm/dd/yyyy", startview = 'decade')
    } else {
      return(NULL)  # Hide the custom date range input
    }
  })
  
  # Output plot based on selected ETF and date range
  output$etfPlot = renderPlotly({
    if(length(input$ETF) == 0){
      return(NULL)  # Do not render plot when no ETF is selected
    } else {
      # Use the helper function to calculate date range for predefined ranges
      if (input$dateRangeETF != "Custom Date Range") {
        date_range = calculate_date_range(input$dateRangeETF, data)
        min_date = date_range[1]
        max_date = date_range[2]
      } else {
        min_date = input$customDateRangeETF[1]
        max_date = input$customDateRangeETF[2]
      }
      
      # Filter the data based on the selected date range
      filtered_data = filterData(data, input$ETF, min_date, max_date)
      
      # Generate the plot for selected ETFs
      generatePlot(filtered_data, input$plotTypeETF, min_date, max_date, input$metricETF, "etfPlot")
    }
  })
  
  # Reset Filters for ETF Page
  observeEvent(input$resetETF, {
    updateSelectInput(session, "ETF", selected = NULL)
    updateSelectInput(session, "dateRangeETF", selected = "1 Month")
    updateDateRangeInput(session, "customDateRangeETF", start = min(data$Date), end = max(data$Date))
    updateSelectInput(session, "ETF", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  # Create a reactive expression for the ETF summary
  eventETF = reactive({
    event_data("plotly_click", source = "etfPlot")
  })
  
  # Output summary when an ETF is clicked
  output$summaryETF = renderUI({
    event = eventETF()
    if (!is.null(event)) {
      createInvestmentSummary(event, input, data, length(input$ETF))
    } else {
      return(NULL)  # Hide the summary if no ETF is selected
    }
  })
}
