# mutual_tab.R

# UI for the Mutual Fund tab
mutualfund_tab_ui = function(data) {
  tabPanel("Mutual Funds",
           sidebarLayout(
             sidebarPanel(
               selectInput("mutualFund", 'Select Mutual Fund', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               selectInput("metricMutual", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeMutual", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change", "Waterfall" = "waterfall"),
                            selected = "price"),
               
               # Dropdown for Predefined Date Range
               selectInput("dateRangeMutual", "Select Date Range", 
                           choices = c("7 Days", "1 Month", "6 Months", "1 Year", "Year-to-Date", "5 Years", "Custom Date Range"), 
                           selected = "1 Month"),
               
               # Conditional UI for Custom Date Range
               uiOutput("customDateRangeMutual"),
               
               actionButton("resetMutual", "Reset Filters")
             ),
             mainPanel(
               # Show introductory text when no Mutual Fund is selected
               uiOutput("introTextMutual"),
               plotlyOutput("mutualPlot"),
               # Only show "Click on a Mutual Fund's line" message when Mutual Funds are selected
               uiOutput("clickTextMutual"),
               uiOutput("summaryMutual")
             )
           )
  )
}

# Server logic for the Mutual Fund tab
mutualfund_tab_server = function(data, input, output, session) {
  
  # Show introductory text when no Mutual Fund is selected
  output$introTextMutual = renderUI({
    if(length(input$mutualFund) == 0) {
      HTML(
        paste(
          "<div style='font-size: 16px; width: 80%; margin: 0 auto; white-space: normal; word-wrap: break-word; overflow-wrap: break-word;'>",
          
          "Mutual funds are investment vehicles that pool money from many investors to invest in a diversified portfolio of stocks, bonds, or other securities. ",
          "With this tool, you can explore various mutual funds and analyze their price trends over time.<br><br>",
          
          "You can choose different price metrics like Open, Close, Low, High, and Adjusted to track their performance. ",
          "Select a date range to analyze price movements and trends.<br><br>",
          
          "Select one or more Mutual Funds to start exploring. The available data covers from ", 
          format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          " to ", 
          format(max(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          ".<br><br>",
          
          "Tip: Use the reset button to clear your filters at any time and start fresh.<br><br>",
          
          "Mutual funds can be an excellent way to diversify your portfolio and access a range of asset classes. Make sure to select mutual funds that align with your investment goals.<br><br>",
          
          "</div>"
        )
      )
    } else {
      return(NULL)  # Hide intro text when Mutual Funds are selected
    }
  })
  
  # Show the "Click on a Mutual Fund's line" message only when Mutual Funds are selected
  output$clickTextMutual = renderUI({
    if(length(input$mutualFund) > 0) {
      p("Click on a Mutual Fund's line in the chart to get more information about that mutual fund.")
    } else {
      return(NULL)  # Hide the message when no Mutual Fund is selected
    }
  })
  
  # Show custom date range picker when "Custom" is selected
  output$customDateRangeMutual = renderUI({
    if (input$dateRangeMutual == "Custom Date Range") {
      dateRangeInput("customDateRangeMutual", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date),
                     format = "mm/dd/yyyy", startview = 'decade')  # Match the style from other tabs
    } else {
      return(NULL)  # Hide the custom date range input
    }
  })
  
  # Output plot based on selected Mutual Fund and date range
  output$mutualPlot = renderPlotly({
    if(length(input$mutualFund) == 0){
      return(NULL)  # Do not render plot when no Mutual Fund is selected
    } else {
      # Use the helper function to calculate date range for predefined ranges
      if (input$dateRangeMutual != "Custom Date Range") {
        date_range = calculate_date_range(input$dateRangeMutual, data)
        min_date = date_range[1]
        max_date = date_range[2]
      } else {
        min_date = input$customDateRangeMutual[1]
        max_date = input$customDateRangeMutual[2]
      }
      
      # Filter the data based on the selected date range
      filtered_data = filterData(data, input$mutualFund, min_date, max_date)
      
      # Generate the plot for selected Mutual Funds
      generatePlot(filtered_data, input$plotTypeMutual, min_date, max_date, input$metricMutual, "mutualPlot")
    }
  })
  
  # Reset Filters for Mutual Fund Page
  observeEvent(input$resetMutual, {
    updateSelectInput(session, "mutualFund", selected = NULL)
    updateSelectInput(session, "dateRangeMutual", selected = "1 Month")
    updateDateRangeInput(session, "customDateRangeMutual", start = min(data$Date), end = max(data$Date))
    updateSelectInput(session, "mutualFund", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  # Create a reactive expression for the Mutual Fund summary
  eventMutual = reactive({
    event_data("plotly_click", source = "mutualPlot")
  })
  
  # Output summary when a Mutual Fund is clicked
  output$summaryMutual = renderUI({
    event = eventMutual()
    if (!is.null(event)) {
      createInvestmentSummary(event, input, data, length(input$mutualFund))
    } else {
      return(NULL)  # Hide the summary if no Mutual Fund is selected
    }
  })
}
