# crypto_tab.R

# UI for the Cryptocurrency tab
crypto_tab_ui = function(data) {
  tabPanel("Cryptocurrencies",
           sidebarLayout(
             sidebarPanel(
               selectInput("crypto", 'Select Cryptocurrency', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               selectInput("metricCrypto", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeCrypto", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change", "Waterfall" = "waterfall"),
                            selected = "price"),
               
               # Dropdown for Predefined Date Range
               selectInput("dateRangeCrypto", "Select Date Range", 
                           choices = c("7 Days", "1 Month", "6 Months", "1 Year", "Year-to-Date", "5 Years", "Custom Date Range"), 
                           selected = "1 Month"),
               
               # Conditional UI for Custom Date Range
               uiOutput("customDateRangeCrypto"),
               
               actionButton("resetCrypto", "Reset Filters")
             ),
             mainPanel(
               # Show introduction text when no cryptocurrency is selected
               uiOutput("introTextCrypto"),
               plotlyOutput("cryptoPlot"),
               # Only show the "Click on a Cryptocurrency's line" text when a cryptocurrency is selected
               uiOutput("clickTextCrypto"),
               uiOutput("summaryCrypto")
             )
           )
  )
}

# Server logic for the Cryptocurrency tab
crypto_tab_server = function(data, input, output, session) {
  
  # Show introduction text when no cryptocurrency is selected
  output$introTextCrypto = renderUI({
    if(length(input$crypto) == 0) {
      HTML(
        paste(
          "<div style='font-size: 16px; width: 80%; margin: 0 auto; white-space: normal; word-wrap: break-word; overflow-wrap: break-word;'>",
          
          "Cryptocurrencies are a digital form of money, secured by cryptography, making them difficult to counterfeit. ",
          "Unlike traditional currencies, cryptocurrencies operate on decentralized networks, typically using blockchain technology. ",
          "Bitcoin, Ethereum, and others are examples of popular cryptocurrencies. This tool allows you to explore their price trends over time.<br><br>",
          
          "You can track various price metrics like Open, Close, Low, High, and Adjusted prices. ",
          "Choose a specific date range to analyze price movements over different periods—whether you're interested in short-term fluctuations or long-term trends.<br><br>",
          
          "Select one or more cryptocurrencies, adjust the filters, and gain insights into how these digital assets perform. ",
          "The data presented here is from ", 
          format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          " to ", 
          format(max(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          ".<br><br>",
          
          "Tip: Use the filters to narrow down your cryptocurrency selection and explore different price metrics. You can reset your filters anytime to start fresh.<br><br>",
          
          "Investing in cryptocurrencies can be risky due to volatility, but it offers opportunities for high returns. Always perform your own research before investing.<br><br>",
          
          "</div>"
        )
      )
    } else {
      return(NULL)  # Hide the text when a cryptocurrency is selected
    }
  })
  
  # Show the "Click on a cryptocurrency's line" text only when a cryptocurrency is selected
  output$clickTextCrypto = renderUI({
    if(length(input$crypto) > 0) {
      p("Click on a Cryptocurrency's line in the chart to get more information about that cryptocurrency.")
    } else {
      return(NULL)  # Hide the text when no cryptocurrency is selected
    }
  })
  
  # Show custom date range picker when "Custom" is selected
  output$customDateRangeCrypto = renderUI({
    if (input$dateRangeCrypto == "Custom Date Range") {
      dateRangeInput("customDateRangeCrypto", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date),
                     format = "mm/dd/yyyy", startview = 'decade')
    } else {
      return(NULL)  # Hide the custom date range input
    }
  })
  
  # Output plot based on selected Cryptocurrency and date range
  output$cryptoPlot = renderPlotly({
    if(length(input$crypto) == 0){
      return(NULL)  # Do not render plot when no cryptocurrency is selected
    } else {
      # Use the helper function to calculate date range for predefined ranges
      if (input$dateRangeCrypto != "Custom Date Range") {
        date_range = calculate_date_range(input$dateRangeCrypto, data)
        min_date = date_range[1]
        max_date = date_range[2]
      } else {
        min_date = input$customDateRangeCrypto[1]
        max_date = input$customDateRangeCrypto[2]
      }
      
      # Filter the data based on the selected date range
      filtered_data = filterData(data, input$crypto, min_date, max_date)
      
      # Generate the plot for selected cryptocurrencies
      generatePlot(filtered_data, input$plotTypeCrypto, min_date, max_date, input$metricCrypto, "cryptoPlot")
    }
  })
  
  # Reset Filters for Cryptocurrency Page
  observeEvent(input$resetCrypto, {
    updateSelectInput(session, "crypto", selected = NULL)
    updateSelectInput(session, "dateRangeCrypto", selected = "1 Month")
    updateDateRangeInput(session, "customDateRangeCrypto", start = min(data$Date), end = max(data$Date))
    updateSelectInput(session, "crypto", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  # Create a reactive expression for the Cryptocurrency summary
  eventCrypto = reactive({
    event_data("plotly_click", source = "cryptoPlot")
  })
  
  # Output summary when a cryptocurrency is clicked
  output$summaryCrypto = renderUI({
    event = eventCrypto()
    if (!is.null(event)) {
      createInvestmentSummary(event, input, data, length(input$crypto))
    } else {
      return(NULL)  # Hide the summary if no cryptocurrency is selected
    }
  })
}
