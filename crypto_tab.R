# crypto_tab.R

# Common UI for both stocks and cryptos
crypto_tab_ui = function(data){
  tabPanel("Cryptocurrency",
           sidebarLayout(
             sidebarPanel(
               selectInput("Crypto", 'Select Cryptocurrency', 
                           choices = data  %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               dateRangeInput("dateRangeCrypto", "Select Date Range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date), 
                              startview = 'decade', format = "mm/dd/yyyy"),
               selectInput("metricCrypto", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeCrypto", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                            selected = "price"),
               actionButton("resetCrypto", "Reset Filters")
             ),
             mainPanel(
               plotlyOutput("cryptoPlot"),
               p("Click on a company's line in the chart to get more information about that company."),
               uiOutput("summaryCrypto")
             )
           )
  )
}


crypto_tab_server = function(data, input, output, session) {
  output$cryptoPlot = renderPlotly({
    if(length(input$Crypto) == 0){
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
      min_date = input$dateRangeCrypto[1]
      max_date = input$dateRangeCrypto[2]
      filtered_data = filterData(data, input$Crypto, min_date, max_date)
      
      generatePlot(filtered_data, input$plotTypeCrypto, min_date, max_date, input$metricCrypto,"cryptoPlot")
    }
  })
  
  # Reset Filters for Crypto Page
  observeEvent(input$resetCrypto, {
    updateSelectInput(session, "Crypto", selected = NULL)
    updateDateRangeInput(session, "dateRangeCrypto", 
                         start = min(data$Date), end = max(data$Date))
    
    updateDateRangeInput(session, "dateRangeCrypto", 
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    
    updateSelectInput(session, "Crypto", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  
  # Create a reactive expression for the company summary
  eventCrypto = reactive({
    event_data("plotly_click", source = "cryptoPlot")
  })
  
  output$summaryCrypto = renderUI({
    event = eventCrypto()
    createInvestmentSummary(event, input, data, length(input$Crypto))
  })
  }

