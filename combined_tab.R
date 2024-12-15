# combined_tab.R


# UI for the Combined Summary Tab
combine_summary_ui = function(data) {
  tabPanel("Compare All Investments Summary",
           sidebarLayout(
             sidebarPanel(
               # Dropdowns for each investment type
               selectInput("compareStocks", "Select Stocks", 
                           choices = data %>% filter(Type == 'Stock') %>% distinct(Name) %>% arrange(Name) %>% pull(Name),
                           multiple = TRUE),
               selectInput("compareIndex", "Select Index Funds", 
                           choices = data %>% filter(Type == 'Index') %>% distinct(Name) %>% arrange(Name) %>% pull(Name),
                           multiple = TRUE),
               selectInput("compareCryptos", "Select Cryptocurrencies", 
                           choices = data %>% filter(Type == 'Crypto') %>% distinct(Name) %>% arrange(Name) %>% pull(Name),
                           multiple = TRUE),
               selectInput("compareETFs", "Select ETFs", 
                           choices = data %>% filter(Type == 'ETF') %>% distinct(Name) %>% arrange(Name) %>% pull(Name),
                           multiple = TRUE),
               selectInput("compareMutual", "Select Mutual Funds", 
                           choices = data %>% filter(Type == 'Mutual Fund') %>% distinct(Name) %>% arrange(Name) %>% pull(Name),
                           multiple = TRUE),
               
               # Price Metric
               selectInput("compareMetricSummary", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               
               # Plot Type
               radioButtons("comparePlotTypeSummary", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change", "Waterfall" = "waterfall"),
                            selected = "price"),
               
               # Predefined Date Range
               selectInput("PreDateRangeCompareSummary", "Select Date Range", 
                           choices = c("7 Days", "1 Month", "6 Months", "1 Year", "5 Years", "Year-to-Date", "Custom Date Range"), 
                           selected = "1 Month"),
               
               # Conditional UI for custom date range
               uiOutput("customDateRangeCompareSummary"),
               
               # Reset Button
               actionButton("resetCompareSummary", "Reset Filters")
             ),
             mainPanel(
               # Introduction text when no investment is selected
               uiOutput("introTextCompareSummary"),
               plotlyOutput("comparisonSummaryPlot"),
               
               # Message when an investment is selected
               uiOutput("clickTextCompareSummary"),
               uiOutput("summaryCompareSummary")
             )
           )
  )
}

# Server logic for the Combined Summary Tab
combine_summary_server = function(data, input, output, session) {
  
  # Introduction text when no investments are selected
  output$introTextCompareSummary = renderUI({
    if (length(input$compareStocks) == 0 && length(input$compareIndex) == 0 && 
        length(input$compareCryptos) == 0 && length(input$compareETFs) == 0 && 
        length(input$compareMutual) == 0) {
      HTML("<div style='font-size: 16px; width: 80%; margin: 0 auto; white-space: normal; word-wrap: break-word; overflow-wrap: break-word;'>",
           
           # Investment Types Overview
           "<b>Investment Types Overview</b>",
           "<ul style='margin-top: 0; padding-left: 20px;'>",
           "<li><b>Stocks:</b> Stocks represent ownership in a company and offer potential for capital appreciation through price increases, along with dividends. They can be volatile and are ideal for investors seeking growth with higher risk.</li>",
           "<li><b>Mutual Funds:</b> Mutual funds pool money from many investors to buy a diversified portfolio of assets. They are actively or passively managed and are great for investors seeking professional management of their portfolios.</li>",
           "<li><b>ETFs (Exchange-Traded Funds):</b> ETFs pool investments in a variety of assets like stocks, bonds, or commodities. They are traded on exchanges like stocks and offer liquidity, diversification, and lower expense ratios compared to mutual funds. Ideal for diversified investors.</li>",
           "<li><b>Index Funds:</b> Index funds track the performance of a specific market index (like the S&P 500). They offer diversification and tend to have lower fees than actively managed funds. Ideal for passive investors seeking long-term growth.</li>",
           "<li><b>Cryptocurrencies:</b> Cryptocurrencies are digital assets that use blockchain technology. They are highly volatile and can offer high returns, but they come with significant risk. Suitable for investors with high risk tolerance seeking alternative investments.</li>",
           "</ul>",
           
           # Chart Options
           "<b>Chart Options</b>",
           "<ul style='margin-top: 0; padding-left: 20px;'>",
           "<li><b>Price vs Percentage Change vs Waterfall:</b> Choose between three chart types:",
           "<ul style='margin-top: 0; padding-left: 20px;'>",
           "<li><b>Price:</b> Displays the actual price changes of the selected investments over time.</li>",
           "<li><b>Percentage Change:</b> Shows the relative percentage change in price compared to the starting point, illustrating how the value has increased or decreased.</li>",
           "<li><b>Waterfall:</b> A waterfall chart displays the incremental changes in price over time. Each period's change is represented as a bar, with the cumulative effect shown at the end of the period. This chart is ideal for understanding how small changes add up to larger overall performance changes.</li>",
           "</ul>",
           "</li>",
           
           "<li><b>Price Metrics:</b> You can compare investments based on various price metrics:",
           "<ul style='margin-top: 0; padding-left: 20px;'>",
           "<li><b>Open/Close:</b> The price at the beginning and end of each day.</li>",
           "<li><b>High/Low:</b> The highest and lowest prices during the day.</li>",
           "<li><b>Adjusted:</b> The price adjusted for dividends, stock splits, and other corporate actions, providing a clearer picture of performance over time.</li>",
           "</ul>",
           "</li>",
           "<li><b>Date Range:</b> You can select different date ranges. For custom date ranges, you can choose any period between ", 
           format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
           "and ",
           format(max(data$Date, na.rm = TRUE), "%B %d, %Y"), 
           ".</li>",
           "</ul>",
           
           "</div>"
      )
    } else {
      return(NULL)  # Hide intro text when investments are selected
    }
  })
  
  # Show the "Click on a line" message only when investments are selected
  output$clickTextCompareSummary = renderUI({
    if (length(input$compareStocks) > 0 || length(input$compareIndex) > 0 ||
        length(input$compareCryptos) > 0 || length(input$compareETFs) > 0 ||
        length(input$compareMutual) > 0) {
      p("Click on a line in the chart to get more information about the selected investments.")
    } else {
      return(NULL)  # Hide the message if no investment is selected
    }
  })
  
  # Custom date range UI
  output$customDateRangeCompareSummary = renderUI({
    if (input$PreDateRangeCompareSummary == "Custom Date Range") {
      dateRangeInput("customDateRangeCompareSummary", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date),
                     format = "mm/dd/yyyy", startview = 'decade')
    } else {
      return(NULL)  # Hide the custom date range input
    }
  })
  
  # Render the comparison plot
  output$comparisonSummaryPlot = renderPlotly({
    if (length(input$compareStocks) == 0 && length(input$compareIndex) == 0 &&
        length(input$compareCryptos) == 0 && length(input$compareETFs) == 0 && 
        length(input$compareMutual) == 0) {
      return(NULL)  # Do not render plot when no data is available
    } else {
      # Calculate date range based on predefined or custom selection
      if (input$PreDateRangeCompareSummary != "Custom Date Range") {
        date_range = calculate_date_range(input$PreDateRangeCompareSummary, data)
        min_date = date_range[1]
        max_date = date_range[2]
      } else {
        min_date = input$customDateRangeCompareSummary[1]
        max_date = input$customDateRangeCompareSummary[2]
      }
      
      # Filter data based on selected investments and date range
      all_data = bind_rows(
        filterData(data, input$compareStocks, min_date, max_date),
        filterData(data, input$compareIndex, min_date, max_date),
        filterData(data, input$compareCryptos, min_date, max_date),
        filterData(data, input$compareETFs, min_date, max_date),
        filterData(data, input$compareMutual, min_date, max_date)
      )
      
      # Generate plot if data is available
      return(generatePlot(all_data, input$comparePlotTypeSummary, min_date, max_date, input$compareMetricSummary, "comparisonSummaryPlot"))
    }
  })
  
  # Reset Filters for the Combined Summary Tab (for all categories)
  observeEvent(input$resetCompareSummary, {
    
    # Reset all investment category selections to NULL (clear selected investments)
    updateSelectInput(session, "compareStocks", selected = NULL)
    updateSelectInput(session, "compareIndex", selected = NULL)
    updateSelectInput(session, "compareCryptos", selected = NULL)
    updateSelectInput(session, "compareETFs", selected = NULL)
    updateSelectInput(session, "compareMutual", selected = NULL)
    
    # Reset the predefined date range to default "1 Month"
    updateSelectInput(session, "PreDateRangeCompareSummary", selected = "1 Month")
    
    # Reset custom date range to default min and max values based on data
    updateDateRangeInput(session, "customDateRangeCompareSummary", 
                         start = min(data$Date, na.rm = TRUE), 
                         end = max(data$Date, na.rm = TRUE))
    
    # Reset the choices for each investment category (stocks, index, cryptos, ETFs, mutual funds)
    updateSelectInput(session, "compareStocks", 
                      choices = data %>% filter(Type == 'Stock') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    
    updateSelectInput(session, "compareIndex", 
                      choices = data %>% filter(Type == 'Index') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    
    updateSelectInput(session, "compareCryptos", 
                      choices = data %>% filter(Type == 'Crypto') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    
    updateSelectInput(session, "compareETFs", 
                      choices = data %>% filter(Type == 'ETF') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    
    updateSelectInput(session, "compareMutual", 
                      choices = data %>% filter(Type == 'Mutual Fund') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  
  # Create a reactive expression for the clicked investment summary
  eventCombinedSummary = reactive({
    event_data("plotly_click", source = "comparisonSummaryPlot")
  })
  
  # Output the summary when an investment is clicked
  output$summaryCompareSummary = renderUI({
    event = eventCombinedSummary()
    if (!is.null(event)) {
      createInvestmentSummary(event, input, data, 
                              length(input$compareStocks) + length(input$compareIndex) + 
                                length(input$compareCryptos) + length(input$compareETFs) + 
                                length(input$compareMutual))
    } else {
      return(NULL)  # Hide summary if no investment is clicked
    }
  })
}
