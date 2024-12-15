# UI for the Index Fund tab
index_tab_ui = function(data) {
  tabPanel("Index Funds",
           sidebarLayout(
             sidebarPanel(
               selectInput("indexFund", 'Select Index Fund', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               selectInput("metricIndex", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeIndex", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change", "Waterfall" = "waterfall"),
                            selected = "price"),
               
               # Dropdown for Predefined Date Range
               selectInput("dateRangeIndex", "Select Date Range", 
                           choices = c("7 Days", "1 Month", "6 Months", "Year-to-Date", "1 Year", "5 Years", "Custom Date Range"), 
                           selected = "1 Month"),
               
               # Conditional UI for Custom Date Range
               uiOutput("customDateRangeIndex"),
               
               actionButton("resetIndex", "Reset Filters")
             ),
             mainPanel(
               # Show introduction text when no index fund is selected
               uiOutput("introTextIndex"),
               plotlyOutput("indexPlot"),
               # Only show the "Click on an index fund's line" text when an index fund is selected
               uiOutput("clickTextIndex"),
               uiOutput("summaryIndex")
             )
           )
  )
}

# Server logic for the Index Fund tab
index_tab_server = function(data, input, output, session) {
  
  # Show introduction text when no index funds are selected
  output$introTextIndex <- renderUI({
    if(length(input$indexFund) == 0) {
      HTML(
        paste(
          "<div style='font-size: 16px; width: 80%; margin: 0 auto; white-space: normal; word-wrap: break-word; overflow-wrap: break-word;'>",
          
          "An index fund is a type of investment fund that aims to replicate the performance of a specific market index. ",
          "Rather than picking individual stocks, an index fund tracks a large group of stocks that represent a particular segment of the market, like the S&P 500 or the NASDAQ-100. ",
          "Index funds are designed to give investors exposure to a wide range of companies, offering diversification in a single investment.<br><br>",
          
          "One of the key benefits of index funds is their low cost. Unlike actively managed funds, where fund managers select the stocks, index funds simply replicate an index. ",
          "This reduces management fees and costs. Additionally, because index funds are passively managed, they often outperform actively managed funds in the long run.<br><br>",
          
          "Investing in index funds offers several advantages: diversification, lower fees, and ease of management. They are perfect for investors looking to follow the market rather than trying to beat it.<br><br>",
          
          "In this tool, you can explore various index funds, view different price metrics such as Open, Close, High, Low, or Adjusted prices, and analyze their performance over time. ",
          "Select a date range to see how an index fund has performed, whether you want to look at 7 days, 1 month, or up to 5 years of data. You can even choose a custom date range for specific analysis.<br><br>",
          "</div>"
        )
      )
    } else {
      return(NULL)  # Hide the text when an index fund is selected
    }
  })
  
  # Show the "Click on an index fund's line" text only when an index fund is selected
  output$clickTextIndex <- renderUI({
    if(length(input$indexFund) > 0) {
      p("Click on an Index Fund's line in the chart to get more information about that index.")
    } else {
      return(NULL)  # Hide the text when no index fund is selected
    }
  })
  
  # Output the index plot
  output$indexPlot = renderPlotly({
    if(length(input$indexFund) == 0){
      plot_ly() %>%
        layout(
          title = list(
            text = ""
          ),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    } else {
      # Use the helper function to calculate date range for predefined ranges
      if (input$dateRangeIndex != "Custom Date Range") {
        date_range = calculate_date_range(input$dateRangeIndex, data)
        min_date = date_range[1]
        max_date = date_range[2]
      } else {
        min_date = input$customDateRangeIndex[1]
        max_date = input$customDateRangeIndex[2]
      }
      
      # Filter the data based on the selected date range
      filtered_data = filterData(data, input$indexFund, min_date, max_date)
      
      # Generate the plot for selected index funds
      generatePlot(filtered_data, input$plotTypeIndex, min_date, max_date, input$metricIndex, "indexPlot")
    }
  })
  
  # Reset Filters for Index Fund Page
  observeEvent(input$resetIndex, {
    updateSelectInput(session, "indexFund", selected = NULL)
    updateSelectInput(session, "dateRangeIndex", selected = "1 Month")
    updateDateRangeInput(session, "customDateRangeIndex", start = min(data$Date), end = max(data$Date))
    updateSelectInput(session, "indexFund", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  # Show custom date range picker when "Custom" is selected
  output$customDateRangeIndex <- renderUI({
    if (input$dateRangeIndex == "Custom Date Range") {
      dateRangeInput("customDateRangeIndex", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date),
                     format = "mm/dd/yyyy", startview = 'decade')  # Match the style from other tabs
    } else {
      return(NULL)  # Hide the custom date range input
    }
  })
  
  # Create a reactive expression for the index fund summary
  eventIndex = reactive({
    event_data("plotly_click", source = "indexPlot")
  })
  
  # Output summary when an index fund is clicked
  output$summaryIndex = renderUI({
    event = eventIndex()
    if (!is.null(event)) {
      createInvestmentSummary(event, input, data, length(input$indexFund))
    } else {
      return(NULL)  # Hide the summary if no index fund is selected
    }
  })
}
