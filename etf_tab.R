# etf_tab.R

# UI for the ETF tab
etf_tab_ui = function(data) {
  tabPanel("ETFs",
           sidebarLayout(
             sidebarPanel(
               selectInput("ETF", 'Select ETF', 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               dateRangeInput("dateRangeETF", "Select Date Range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date), 
                              startview = 'decade', format = "mm/dd/yyyy"),
               selectInput("metricETF", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeETF", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                            selected = "price"),
               actionButton("resetETF", "Reset Filters")
             ),
             mainPanel(
               plotlyOutput("etfPlot"),
               p("Click on a company's line in the chart to get more information about that company."),
               uiOutput("summaryETF")
             )
           )
  )
}

# Server logic for the ETF tab
etf_tab_server = function(data, input, output, session) {
  output$etfPlot = renderPlotly({
    if(length(input$ETF) == 0){
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
      min_date = input$dateRangeETF[1]
      max_date = input$dateRangeETF[2]
      filtered_data = filterData(data, input$ETF, min_date, max_date)
      
      generatePlot(filtered_data, input$plotTypeETF, min_date, max_date, input$metricETF, "etfPlot")
    }
  })
  # Reset Filters for ETF Page
  observeEvent(input$resetETF, {
    updateSelectInput(session, "ETF", selected = NULL)
    updateDateRangeInput(session, "dateRangeETF", 
                         start = min(data$Date), end = max(data$Date))
    
    updateDateRangeInput(session, "dateRangeETF", 
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    
    updateSelectInput(session, "ETF", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  # Create a reactive expression for the company summary
  eventETF = reactive({
    event_data("plotly_click", source = "etfPlot")
  })
  
  output$summaryETF = renderUI({
    event = eventETF()
    createInvestmentSummary(event, input, data, length(input$ETF))
  })
  
  }
