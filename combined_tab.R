# combined_tab.R

# UI for the Combined Tab
combined_tab_ui = function(data) {
  tabPanel("Compare All Investments",
           sidebarLayout(
             sidebarPanel(
               selectInput("compareCompanies", "Select Companies", 
                           choices = data %>% filter(Type == 'Stock') %>% distinct(Name) %>% arrange(Name) %>% pull(Name),
                           multiple = TRUE),
               selectInput("compareIndex", 'Select Index Fund', 
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
               dateRangeInput("compareDateRange", "Select Date Range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date), 
                              startview = 'decade', format = "mm/dd/yyyy"),
               selectInput("compareMetric", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("comparePlotType", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                            selected = "price"),
               actionButton("resetCompare", "Reset Filters")
             ),
             mainPanel(
               plotlyOutput("comparisonPlot"),
               p("Click on a company's line in the chart to get more information about that company."),
               uiOutput("summaryCompare")
             )
           )
  )
}



combined_tab_server = function(data, input, output, session) {
  output$comparisonPlot = renderPlotly({
    if((length(input$compareCompanies)) + (length(input$compareIndex)) + (length(input$compareCryptos)) 
       + (length(input$compareETFs)) + (length(input$compareMutual)) == 0){
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
      min_date = input$compareDateRange[1]
      max_date = input$compareDateRange[2]
      all_data = bind_rows(
        filterData(data, input$compareCompanies, min_date, max_date),
        filterData(data, input$compareIndex, min_date, max_date),
        filterData(data, input$compareCryptos, min_date, max_date),
        filterData(data, input$compareETFs, min_date, max_date),
        filterData(data, input$compareMutual, min_date, max_date),
      )
      generatePlot(all_data, input$comparePlotType, min_date, max_date, input$compareMetric, "comparisonPlot")
    }
  })
  
  # add reset button, to set all filters back to the starting point
  observeEvent(input$resetCompare, {
    updateSelectInput(session, "compareCompanies", selected = NULL)
    updateSelectInput(session, "compareCryptos", selected = NULL)
    updateSelectInput(session, "compareIndex", selected = NULL)
    updateSelectInput(session, "compareETFs", selected = NULL)
    updateSelectInput(session, "compareMutual", selected = NULL)
    updateDateRangeInput(session, "compareDateRange", 
                         start = min(data$Date), end = max(data$Date))
    
    
    updateDateRangeInput(session, "compareDateRange", 
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    updateSelectInput(session, "compareCompanies", choices = data %>% filter(Type == 'Stock') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    updateSelectInput(session, "compareCryptos", choices = data %>% filter(Type == 'Crypto') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    updateSelectInput(session, "compareIndex", choices = data %>% filter(Type == 'Index') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    updateSelectInput(session, "compareETFs", choices  = data %>% filter(Type == 'ETF') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    updateSelectInput(session, "compareMutual", choices = data %>% filter(Type == 'Mutual Fund') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  })
  
  
  # Create a reactive expression for the company summary
  eventCombined = reactive({
    event_data("plotly_click", source = "comparisonPlot")
  })
  
  output$summaryCompare = renderUI({
    event = eventCombined()
    createInvestmentSummary(event, input, data, 
        length(input$compareCompanies) + length(input$compareIndex) + length(input$compareCryptos) + length(input$compareETFs) + length(input$compareMutual))
  })
  
  }
