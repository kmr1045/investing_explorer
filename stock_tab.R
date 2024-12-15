# stock_tab.R

# Update possible industries and companies when sector drop down is updated
updateSelections = function(selected_sectors, data, session, input) {
  industries = unique(data %>% filter(Sector %in% selected_sectors) %>% pull(Industry))
  updateSelectInput(session, "Industry", choices = sort(industries), selected = intersect(input$Industry, industries))
  companies = unique(data %>% filter(Sector %in% selected_sectors) %>% pull(Name))
  updateSelectInput(session, "Company", choices = sort(companies), selected = intersect(input$Company, companies))
}

# Update the sector and companies when industry drop down is updated
filterCompaniesByIndustry = function(selected_industries, data, session, input) {
  companies = unique(data %>% filter(Industry %in% selected_industries) %>% pull(Name))
  updateSelectInput(session, "Company", choices = sort(companies), selected = intersect(input$Company, companies))
  sectors = unique(data %>% filter(Industry %in% selected_industries) %>% pull(Sector))
  updateSelectInput(session, "Sector", choices = sort(sectors), selected = intersect(input$Sector, sectors))
}

# UI for the Stocks tab
stock_tab_ui = function(data) {
  tabPanel("Stocks",
           sidebarLayout(
             sidebarPanel(
               selectInput("Company", "Select Company", 
                           choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name), 
                           multiple = TRUE),
               selectInput("Sector", "Select Sector", 
                           choices = data %>% distinct(Sector) %>% arrange(Sector) %>% pull(Sector), 
                           multiple = TRUE),
               selectInput("Industry", "Select Industry", 
                           choices = data %>% distinct(Industry) %>% arrange(Industry) %>% pull(Industry), 
                           multiple = TRUE),
               selectInput("metricStock", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeStocks", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change", "Waterfall" = "waterfall"),
                            selected = "price"),
               # Dropdown for date range selection with an added "Custom" option
               selectInput("dateRangeStocks", "Select Date Range", 
                           choices = c("7 Days", "1 Month", "6 Months", "Year-to-Date", "1 Year", "5 Years", "Custom Date Range"),
                           selected = "1 Month"),  # Default is "1 Month"
               
               # Conditional UI to show custom date range input when "Custom" is selected
               uiOutput("customDateRangeStock"),
               
               actionButton("resetStocks", "Reset Filters")
             ),
             mainPanel(
               # Show introduction text when no company is selected
               uiOutput("introText"),
               plotlyOutput("stockPlot"),
               # Only show the "Click on a company's line" text when a company is selected
               uiOutput("clickText"),
               uiOutput("summaryStock")
             )
           )
  )
}

# Server logic for the Stocks tab
stock_tab_server = function(data, input, output, session) {
  
  # Filter companies when sector is changed
  observeEvent(input$Sector, {
    req(input$Sector)
    updateSelections(input$Sector, data, session, input)
  })
  
  # Filter companies when industry is changed
  observeEvent(input$Industry, {
    req(input$Industry)
    filterCompaniesByIndustry(input$Industry, data, session, input)
  })
  
  # Show introduction text when no companies are selected
  output$introText = renderUI({
    if(length(input$Company) == 0) {
      HTML(
        paste(
          "<div style='font-size: 16px; width: 80%; margin: 0 auto; white-space: normal; word-wrap: break-word; overflow-wrap: break-word;'>",
          
          "To begin exploring stocks, select one or more companies from the list. You can narrow down your search by selecting a specific sector or industry. ",
          
          "The app allows you to view stock prices based on different metrics such as Open, Close, High, Low, or Adjusted prices. Select a time frame (like 7 days, 1 month, or 5 years) to analyze trends over time. ",
          
          "Data for stocks is available from ", 
          format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          " to ", 
          format(max(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          ".<br><br>",
          
          "Tip: If you're unsure, start by selecting a sector like Technology or Healthcare to see how different companies perform within those categories. ",
          
          "You can always reset your filters to start fresh. Stocks are organized by sectors and industries to make it easier for you to explore companies within specific categories. <br><br>",
          
          "Sectors represent large areas of the economy, such as Technology, Healthcare, Finance, or Energy, grouping companies based on the nature of their business activities. ",
          
          "Industries narrow this focus further, helping you find more specific categories within a sector. For example, within the Technology sector, you may find industries like Software, Hardware, or IT Services. ",
          
          "Select a company, and we will show you its stock price trends over time, helping you make informed decisions as you learn more about investing.<br><br>",
          
          "Remember, you can always reset the filters to explore other sectors, industries, or companies!",
          
          "</div>"
        )
      )
    } else {
      return(NULL)  # Hide the text when a company is selected
    }
  })
  
  # Show the "Click on a company's line" text only when a company is selected
  output$clickText = renderUI({
    if(length(input$Company) > 0) {
      p("Click on a company's line in the chart to get more information about that company.")
    } else {
      return(NULL)  # Hide the text when no company is selected
    }
  })
  
  # Output the stock plot
  output$stockPlot = renderPlotly({
    if(length(input$Company) == 0){
      return(NULL) 
    } else {
      # Use the helper function to calculate date range for predefined ranges
      if (input$dateRangeStocks != "Custom Date Range") {
        date_range = calculate_date_range(input$dateRangeStocks, data)
        min_date = date_range[1]
        max_date = date_range[2]
      } else {
        min_date = input$customDateRangeStocks[1]
        max_date = input$customDateRangeStocks[2]
      }
      
      # Filter the data based on the selected date range
      filtered_data = filterData(data, input$Company, min_date, max_date)
      
      # Generate the plot for selected companies
      generatePlot(filtered_data, input$plotTypeStocks, min_date, max_date, input$metricStock, "stockPlot")
    }
  })
  
  # Reset Filters for Stocks Page
  observeEvent(input$resetStocks, {
    updateSelectInput(session, "Company", selected = NULL)
    updateSelectInput(session, "Sector", selected = NULL)
    updateSelectInput(session, "Industry", selected = NULL)
    
    # Reset date range to default (1 Month)
    updateSelectInput(session, "dateRangeStocks", selected = "1 Month")
    updateDateRangeInput(session, "customDateRangeStocks", start = min(data$Date), end = max(data$Date))
    
    updateSelectInput(session, "Industry", choices = data %>% distinct(Industry) %>% arrange(Industry) %>% pull(Industry))
    updateSelectInput(session, "Company", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    updateSelectInput(session, "Sector", choices = data %>% distinct(Sector) %>% arrange(Sector) %>% pull(Sector))
  })
  
  # Show custom date range picker when "Custom" is selected
  output$customDateRangeStock = renderUI({
    if (input$dateRangeStocks == "Custom Date Range") {
      dateRangeInput("customDateRangeStocks", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date),
                     format = "mm/dd/yyyy", startview = 'decade')  # Match the style from other tabs
    } else {
      return(NULL)  # Hide the custom date range input
    }
  })
  
  # Create a reactive expression for the company summary
  eventStock = reactive({
    event_data("plotly_click", source = "stockPlot")
  })
  
  output$summaryStock = renderUI({
    event = eventStock()
    createInvestmentSummary(event, input, data, length(input$Company))
  })
}
