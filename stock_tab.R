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



# UI for the Stock Prices tab
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
               dateRangeInput("dateRangeStocks", "Select Date Range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date), 
                              startview = 'decade', format = "mm/dd/yyyy"),
               selectInput("metricStock", "Price Metric", 
                           choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                           selected = 'Adjusted'),
               radioButtons("plotTypeStocks", "Select Plot Type", 
                            choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                            selected = "price"),
               actionButton("resetStocks", "Reset Filters")
             ),
             mainPanel(
               plotlyOutput("stockPlot"),
               p("Click on a company's line in the chart to get more information about that company."),
               uiOutput("summaryStock")
             )
           )
  )}


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
  
  # Output empty plot when nothing is selected else output selected plot
  output$stockPlot = renderPlotly({
    if(length(input$Company) == 0){
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
      min_date = input$dateRangeStocks[1]
      max_date = input$dateRangeStocks[2]
      filtered_data = filterData(data, input$Company, min_date, max_date)
      
      generatePlot(filtered_data, input$plotTypeStocks, min_date, max_date, input$metricStock, "stockPlot")
    }
  })
  
  # Reset Filters for Stocks Page
  observeEvent(input$resetStocks, {
    updateSelectInput(session, "Company", selected = NULL)
    updateSelectInput(session, "Sector", selected = NULL)
    updateSelectInput(session, "Industry", selected = NULL)
    updateDateRangeInput(session, "dateRangeStocks", 
                         start = min(data$Date), end = max(data$Date))
    
    
    updateDateRangeInput(session, "dateRangeStocks", 
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    
    updateSelectInput(session, "Industry", choices = data %>% distinct(Industry) %>% arrange(Industry) %>% pull(Industry))
    updateSelectInput(session, "Company", choices = data %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
    updateSelectInput(session, "Sector", choices = data %>% distinct(Sector) %>% arrange(Sector) %>% pull(Sector))
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

