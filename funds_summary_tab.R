# funds_summary_tab.R

# Funds Summary Tab UI
funds_summary_tab_ui <- function(data) {
  tabPanel(
    "Funds Summary", 
    fluidPage(
      titlePanel("Summary of Funds"),
      sidebarLayout(
        sidebarPanel(
          # Dropdown menu for predefined date ranges
          selectInput("PreDateRangeSummary", "Select Date Range", 
                      choices = c("7 Days", "1 Month", "6 Months", "Year-to-Date", "1 Year", "5 Years", "Custom Date Range"), 
                      selected = "1 Month"),
          
          # Conditional UI for custom date range
          uiOutput("CustomDateRangeUISummary"),
          
          # Toggle between two plot types
          radioButtons("plotType", "Select Plot Type", 
                       choices = c("Cumulative Return" = "cumulative", 
                                   "Risk vs. Return" = "risk_return"),
                       selected = "cumulative"),
          
          # Reset button
          actionButton("resetSummary", "Reset")
        ),
        mainPanel(
          plotOutput("fund_plot")  # Placeholder for the plot
        )
      )
    )
  )
}

# Funds Summary Tab Server Logic
funds_summary_tab_server <- function(data, input, output, session) {
  
  filtered_data <- reactive({
    # Handle predefined or custom date range
    if (input$PreDateRangeSummary == "Custom Date Range") {
      # Ensure custom date range is parsed as Date
      start_date <- as.Date(input$CustDateRangSummary[1], format = "%m/%d/%Y")
      end_date <- as.Date(input$CustDateRangSummary[2], format = "%m/%d/%Y")
    } else {
      date_range <- calculate_date_range(input$PreDateRangeSummary, data)
      start_date <- date_range[1]
      end_date <- date_range[2]
    }
    
    # Filter the data based on the selected date range
    return(data %>% filter(Date >= start_date & Date <= end_date))
  })
  
  # Render custom date range input UI if "Custom Date Range" is selected
  output$CustomDateRangeUISummary <- renderUI({
    if (input$PreDateRangeSummary == "Custom Date Range") {
      dateRangeInput("CustDateRangSummary", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date), 
                     startview = 'decade', format = "mm/dd/yyyy")
    }
  })
  
  # Reset the date range when the reset button is clicked
  observeEvent(input$resetSummary, {
    updateSelectInput(session, "PreDateRangeSummary", selected = "1 Month")
    updateDateRangeInput(session, "CustDateRangSummary", 
                         start = min(data$Date), end = max(data$Date))
  })
  
  # Render the plot based on the selected plot type
  output$fund_plot <- renderPlot({
    # Get filtered data based on the selected date range
    data_filtered <- filtered_data()
    
    # Generate the plot based on selected plot type
    if (input$plotType == "cumulative") {
      # Generate cumulative return plot
      generate_cumulative_return_plot(data_filtered)
    } else if (input$plotType == "risk_return") {
      # Generate risk-return plot
      generate_risk_return_plot(data_filtered)
    }
  })
}
