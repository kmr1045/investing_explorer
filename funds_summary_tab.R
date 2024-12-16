# funds_summary_tab.R

# Generate Cumulative Return Plot
generate_cumulative_return_plot = function(data) {
  start_date = min(data$Date)
  end_date = max(data$Date)
  
  formatted_start_date = format(as.Date(start_date), "%b %d, %Y")
  formatted_end_date = format(as.Date(end_date), "%b %d, %Y")
  
  # Filter the data for the specified date range
  data_cumulative = data %>%
    group_by(Type, Sector, Symbol) %>%
    arrange(Date) %>%
    mutate(
      DailyReturn = (Adjusted - lag(Adjusted)) / lag(Adjusted),  # Calculate daily return
      CumulativeReturn = (cumprod(1 + ifelse(is.na(DailyReturn), 0, DailyReturn)) - 1) * 100  # Cumulative return in percentage
    ) %>%
    ungroup()
  
  # Create a new column for "Stock" vs "Other" and group by that
  data_cumulative = data_cumulative %>%
    mutate(
      Category = ifelse(Type == "Stock", "Stock", "Other"),
      facet_var = ifelse(Type == "Stock" & !is.na(Sector), Sector, Type)
    ) %>%
    filter(!is.na(facet_var) & facet_var != "Crypto")  # Filter out "Crypto" and NA
  
  # Define the custom order for faceting (removed "Crypto")
  facet_order = c("ETF", "Index", "Mutual Fund",
                   "Communication Services", "Consumer Discretionary",
                   "Consumer Staples", "Energy", "Financials",
                   "Health Care", "Industrials", "Information Technology",
                   "Materials", "Real Estate", "Utilities")
  
  # Ensure facet_var is a factor with the specified order
  data_cumulative$facet_var = factor(data_cumulative$facet_var, levels = facet_order)
  
  # Summarize the data to get the median and quartiles (1st and 3rd)
  data_summary = data_cumulative %>%
    group_by(Category, facet_var, Date) %>%
    summarise(
      MedianCumulativeReturn = median(CumulativeReturn, na.rm = TRUE),
      Q1 = quantile(CumulativeReturn, 0.25, na.rm = TRUE),
      Q3 = quantile(CumulativeReturn, 0.75, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Plot with ribbon and median line for both stocks and other assets
  ggplot(data_summary, aes(x = Date, y = MedianCumulativeReturn)) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "blue", alpha = 0.2) +  # Ribbon for 1st and 3rd quartile
    geom_line() +  # Line for median
    geom_hline(yintercept = 0, color = "black", size = 0.1) +  # Line at y=0
    labs(title = paste("Cumulative Return from", formatted_start_date, "to", formatted_end_date), 
         x = "Date", y = "Cumulative Return (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
    facet_wrap(~ facet_var, scales = "fixed") +  # Facet with fixed scales
    theme_minimal() +  # Clean theme
    theme(
      legend.position = "none",  # Remove legend
      plot.title = element_text(hjust = 0.5, margin = margin(t = 20))  # Add margin to title
    )
}



# Generate Risk vs. Return Plot
generate_risk_return_plot = function(data) {
  start_date = min(data$Date)
  end_date = max(data$Date)
  
  formatted_start_date = format(as.Date(start_date), "%b %d, %Y")
  formatted_end_date = format(as.Date(end_date), "%b %d, %Y")
  
  data_risk_return = data %>%
    group_by(Type, Sector, Name) %>%
    arrange(Date) %>%
    mutate(DailyReturn = (Adjusted - lag(Adjusted)) / lag(Adjusted)) %>%
    summarise(
      AvgReturn = mean(DailyReturn, na.rm = TRUE),
      Risk = sd(DailyReturn, na.rm = TRUE),
      .groups = "drop"  # This removes grouping after summarising
    ) %>%
    ungroup()
  
  # Create a category for stock and other asset types
  data_risk_return = data_risk_return %>%
    mutate(
      Category = ifelse(Type == "Stock", "Stock", "Other"),
      facet_var = ifelse(Type == "Stock" & !is.na(Sector), Sector, Type)
    ) %>%
    filter(!is.na(facet_var) & facet_var != "Crypto")  # Filter out "Crypto" and NA
  
  # Define the custom order for faceting (removed "Crypto")
  facet_order = c("ETF", "Index", "Mutual Fund",
                   "Communication Services", "Consumer Discretionary",
                   "Consumer Staples", "Energy", "Financials",
                   "Health Care", "Industrials", "Information Technology",
                   "Materials", "Real Estate", "Utilities")
  
  # Ensure that facet_var is a factor with the specified order
  data_risk_return$facet_var = factor(data_risk_return$facet_var, levels = facet_order)
  
  # Create the ggplot
  p = ggplot(data_risk_return, aes(x = Risk, y = AvgReturn, text = paste("Name: ", Name, 
                                                                          "<br>Avg Daily Return: ", round(AvgReturn, 4), "%", 
                                                                          "<br>Std Dev: ", round(Risk, 4)))) +
    geom_point() +  # Dot plot (Risk vs. Return)
    labs(title = paste("Risk vs. Return from", formatted_start_date, "to", formatted_end_date), 
         x = "Risk (Standard Deviation)", y = "Average Daily Return (%)") +
    facet_wrap(~ facet_var, scales = "fixed", nrow = 3) +  # Adjust facets to have 3 rows to spread out the plots
    geom_hline(yintercept = 0, color = "black", size = 0.2) +
    theme_minimal() +  # Apply minimal theme
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(t = 20)),  # Center title and add margin
      axis.title = element_text(size = 12),  # Adjust axis titles for consistency
      axis.text = element_text(size = 10),   # Adjust axis text size for readability
      strip.text = element_text(size = 10),  # Adjust facet label size
      legend.position = "none",              # Remove the legend
      plot.margin = margin(t = 30, r = 10, b = 20, l = 10),  # Add more space around the plot
      axis.ticks.y = element_blank()  # Remove ticks on y-axis to reduce clutter
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))  # Reduce the number of y-axis labels
  
  # Convert ggplot to a plotly object for interactivity (hover)
  p_plotly = ggplotly(p, tooltip = "text", source = 'fund_plot') %>% 
    event_register('plotly_click')  
  
  return(p_plotly)
}



# Funds Summary Tab UI
funds_summary_tab_ui = function(data) {
  tabPanel(
    "Risk vs Return", 
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          radioButtons("plotType", "Select Plot Type", 
                       choices = c("Cumulative Return" = "cumulative", 
                                   "Risk vs. Return" = "risk_return"),
                       selected = "cumulative"),
          
          selectInput("PreDateRangeSummary", "Select Date Range", 
                      choices = c("7 Days", "1 Month", "6 Months", "Year-to-Date", "1 Year", "5 Years", "Custom Date Range"), 
                      selected = "1 Month"),
          
          uiOutput("CustomDateRangeUISummary"),
          
          
          actionButton("resetSummary", "Reset")
        ),
        mainPanel(
          plotlyOutput("fund_plot"),  # Use plotlyOutput instead of plotOutput
          uiOutput("explanation_text")  # Add the explanation text UI here
        )
      )
    )
  )
}


# Funds Summary Tab Server Logic
funds_summary_tab_server = function(data, input, output, session) {
  
  filtered_data = reactive({
    if (input$PreDateRangeSummary == "Custom Date Range") {
      start_date = as.Date(input$CustDateRangSummary[1], format = "%m/%d/%Y")
      end_date = as.Date(input$CustDateRangSummary[2], format = "%m/%d/%Y")
    } else {
      date_range = calculate_date_range(input$PreDateRangeSummary, data)
      start_date = date_range[1]
      end_date = date_range[2]
    }
    
    return(data %>% filter(Date >= start_date & Date <= end_date))
  })
  
  # Render custom date range input UI if "Custom Date Range" is selected
  output$CustomDateRangeUISummary = renderUI({
    if (input$PreDateRangeSummary == "Custom Date Range") {
      dateRangeInput("CustDateRangSummary", "Select Custom Date Range", 
                     start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date), 
                     startview = 'decade', format = "mm/dd/yyyy")
    }
  })
  
  observeEvent(input$resetSummary, {
    updateSelectInput(session, "PreDateRangeSummary", selected = "1 Month")
    updateDateRangeInput(session, "CustDateRangSummary", 
                         start = min(data$Date), end = max(data$Date))
  })
  
  # Render the plot based on the selected plot type
  output$fund_plot = renderPlotly({
    data_filtered = filtered_data()
    
    plot = NULL
    if (input$plotType == "cumulative") {
      plot = generate_cumulative_return_plot(data_filtered)
    } else if (input$plotType == "risk_return") {
      plot = generate_risk_return_plot(data_filtered)
    }
    
    plot
  })
  
  # Render the explanation text for the plot
  output$explanation_text = renderUI({
    HTML(paste(
      "<h4>Explanation of the Graph:</h4>",
      
      "<p><strong>Cumulative Return Plot:</strong><br>",
      "This plot shows the cumulative return of different investment types (such as ETFs, Mutual Funds, and Stocks) over time. ",
      "The shaded area represents the 1st and 3rd quartiles, while the line shows the median cumulative return. ",
      "The graph helps you understand how different assets have performed during the selected time period, with the horizontal line at zero indicating no return.</p>",
      
      "<p><strong>Risk vs. Return Plot:</strong><br>",
      "This plot compares the risk (standard deviation) and the average daily return of various assets. ",
      "Each point represents a different investment, and you can see how risky it is relative to its average daily return. ",
      "A higher point on the y-axis indicates a higher return, while points further to the right on the x-axis represent higher risk. </p>",
      
      "<p><strong>Overall Risk vs. Return Comparison:</strong><br>",
      "The Risk vs. Return plot helps compare how different types of investments perform in terms of risk (volatility) and return. For example, stocks typically offer higher returns but come with greater volatility, making them riskier compared to more stable assets like bonds or mutual funds. ETFs, which may track broader indices or sectors, often fall between these two extremes, offering moderate returns with moderate risk. ",
      "Understanding where different asset types lie on this plot can help investors make decisions that balance their risk tolerance and desired returns. Generally, assets to the right of the plot indicate higher risk, while those higher up on the plot represent higher returns. This comparison is crucial for constructing a diversified portfolio that aligns with an investor’s financial goals and risk preferences.</p>",
      
      "<p><strong>Why Crypto was Removed:</strong><br>",
      "We have excluded 'Crypto' from these plots because the returns of cryptocurrencies tend to be highly volatile, with extreme outliers that can distort the results. ",
      "By removing crypto, we provide a clearer picture of more stable and traditional investment types, making it easier to analyze risk and return trends.</p>"
    ))
  })
  
}
