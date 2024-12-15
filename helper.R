# helper.R

# Helper functions for filtering and plotting


# ____________________________________________________________________________________________________________
# 1. Plot Generation Functions

# Generate plot for prices (e.g., Open, Close, Adjusted)
generatePrices = function(filtered_data, min_date, max_date, metric, plot_source) {
  plot_data = filtered_data %>%
    mutate(hover_text = case_when(
      metric == "Adjusted" ~ paste0("Name: ", Name, "<br>Date: ", format(Date, "%B %d, %Y"), "<br>Adjusted: $", format(round(Adjusted, 2), nsmall = 2)),
      metric %in% c("Open", "Close") ~ paste0("Name: ", Name, "<br>Date: ", format(Date, "%B %d, %Y"), "<br>Open: $", format(round(Open, 2), nsmall = 2), "<br>Close: $", format(round(Close, 2), nsmall = 2), "<br>Adjusted: $", format(round(Adjusted, 2), nsmall = 2)),
      metric %in% c("High", "Low") ~ paste0("Name: ", Name, "<br>Date: ", format(Date, "%B %d, %Y"), "<br>Low: $", format(round(Low, 2), nsmall = 2), "<br>High: $", format(round(High, 2), nsmall = 2), "<br>Adjusted: $", format(round(Adjusted, 2), nsmall = 2))
    ))
  
  plot_ly(data = plot_data, x = ~Date, 
          y = plot_data[[metric]], color = ~Name, 
          type = 'scatter', mode = 'lines',
          text = ~hover_text, 
          hoverinfo = 'text',
          key = ~Name, source = plot_source) %>%
    layout(
      title = paste(metric, "Share Price from", format(min_date, "%B %d, %Y")),
      xaxis = list(title = "Date", range = c(min_date, max_date)),
      yaxis = list(title = paste(metric, "Price ($)"))
    ) %>%
    event_register("plotly_click")
}


# Generate plot for percent change since the minimum date
generatePctChange = function(filtered_data, min_date, max_date, metric, plot_source) {
  plot_data = filtered_data %>%
    group_by(Name) %>%
    mutate(Begin_Val = get(metric)[1],
           Pct_Change = (get(metric) - Begin_Val) / Begin_Val * 100) %>%
    ungroup()
  
  plot_ly(data = plot_data, x = ~Date, 
          y = ~Pct_Change, color = ~Name, 
          type = 'scatter', mode = 'lines', 
          text = ~paste("Name:", Name, "<br>Date:", format(Date, "%B %d, %Y"), "<br>Percent Change:", round(Pct_Change, 2), "%"),
          hoverinfo = 'text', key = ~Name, source = plot_source) %>%
    layout(
      title = paste(metric, "Percentage Change from", format(min_date, "%B %d, %Y")),
      xaxis = list(title = "Date", range = c(min_date, max_date)),
      yaxis = list(title = paste(metric, "Percentage Change (%)"))
    ) %>% event_register("plotly_click")
}

# Add click feature that gives company summary
createInvestmentSummary = function(event, input, data, selection) {
  if ((is.null(event)) | (selection == 0)) {
    return(NULL)
  } else {
    clicked_investment = event$key
    investment_info = data %>%
      filter(Name == clicked_investment) %>%
      select(Name, Symbol, Sector, Industry, Headquarters, Founded, Date_added) %>%
      unique()
    
    # Improved check for Sector to handle empty or NULL values
    if (!is.na(investment_info$Sector)) {
      tagList(
        h4(paste("Summary for:", investment_info$Name)),
        p(paste("Symbol:", investment_info$Symbol)),
        p(paste("Sector:", investment_info$Sector)),
        p(paste("Industry:", investment_info$Industry)),
        p(paste("Headquarters:", investment_info$Headquarters)),
        p(paste("Founded:", investment_info$Founded)),
        p(paste("Date Added to S&P 500:", format(as.Date(investment_info$Date_added), "%B %d, %Y")))
      )
    } else {
      tagList(
        h4(paste("Summary for:", investment_info$Name)),
        p(paste("Symbol:", investment_info$Symbol))
      )
    }
  }
}

generate_waterfall_plot = function(filtered_data, min_date, max_date, metric, plot_source) {
  # Calculate the difference in days
  date_diff = as.numeric(difftime(max_date, min_date, units = "days"))
  
  # Decide grouping period based on the date range length, with buffer
  if (date_diff > 365 * 3) {
    period_type = "year"
  } else if (date_diff > 30 * 4) {
    period_type = "month"
  } else if (date_diff > 7 * 4) {
    period_type = "week"
  } else {
    period_type = "day"
  }
  
  # Data processing with date filtering and dynamic period selection
  water_data = filtered_data %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Name, Symbol, Period = floor_date(Date, period_type)) %>%
    reframe(
      Metric_Value = round(last(!!sym(metric)) - first(!!sym(metric)), 1),
      .groups = 'drop'
    ) %>%
    mutate(Label = as.character(Period)) %>%
    select(Name, Symbol, Label, Metric_Value)
  
  # Calculate cumulative change for waterfall
  water_data = water_data %>%
    group_by(Name) %>%
    mutate(Cumulative_Metric_Value = cumsum(Metric_Value)) %>%
    ungroup()
  
  # Calculate total for each company
  total_data = water_data %>%
    group_by(Name) %>%
    reframe(
      Label = "Total",
      Metric_Value = sum(Metric_Value),
      Cumulative_Metric_Value = sum(Cumulative_Metric_Value),
      Symbol = unique(Symbol),
      hover_text = paste0("Name: ", Name,
                          "<br>Symbol: ", Symbol,
                          "<br>Period: Total",
                          "<br>", metric, " Change: ", dollar(sum(Metric_Value)),
                          "<br>Source: ", plot_source)  
    ) %>%
    ungroup()
  
  # Combine the original data with the total row
  water_data = bind_rows(water_data, total_data) %>%
    distinct(Name, Label, .keep_all = TRUE)
  
  # Create the waterfall plot
  num_companies = n_distinct(water_data$Name)
  colors = RColorBrewer::brewer.pal(min(num_companies, 9), "Set2")
  
  waterfall_plot = plot_ly(data = water_data, x = ~Label, 
                            y = ~Metric_Value, type = 'bar', 
                            color = ~Name, colors = colors,
                            text = ~dollar(Metric_Value),
                            hoverinfo = 'text', 
                            textposition = 'inside',
                            textfont = list(color = 'black'),
                            showlegend = TRUE, 
                            marker = list(line = list(color = 'black', width = 1))) %>%
    layout(
      title = paste("Change in", metric, "from", format(min_date, "%B %d, %Y"), "to", format(max_date, "%B %d, %Y")),
      xaxis = list(title = "Period"),
      yaxis = list(title = paste("Change in", metric, "($)"), rangemode = "tozero"),
      barmode = 'group'
    ) %>%
    event_register("plotly_click")
  
  return(waterfall_plot)
}


# ____________________________________________________________________________________________________________
# 2. Data Filtering

# Filter the data based on selected names, and date range
filterData = function(data, selected_names, min_date, max_date) {
  data %>% filter(Name %in% selected_names) %>%
    filter(Date >= min_date, Date <= max_date)
}


# Helper function to calculate date ranges
calculate_date_range = function(predefined_range, data) {
  end_date = max(data$Date)
  start_date = case_when(
    predefined_range == "7 Days" ~ end_date - 7,
    predefined_range == "1 Month" ~ end_date - 30,
    predefined_range == "6 Months" ~ end_date - 180,
    predefined_range == "Year-to-Date" ~ as.Date(paste0(format(end_date, "%Y"), "-01-01")),
    predefined_range == "1 Year" ~ end_date - 365,
    predefined_range == "5 Years" ~ end_date - 1825,
    TRUE ~ end_date  # Default case
  )
  return(c(start_date, end_date))
}



# ____________________________________________________________________________________________________________
# 3. Plot Type Decision

# Decide which type of plot to generate (prices or percent change)
generatePlot = function(plot_data, plot_type, min_date, max_date, metric, plot_source) {
  if (plot_type == "pct_change") {
    return(generatePctChange(plot_data, min_date, max_date, metric, plot_source))
  } else if (plot_type == "waterfall") {
    return(generate_waterfall_plot(plot_data, min_date, max_date, metric, plot_source))  
  } else {
    return(generatePrices(plot_data, min_date, max_date, metric, plot_source))
  }
}

