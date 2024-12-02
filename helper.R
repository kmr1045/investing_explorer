# helper.R

# Helper functions for filtering and plotting

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

# 2. Data Filtering

# Filter the data based on selected names, and date range
filterData = function(data, selected_names, min_date, max_date) {
  data %>% filter(Name %in% selected_names) %>%
    filter(Date >= min_date, Date <= max_date)
}

# 3. Plot Type Decision

# Decide which type of plot to generate (prices or percent change)
generatePlot = function(plot_data, plot_type, min_date, max_date, metric, plot_source) {
  if (plot_type == "pct_change") {
    return(generatePctChange(plot_data, min_date, max_date, metric, plot_source))
  } else {
    return(generatePrices(plot_data, min_date, max_date, metric, plot_source))
  }
}
