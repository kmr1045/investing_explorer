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
generatePlot <- function(plot_data, plot_type, min_date, max_date, metric, plot_source) {
  if (plot_type == "pct_change") {
    return(generatePctChange(plot_data, min_date, max_date, metric, plot_source))
  } else if (plot_type == "waterfall") {
    return(generate_waterfall_plot(plot_data, min_date, max_date, metric, plot_source))  
  } else {
    return(generatePrices(plot_data, min_date, max_date, metric, plot_source))
  }
}


# Generate Cumulative Return Plot
generate_cumulative_return_plot <- function(data) {
  start_date = min(data$Date)
  end_date = max(data$Date)
  
  formatted_start_date <- format(as.Date(start_date), "%b %d, %Y")
  formatted_end_date <- format(as.Date(end_date), "%b %d, %Y")
  
  # Filter the data for the specified date range
  data_cumulative <- data %>%
    group_by(Type, Sector, Symbol) %>%
    arrange(Date) %>%
    mutate(
      DailyReturn = (Adjusted - lag(Adjusted)) / lag(Adjusted),  # Calculate daily return
      CumulativeReturn = (cumprod(1 + ifelse(is.na(DailyReturn), 0, DailyReturn)) - 1) * 100  # Cumulative return in percentage
    ) %>%
    ungroup()
  
  # Create a new column for "Stock" vs "Other" and group by that
  data_cumulative <- data_cumulative %>%
    mutate(
      Category = ifelse(Type == "Stock", "Stock", "Other"),
      facet_var = ifelse(Type == "Stock" & !is.na(Sector), Sector, Type)
    )
  
  # Define the custom order for faceting
  facet_order <- c("Crypto", "ETF", "Index", "Mutual Fund",
                   "Communication Services", "Consumer Discretionary",
                   "Consumer Staples", "Energy", "Financials",
                   "Health Care", "Industrials", "Information Technology",
                   "Materials", "Real Estate", "Utilities")
  
  # Ensure facet_var is a factor with the specified order
  data_cumulative$facet_var <- factor(data_cumulative$facet_var, levels = facet_order)
  
  # Summarize the data to get the median and quartiles (1st and 3rd)
  data_summary <- data_cumulative %>%
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
    facet_wrap(~ facet_var, scales = "free_y") +  # Facet by Sector for "Stock" or Type for "Other"
    theme_minimal() +  # Clean theme
    theme(
      legend.position = "none",  # Remove legend
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
}

# Generate Risk vs. Return Plot
generate_risk_return_plot <- function(data) {
  start_date = min(data$Date)
  end_date = max(data$Date)
  
  formatted_start_date <- format(as.Date(start_date), "%b %d, %Y")
  formatted_end_date <- format(as.Date(end_date), "%b %d, %Y")
  
  # Filter and prepare the data
  data_risk_return <- data %>%
    group_by(Type, Sector, Symbol) %>%
    arrange(Date) %>%
    mutate(DailyReturn = (Adjusted - lag(Adjusted)) / lag(Adjusted)) %>%
    summarise(
      AvgReturn = mean(DailyReturn, na.rm = TRUE),
      Risk = sd(DailyReturn, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Create a category for stock and other asset types
  data_risk_return <- data_risk_return %>%
    mutate(
      Category = ifelse(Type == "Stock", "Stock", "Other"),
      facet_var = ifelse(Type == "Stock" & !is.na(Sector), Sector, Type)
    )
  
  facet_order <- c("Crypto", "ETF", "Index", "Mutual Fund",
                   "Communication Services", "Consumer Discretionary",
                   "Consumer Staples", "Energy", "Financials",
                   "Health Care", "Industrials", "Information Technology",
                   "Materials", "Real Estate", "Utilities")
  
  # Ensure that facet_var is a factor with the specified order
  data_risk_return$facet_var <- factor(data_risk_return$facet_var, levels = facet_order)
  
  # Plot for "Other" Category
  ggplot(data_risk_return, aes(x = Risk, y = AvgReturn)) +
    geom_point() +
    labs(title = paste("Risk vs. Return from", formatted_start_date, "to", formatted_end_date), 
         x = "Risk (Standard Deviation)", y = "Average Daily Return (%)") +
    theme(legend.title = element_blank()) +
    facet_wrap(~ facet_var) +   # Adjust facets 
    geom_hline(yintercept = 0, color = "black", size = 0.2)  
}

# Helper function to calculate date ranges
calculate_date_range <- function(predefined_range, data) {
  end_date <- max(data$Date)
  start_date <- case_when(
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



generate_waterfall_plot <- function(filtered_data, min_date, max_date, metric, plot_source) {
  # Calculate the difference in days
  date_diff <- as.numeric(difftime(max_date, min_date, units = "days"))
  
  # Decide grouping period based on the date range length, with buffer
  if (date_diff > 365 * 3) {
    period_type <- "year"
  } else if (date_diff > 30 * 6) {
    period_type <- "month"
  } else if (date_diff > 7 * 5) {
    period_type <- "week"
  } else {
    period_type <- "day"
  }
  
  # Data processing with date filtering and dynamic period selection
  water_data <- filtered_data %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Name, Symbol, Period = floor_date(Date, period_type)) %>%
    reframe(
      Metric_Value = round(last(!!sym(metric)) - first(!!sym(metric)), 1),
      .groups = 'drop'
    ) %>%
    mutate(Label = as.character(Period)) %>%
    select(Name, Symbol, Label, Metric_Value)
  
  # Calculate cumulative change for waterfall
  water_data <- water_data %>%
    group_by(Name) %>%
    mutate(Cumulative_Metric_Value = cumsum(Metric_Value)) %>%
    ungroup()
  
  # Calculate total for each company
  total_data <- water_data %>%
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
  water_data <- bind_rows(water_data, total_data) %>%
    distinct(Name, Label, .keep_all = TRUE)
  
  # Create the waterfall plot
  num_companies <- n_distinct(water_data$Name)
  colors <- RColorBrewer::brewer.pal(min(num_companies, 9), "Set2")
  
  waterfall_plot <- plot_ly(data = water_data, x = ~Label, 
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
