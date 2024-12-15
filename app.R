# Load necessary libraries
library(ggplot2)          # Data visualization
library(dplyr)            # Data manipulation
library(lubridate)        # Date handling
library(readr)            # Reading data
library(shiny)            # Shiny framework
library(plotly)           # Interactive plots
library(waterfalls)       # Waterfall Plot
library(scales)           # Format currency

# Source the helper functions and tab UI/Server files
source('helper.R')
source('stock_tab.R')
source('index_tab.R')
source('crypto_tab.R')
source('etf_tab.R')
source('mutual_tab.R')
source('combined_tab.R')
source('funds_summary_tab.R')

# Read in the data (using the local file path or a URL for live data)
data = read_csv('data generation/daily_prices.csv')
# data = read_csv("https://uwmadison.box.com/shared/static/81h2znsto477hgtn99nycawhsy626bae.csv")

# Split data by type for each investment category
stock_data = data %>% filter(Type == 'Stock')
index_data = data %>% filter(Type == 'Index')
crypto_data = data %>% filter(Type == 'Crypto')
mutual_data = data %>% filter(Type == 'Mutual Fund')
etf_data = data %>% filter(Type == 'ETF')

# Define a simpler and cleaner theme using Shiny's built-in options
ui = fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Use Shiny's 'flatly' theme
  
  titlePanel("Investing Explorer"),
  
  navbarPage("", id = "nav",
             
             # Tabs in a logical order based on their relationships
             combine_summary_ui(data),  # First: Combined Summary (overview of all investments)
             stock_tab_ui(stock_data),  # Second: Stocks (individual stock investments)
             mutualfund_tab_ui(mutual_data),  # Third: Mutual Funds (actively or passively managed funds)
             etf_tab_ui(etf_data),      # Fourth: ETFs (similar to mutual funds but traded like stocks)
             index_tab_ui(index_data),  # Fifth: Index Funds (specific type of ETF/mutual fund)
             crypto_tab_ui(crypto_data),# Sixth: Cryptos (digital assets, more volatile)
             
             # Last: Funds Summary tab (overview of all funds)
             funds_summary_tab_ui(data)  # New tab
  )
)

# Server logic to handle the data processing and interactions
server = function(input, output, session) {
  
  # Call server functions for each tab
  stock_tab_server(stock_data, input, output, session)
  index_tab_server(index_data, input, output, session)
  crypto_tab_server(crypto_data, input, output, session)
  etf_tab_server(etf_data, input, output, session)
  mutualfund_tab_server(mutual_data, input, output, session)
  combine_summary_server(data, input, output, session)
  
  # Server logic for the Funds Summary tab
  funds_summary_tab_server(data, input, output, session)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
