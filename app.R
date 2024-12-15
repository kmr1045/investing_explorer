# app.R

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

# Read in the data
data = read_csv("https://uwmadison.box.com/shared/static/81h2znsto477hgtn99nycawhsy626bae.csv")

# Split data by type for each investment category
stock_data = data %>% filter(Type == 'Stock')
index_data = data %>% filter(Type == 'Index')
crypto_data = data %>% filter(Type == 'Crypto')
mutual_data = data %>% filter(Type == 'Mutual Fund')
etf_data = data %>% filter(Type == 'ETF')

ui = fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Use Shiny's 'flatly' theme
  
  titlePanel("Investing Explorer"),
  
  navbarPage("", id = "nav",
             
             combine_summary_ui(data), 
             stock_tab_ui(stock_data),  
             mutualfund_tab_ui(mutual_data), 
             etf_tab_ui(etf_data),      
             index_tab_ui(index_data), 
             crypto_tab_ui(crypto_data),
  )
)


server = function(input, output, session) {
  
  # Call server functions for each tab
  stock_tab_server(stock_data, input, output, session)
  index_tab_server(index_data, input, output, session)
  crypto_tab_server(crypto_data, input, output, session)
  etf_tab_server(etf_data, input, output, session)
  mutualfund_tab_server(mutual_data, input, output, session)
  combine_summary_server(data, input, output, session)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
