# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(shiny)
library(plotly)


source('helper.R')
source('stock_tab.R')
source('index_tab.R')
source('crypto_tab.R')
source('etf_tab.R')
source('mutual_tab.R')
source('combined_tab.R')

# Read data
# data = read_csv('data generation/daily_prices.csv')
data = read_csv("https://uwmadison.box.com/shared/static/81h2znsto477hgtn99nycawhsy626bae.csv")
stock_data = data %>% filter(Type == 'Stock')
index_data = data %>% filter(Type == 'Index')
crypto_data = data %>% filter(Type == 'Crypto')
mutual_data = data %>% filter(Type == 'Mutual Fund')
etf_data = data %>% filter(Type == 'ETF')


# Define UI
ui = fluidPage(
  titlePanel("Investing Explorer"),

  navbarPage("", id = "nav",
             
             stock_tab_ui(stock_data),
             index_tab_ui(index_data),
             crypto_tab_ui(crypto_data),
             etf_tab_ui(etf_data),
             mutual_tab_ui(mutual_data),
             combined_tab_ui(data)
  )
)




# Define server
server = function(input, output, session) {
  
  stock_tab_server(stock_data, input, output, session)
  index_tab_server(index_data, input, output, session)
  crypto_tab_server(crypto_data, input, output, session)
  etf_tab_server(etf_data, input, output, session)
  mutual_tab_server(mutual_data, input, output, session)
  combined_tab_server(data, input, output, session)
}



# Run the app
shinyApp(ui = ui, server = server)
