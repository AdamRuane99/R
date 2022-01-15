
# Adam Ruane STock Analyser ##

# Gives YTD plot of ticker


## ---- TO DO --- ##

# To add TechnicaL Analysis such as RSI ##

##To tidy Overall UI and look 

## To add Twitter Sentiment WordClouds ##

####################################################


#Packages
library(quantmod)
library(shiny)
library(plotly)
library(tidyverse)
require(rvest) 
library(shinythemes)

## First load in some preliminary data from yahoo finance
stocks <- c('AAPL','AMD','TSLA', 'NIO')

#data prep
market_data <- c()
for (i in stocks){
    
    #grabs ticker data from yahoo finance
    stock_data <- getSymbols(i,src='yahoo', from = "2021-01-01",
                             auto.assign = FALSE) 
    
    # rename columns for consistent plotting
    # getSymbols default names are hard to work with, as they
    # assign col names based on ticker!
    column_names <- c('open', 'high', 'low', 'close', 'vol', 'adjusted')
    colnames(stock_data) <- column_names
    
    
    
    df <- data.frame(Date=index(stock_data),coredata(stock_data))
    df$ticker <- paste(i)
    
    market_data <- bind_rows(market_data,df)
}

# now we have all of our yahoo finance data for plotting

## now lets scrub all of our mentions and sentiment data from Ape Wisdom
## get_more_stats function I wrote
get_more_stats <- function(ticker){
    
    url <- paste('https://apewisdom.io/stocks/',ticker,'/',sep = '')
    site <- read_html(url)
    
    row_labels <- c('mentions',
                    'upvotes',
                    'mentioning_users',
                    'sentiment')
    
    scrubbed_data <- site %>% html_nodes(".tile-value") %>% html_text(.)
    scrubbed_data <- as.data.frame(scrubbed_data,row_labels)
    colnames(scrubbed_data) <- c('24 hour Stats')
    
    return(scrubbed_data)
}

#########################################################################

ui <- fluidPage(theme = shinytheme("united"),
                headerPanel('StockAnalyser | Reddit Stock Tracker'),
                sidebarPanel(selectInput("company", "Choose A Stock:", #this is our selector
                                         c(
                                             "AMD" = "AMD",
                                             "Tesla" = "TSLA",
                                             "NIO" = "NIO",
                                             "AAPL"
                                         )),
                             tableOutput("table") ),  #this table shows our srcubbed stats
                mainPanel(plotlyOutput("graph")) #candles plot
                
)


## This is where the computation and interactive elements are shown

server <- function(input, output, session){
    
    Sortie <- reactive({
        company <- input$company
        temp <- market_data %>% filter(ticker == input$company)
        temp
    })
    
    output$graph <- renderPlotly({
        sortie <- Sortie()
        plot_ly(sortie,x=sortie$Date,type="candlestick",
                open=sortie$open,close=sortie$close,
                high=sortie$high,low=sortie$low) %>% 
            layout(title = paste(input$company, "Daily Candles"), 
                   xaxis = list(rangeslider = list(visible = F)),
                   automargin = TRUE) 
        
    })
    
    
    data <- eventReactive(input$company,{
        req(input$company)
        get_more_stats(input$company)
    })
    
    output$table <- renderTable(
        data(),
        rownames = TRUE,
        colnames = TRUE
    )
    
}

shinyApp(ui ,server)
