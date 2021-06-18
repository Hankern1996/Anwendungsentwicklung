

library(shiny)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    data <- read.csv("C:\Users\HANKERN\Desktop\DAP\Master\Business Analytics Anwendungsentwicklung\archive\books.csv")
    
    output$distPlot <- renderPlot({
    ggplot(data) + geom_point(aes(x=mpg,y=wt))

    })

})
