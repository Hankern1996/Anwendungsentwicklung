#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Books"),
    
    #sidebarLayout()
    
    verbatimTextOutput("code"),
    
    tableOutput("static"),

    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    path<-"/Users/paulinawittich/Desktop/Hochschule\ Aalen/Business\ Analytics\ Anwendungsentwicklung/books.csv"
    data<-read.csv(path, header = TRUE)
    
    output$distPlot <- renderPlot({
        tmp <- filter(data, cyl >= input$average_rating)
        ggplot(tmp)+geom_point(aes(x=average_rating, y=ratings_count))
    })
    
    output$code <- renderPrint({ 
        summary(data) 
    })
    
    output$dynamic <- renderDataTable(data, options = list(pageLength = 5))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
