install.packages("shiny")


library(shiny)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$input_file <- renderTable({
        
        file_to_read = input$file
        if(is.null(file_to_read)){
            return()
        }
        read.table(file_to_read$datapath, sep = input$sep, header = input$header)
    })
    
    output$distPlot <- renderPlot({
    ggplot(data) + geom_point(aes(x=mpg,y=wt))

    })

})
