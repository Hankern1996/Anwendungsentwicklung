#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    allData <- read_excel("/Users/paulinawittich/Desktop/Hochschule\ Aalen/Business\ Analytics\ Anwendungsentwicklung/Ladesaeulenkarte.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "text", "numeric", "numeric", "date", 
                                        "numeric", "text", "numeric", "text", 
                                        "numeric", "text", "numeric", "text", 
                                        "numeric", "text", "numeric", "text", 
                                        "text"))
    
    allData$year <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
    allData$month <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%m")

    data = reactive({
        d = allData %>%
            filter(Bundesland == input$country) %>%
            group_by(year, Ladeeinrichtung) %>%
            summarise(total=sum(Ladepunkte))
    })
    
    
    countries = sort(unique(allData$Bundesland))
    
    updateSelectInput(session, "country", choices=countries, selected="Sachsen")
    
    output$barplot <- renderPlotly({
        plot_ly(data = data(),x=~year,y=~total,name =~Ladeeinrichtung, type="bar")
    })
    
    output$datahead <- renderTable({
        data()
    })

})
