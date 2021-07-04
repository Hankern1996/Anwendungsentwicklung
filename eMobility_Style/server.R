#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(readxl)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    allData <- read_excel("Ladesaeulenkarte.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "text", "numeric", "numeric", "date", 
                                        "numeric", "text", "numeric", "text", 
                                        "numeric", "text", "numeric", "text", 
                                        "numeric", "text", "numeric", "text", 
                                        "text"))
    
    
    
    allData$year <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
    allData$month <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%m")
    
#Ladesäulenkarte    
  
    
    
    allData_Map <- allData[5:6]
    
    output$map <- renderLeaflet({
        leaflet(allData) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(allData$Breitengrad, allData$Längengrad)
    })
    
    
    
    
    
    data_year = reactive({
        
        a = allData %<%
            filter(Jahr == input$year)
            group_by(year, Ladeeinreichtung)%<%
            summarise(total=sum(Ladepunkte))
    })
    
    years = sort(unique(allData$year))
    updateSelectInput(session, "year", choices=years, selected="2008")
    
    
#Analye_pro_Bundesland    

    data = reactive({
        d = allData %>%
            filter(Bundesland == input$country) %>%
            group_by(year, Ladeeinrichtung) %>%
            summarise(total=sum(Ladepunkte))
    })
    
    #Analysen Ladesäule pro Bundesland (Schnell- und Langsam)
    countries = sort(unique(allData$Bundesland))
    
    updateSelectInput(session, "country", choices=countries, selected="Sachsen")
    
    output$barplot <- renderPlotly({
        plot_ly(data = data(),x=~year,y=~total,name =~Ladeeinrichtung, type="bar")
    })
    
    output$datahead <- renderTable({
        data()
    })
    
    
        
})
