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
  allData_Map <- allData[5:9]
  allData_Map$year <- format(as.Date(allData_Map$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
  
  
  data0 <-reactive({
    allData_Map %>%
      filter(Jahr == input$Jahr) %>%
      summarise(total=(allData_Map$Ladeeinrichtung))
  })
  
  years = sort(unique(allData_Map$year))
  
  updateSelectInput(session, "Jahr", choices=years, selected="2008")


  pal <- colorFactor(
    palette = c('red', 'blue'),
    domain = allData_Map$Ladeeinrichtung
  )
  
  
  output$map <- renderLeaflet({
    leaflet(allData_Map) %>%
      addTiles() 

  })
  
  observe({
    
    leafletProxy("map", data = data0()) %>%
      clearShapes() %>% 
      clearPopups() %>% 
      clearMarkers() %>%
      addCircles(~allData_Map$Längengrad, 
                 ~allData_Map$Breitengrad,
                 radius = 40, 
                 weight = 3, 
                 color=~pal(allData_Map$Ladeeinrichtung), 
                 fillOpacity = 0.8
      ) %>%
      addProviderTiles("Stamen.Toner") 
    
    #addMarkers(
    #   lng = ~Longitude, # note the tildes before values, required
    #   lat = ~Latitude,
    #   popup = ~paste(
    #      Institution,
    #      "<br>",
    #      "Overall Satisfaction:",
    #      Sat_2016,
    #      "<br>"
    #   )
    #)
    
  })
  
  
  


  
  #Analyse_pro_Bundesland    
  
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
  
  # Graphen zu Top 10 Städte Deutschland
  
  
  data1 = reactive({
    d = allData %>%
      filter(year <= input$checkYear) %>%
      group_by(Ort) %>%
      summarise(total1=sum(Ladepunkte)) %>%
      arrange(desc(total1)) %>%
      slice(1:10)
  })
  
  #yform <- list(categoryorder = "array",
  #             categoryarray = as.vector(data1()))
  
  
  year = sort(unique(allData$year))
  
  #my_list[names(my_list) != "b"]  
  
  updateSelectInput(session, "checkYear", choices=year[-1], selected="2020")
  
  output$barplot1 <- renderPlotly({
    plot_ly(data = data1(),y=~Ort,x=~total1, type="bar",  orientation = 'h' ) #%>% 
    #layout(yaxis = yform)
  })
  
  output$datahead1 <- renderTable({
    data2()
  })
  
  
  
  
})
