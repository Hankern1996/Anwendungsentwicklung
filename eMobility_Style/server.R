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
    
    long <- as.numeric(allData$Breitengrad)
    lat <- as.numeric(allData$Längengrad)
    
    
#Ladesäulenkarte    
  
    
    
    allData_Map <- allData[5:6]
    
<<<<<<< HEAD
=======
    output$map <- renderLeaflet({
        leaflet(allData_Map) %>%
        addTiles() %>% 
        addCircles(~Längengrad, ~Breitengrad, weight = 3, radius=40, 
                   color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 
    })
    
    
    
>>>>>>> db1539462ec9605bd757972a8cf87220f71fa440
    
    
    
    
    data_year = reactive({
        a = allData %<%
            filter(Jahr == input$year)%%
            group_by(year, Ladeeinreichtung)%<%
            summarise(total=sum(Ladepunkte))
    })
    
    years = sort(unique(allData$year))
    
    updateSelectInput(session, "year", choices=years, selected="2008")
    
    m <- leaflet(df) %>% addTiles()
    m %>% addCircleMarkers(radius = ~size, color = ~color)
    
    output$map <- renderLeaflet({
        leaflet(datayear()) %>%
            addTiles() %>%
            setView(lng = 9.183333, lat = 48.783333, zoom = 10)
            addMarkers()
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
