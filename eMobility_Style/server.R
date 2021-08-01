library(shiny)
library(plotly)
library(readxl)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(gganimate)
library(gifski)
library(shinycssloaders)
library(directlabels)
library(forecast)
library(lubridate)
library(geojson)
library(prophet)


allData <- read_excel("Ladesaeulenkarte_v3.xlsx", 
                      col_types = c("text", "text", "text", 
                                    "text", "numeric", "numeric", "date", 
                                    "numeric", "text", "numeric", "text", 
                                    "numeric", "text", "numeric", "text", 
                                    "numeric", "text", "numeric", "text", 
                                    "text"))

#zulassungen <- read.csv("Ladesaeulenkarte_neu.xlsx")
data_zulassungen <- read_table2("zulassungen_bundesland2.csv",
                                col_types = cols(Datum = col_date(format = "%Y-%m-%d")))



allData$year <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
allData$month <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%m")
allData$Month <- floor_date(allData$Inbetriebnahmedatum, "month")
#allData$year <- floor_date(allData$Inbetriebnahmedatum, "year")
#data_zulassungen$Datum <- as.Date(data_zulassungen$Datum, "%Y/%m/%d")
#data_zulassungen$year <- floor_date(data_zulassungen$Datum, "year")
data_zulassungen$year <- format(as.Date(data_zulassungen$Datum, format="%Y-%m-%d"),"%Y")

list_choices <- list("Top 10 Städte","Top 10 Bundesländer")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #-------------------------------
  # Veränderung: Hinzukommende Ladepunkte in Deutschland für die Kästchen
  
  deutschland_total <- reactive({
    allData %>% 
      filter(year <= input$Jahr2, Bundesland != 0) %>% 
      summarize(total=sum(Ladepunkte))
  })
  
  output$mapInfo <- renderText({
    paste("+",as.character(round(deutschland_total()/357400*1000)))
  })
  
  output$mapInfo1 <- renderText({
    paste("+",as.character(round(deutschland_total()/83)))
  })
  
  output$mapInfo2 <- renderText({
    paste("+",as.character(round(deutschland_total())))
  })
  
  
  #-------------------------------
  
  #Dichtekarte pro qkm:
  states <- geojsonio::geojson_read("bundeslaender.geojson", what = "sp")
  class(states)
  
  allData_Map <- allData[1:10]
  allData_Map$year <- format(as.Date(allData_Map$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
  
  flaeche <- read_excel("Bundesland_flaeche.xlsx",
                        col_types = c("text", "numeric"))
  
  
  
  #year_start0 <- allData %>% filter(year <= 2020) %>% group_by(Bundesland) %>% summarize(total=sum(Ladepunkte))
  #year_start0$density <- floor(year_start0$total/flaeche$qkm*100)


  
  bins <- c(0,1,2,5, 10, 15, 20,50,100,150, Inf)

  #pal <- colorBin("YlOrRd", domain = year_start0$density, bins = bins)




  
  year_start_test <- reactive({
    allData %>% 
      filter(year <= input$Jahr2, Bundesland != 0) %>% 
      group_by(Bundesland) %>% 
      summarize(total=sum(Ladepunkte))%>% 
      group_by(Bundesland)
  })
  
  year_start_test0 <- (
    allData %>% 
      filter(year <= 2021, Bundesland != 0) %>% 
      group_by(Bundesland) %>% 
      summarize(total=sum(Ladepunkte))%>% 
      group_by(Bundesland)
  )
  
  
  output$m <- renderLeaflet({
    
    pal <- colorBin("YlOrRd", domain = year_start_test0$total, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ladepunkte / 100 km<sup>2</sup>",
      year_start_test0$Bundesland, ceiling((year_start_test0$total/flaeche$qkm)*100)) %>% lapply(htmltools::HTML)
    
    leaflet(year_einwohner0) %>%
      addTiles() %>% 
      setView(lng = 10.4515,lat = 51.1657, zoom = 5)  %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>%
      addPolygons(data = states, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(ceiling((year_start_test0$total/flaeche$qkm)*100)),
                  highlight = highlightOptions(color = "white", weight = 2,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~total/flaeche$qkm*100, opacity = 0.7, title = NULL,
                position = "bottomright")    
  })
  
  
  observe({
    
    pal <- colorBin("YlOrRd", domain = year_start_test()$total, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ladepunkte / 100 km<sup>2</sup>",
      year_start_test()$Bundesland, ceiling((year_start_test()$total/flaeche$qkm)*100)) %>% lapply(htmltools::HTML)
    
    leafletProxy("m", data = year_start_test()) %>%
      clearShapes() %>% 
      clearPopups() %>% 
      clearMarkers()%>% 
      addPolygons(data = states, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(ceiling((year_start_test()$total/flaeche$qkm)*100)),
                  highlight = highlightOptions(color = "white", weight = 2,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) 
    
  })
  
 

  
  
  
 #Dichtekarte pro Einwohner
  
  einwohner <- read_excel("Bundesland_einwohner.xlsx",
                        col_types = c("text", "numeric"))
  
  
  
  #year_start0 <- allData %>% filter(year <= 2020) %>% group_by(Bundesland) %>% summarize(total=sum(Ladepunkte))
  #year_start0$density <- floor(year_start0$total/flaeche$qkm*100)
  
  
  
  bins2 <- c(0,1,2,5, 10, 20,40,50, Inf)
  
  #pal <- colorBin("YlOrRd", domain = year_start0$density, bins = bins)
  
  
  
  
  
  year_einwohner <- reactive({
    allData %>% 
      filter(year <= input$Jahr2, Bundesland != 0) %>% 
      group_by(Bundesland) %>% 
      summarize(total=sum(Ladepunkte))%>% 
      group_by(Bundesland)
  })
  
  year_einwohner0 <- (
    allData %>% 
      filter(year <= 2021, Bundesland != 0) %>% 
      group_by(Bundesland) %>% 
      summarize(total=sum(Ladepunkte))%>% 
      group_by(Bundesland)
  )
  
  output$map_einwohner <- renderLeaflet({
    
    pal <- colorBin("YlOrRd", domain = year_einwohner0$total, bins = bins2)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ladepunkte / 100.000 Einwohner<sup>2</sup>",
      year_einwohner0$Bundesland, ceiling((year_einwohner0$total/einwohner$einwohner)*100000)) %>% lapply(htmltools::HTML)
    
    leaflet(year_einwohner0) %>%
      addTiles() %>% 
      setView(lng = 10.4515,lat = 51.1657, zoom = 5)  %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>%
      addPolygons(data = states, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(ceiling((year_einwohner0$total/einwohner$einwohner)*100000)),
                  highlight = highlightOptions(color = "white", weight = 2,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~total/einwohner$einwohner*100000, opacity = 0.7, title = NULL,
                position = "bottomright")    
  })
  
  observe({
    
    pal <- colorBin("YlOrRd", domain = year_einwohner()$total, bins = bins2)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ladepunkte / 100.000 Einwohner<sup>2</sup>",
      year_einwohner()$Bundesland, ceiling((year_einwohner()$total/einwohner$einwohner)*100000)) %>% lapply(htmltools::HTML)
    
    leafletProxy("map_einwohner", data = year_einwohner()) %>%
      clearShapes() %>% 
      clearPopups() %>% 
      clearMarkers() %>%
      addPolygons(data = states, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(ceiling((year_einwohner()$total/einwohner$einwohner)*100000)),
                  highlight = highlightOptions(color = "white", weight = 2,
                                               bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
  })
  
  

  
  
  #zweite Map mit Slider
  
  filteredData <- reactive({
    allData_Map %>%
      filter(year <= input$Jahr2) 
  })
  
  yearstart <- (allData_Map %>%
                  filter(year <= 2021, Ladeeinrichtung != 0) 
  )
  
  pal1 <- colorFactor(
    palette = c('blue', 'yellow'),
    domain = allData_Map$Ladeeinrichtung
    
  )

  
  output$map2 <- renderLeaflet({
    leaflet(yearstart) %>%
      addTiles() %>% 
      setView(lng = 10.4515,lat = 51.1657, zoom = 5)  %>% 
      addProviderTiles("CartoDB.Positron")  %>%
      addCircleMarkers(~Längengrad, ~Breitengrad, popup=paste("Ladeeinrichtung:", yearstart$Ladeeinrichtung, "<br>",
                                                            "Längengrad:", yearstart$Längengrad, "<br>",
                                                            "Breitengrad:", yearstart$Breitengrad, "<br>"), weight = 1, radius=2, 
                     color=~pal1(Ladeeinrichtung), stroke = F, fillOpacity = 0.5) 
    
  })
  
  observe({
    
    leafletProxy("map2", data = filteredData()) %>%
      clearShapes() %>% 
      clearPopups() %>% 
      clearMarkers() %>%
      addCircleMarkers(~Längengrad, ~Breitengrad, popup=paste("Ladeeinrichtung:", filteredData()$Ladeeinrichtung, "<br>",
                                                              "Längengrad:", filteredData()$Längengrad, "<br>",
                                                              "Breitengrad:", filteredData()$Breitengrad, "<br>"), weight = 1, radius=2, 
                       color=~pal1(Ladeeinrichtung), stroke = F, fillOpacity = 0.5) 
      
  })
#------
  
  #Forecasting mit Prophet
  
  filtered <- reactive({
    allData_Map %>%
      filter(year <= input$Jahr2) 
  })
  
    
    output$forecast <- renderPlot({
      
      forecast_df <- read_excel("Prediction_Test.xlsx",col_types = c("date","numeric", "numeric"))
      forecasting_data <- forecast_df[c(1,3)]
      
      m <- prophet(forecasting_data, daily.seasonality=TRUE) 
      future <- make_future_dataframe(m, periods = 365) 
      predict(m, future)
      plot(m, forecast)
      
    })
    
   
  
  #------------------------
  #Analyse_pro_Bundesland   
  
  #output$mean_mpg<- renderText({ 
  #  text1
  #})
  
  #output$mean_mpg1<- renderText({ 
  #  "text1"
  #})
  
  output$orderNum <- renderText({
    paste("+",as.character(round(veraenderung())),"%")
  })
  

  output$orderNum1 <- renderText({
    paste("+",as.character(round(veraenderung())),"%")
    
    # Durchschnittliche Hinzunahme von Ladepunkten pro Monat
  })
  
  output$orderNum2 <- renderText({
    paste("+",as.character(round(veraenderung())),"%")
    # Verhältnis Schnell/Langsam 
  })
  
  
  min_wert_1 = reactive({
    d = allData %>%
      filter(Bundesland == "Baden-Württemberg", Ladeeinrichtung != 0) %>%
      filter(Inbetriebnahmedatum <= "2016-01-01") %>%
      group_by(Month) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      arrange(desc(Month)) %>%
      slice(0:1) %>%
      select(total1)
  })
  
  max_wert_1 = reactive({
    d = allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0) %>%
      filter(Inbetriebnahmedatum <= input$input_date_range[2]) %>%
      group_by(Month) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      arrange(desc(Month)) %>%
      slice(0:1) %>%
      select(total1)
  })
  
  veraenderung1 = reactive({
    ((max_wert1() - min_wert1())/min_wert1())*100
  })
  
  
  text1 <- "Hallo"
  #output$vbox <- renderValueBox(vb)
  
  
  
  data = reactive({
    d = allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung!=0) %>%
      filter(Inbetriebnahmedatum >= input$input_date_range[1]) %>%
      filter(Inbetriebnahmedatum <= input$input_date_range[2]) %>%
      group_by(Month, Ladeeinrichtung) %>%
      summarise(total=sum(Ladepunkte))
  })
  
  
  #Analysen Ladesäule pro Bundesland (Schnell- und Langsam)
  countries = sort(unique(allData$Bundesland))[-1]
  
  updateSelectInput(session, "country", choices=countries, selected="Baden-Württemberg")
  
  output$barplot <- renderPlotly({
    fig1 <- plot_ly(data = data(),x=~Month,y=~total,name =~Ladeeinrichtung, type="bar")
    fig1<- fig1 %>% layout(legend = list(x = 0.1, y = 0.9), barmode="stack",
                           title = '\nNeu installierte Ladepunkte\n',
                           xaxis = list(title = 'Monat/Jahr',
                                        zeroline = TRUE),
                           yaxis = list(title = 'Anzahl Ladepunkte\n'))
  })
  
  data_kumuliert = reactive({
    d = allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung!= 0) %>%
      filter(Inbetriebnahmedatum <= input$input_date_range[2]) %>%
      group_by(Month, Ladeeinrichtung) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      group_by(Ladeeinrichtung) %>%
      mutate(total1 = cumsum(total)) %>%
      filter(Month >= input$input_date_range[1]) 
  })
 
  min_wert = reactive({
    d = allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0) %>%
      filter(Inbetriebnahmedatum <= input$input_date_range[1]) %>%
      group_by(Month) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      arrange(desc(Month)) %>%
      slice(0:1) %>%
      select(total1)
  })
  
  max_wert = reactive({
    d = allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0) %>%
      filter(Inbetriebnahmedatum <= input$input_date_range[2]) %>%
      group_by(Month) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      arrange(desc(Month)) %>%
      slice(0:1) %>%
      select(total1)
  })
  
  veraenderung = reactive({
    ((max_wert() - min_wert())/min_wert())*100
  })
  
  output$kumuliert <- renderPlotly({
    fig <- plot_ly(data = data_kumuliert(),x=~Month,y=~total1,name =~Ladeeinrichtung, type="scatter", mode="lines")
    fig %>% layout(legend = list(x = 0.1, y = 0.9),
                   title = '\nSummer aller Ladepunkte\n',
                   xaxis = list(title = 'Monat/Jahr',
                                zeroline = TRUE),
                   yaxis = list(title = 'Anzahl Ladepunkte\n'))
  })
  

  
  output$text_1 <- renderText({ "Hier steht eine Erläuterung"
  })
  
  output$text_2 <- renderText({ "Hier steht noch eine Erläuterung"
  })
  
  #output$datahead <- renderTable({
  #  data()
  #})
  
  
 
  
  #-----------
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
    data1()
  })
  
  
  # -------------------------------------
  # Forecast
  
  data_forecast = reactive({
    d = allData %>%
      filter(Bundesland == input$checkBundesland) %>%
      group_by(year, Ladeeinrichtung) %>%
      summarise(total=sum(Ladepunkte)) %>%
      group_by(Ladeeinrichtung) %>%
      mutate(total1 = cumsum(total))
  })
  
  fcast_dates <- (1:6)
  
  countries = sort(unique(allData$Bundesland))[-1]
  
  updateSelectInput(session, "checkBundesland", choices=countries, selected="Baden-Württemberg")
  
  output$forecast <- renderPlotly({
    plot_ly(data = data_forecast(),x=~year,y=~total1, name="Historical", 
            type="scatter", mode = "lines", orientation = 'h' ) %>% 
      add_trace(x=fcast_dates, y = forecast(auto.arima(~year),h=6), 
                name="Forecast", dash="dot")
    #layout(yaxis = yform)
  })
  
  
  # --------------------------------------------
  # Animation
  # Preparation of data for animation of cities
  
  ladepunkte_aggr <- allData
  ladepunkte_aggr$year <- floor_date(ladepunkte_aggr$Inbetriebnahmedatum, "year")
  ladepunkte_aggr <- ladepunkte_aggr %>% filter(Bundesland != 0, year>"2016-01-01", year<"2020-01-01") %>%
    group_by(Bundesland, year) %>%
    summarise(total=sum(Ladepunkte))
  
  ladepunkte_aggr$year <- format(as.Date(ladepunkte_aggr$year, format="%Y-%m-%d"),"%Y")
  
  zulassungen <- select(data_zulassungen, year, Bundesland, Elektro, Elektro_BEV,Hybrid)
  
  df_merged <- merge(x = zulassungen, y = ladepunkte_aggr, by = c("year", "Bundesland") , all.x = TRUE)
  df_merged <- df_merged %>% filter(Bundesland!="Sonstige")
  names(df_merged) <- c("Jahr", "Bundesland", "Zulassung_BEV", "Elektro_BEV", "Hybrid", "Anzahl_Ladepunkte")
  #df_merged$Jahr <- format(as.Date(df_merged$Jahr, format="%Y-%m-%d"),"%Y")
  
  data_merged = reactive({
    d = df_merged %>%
      filter(Jahr %in% input$jahr_zulassungen)
  })
  
  #fit1 <- lm(total ~ Elektro, data = data_merged())
  
  #jahre = sort(unique(df_merged$year))[-1]
  
  #updateSelectInput(session, "jahr_zulassungen", choices=jahre)
  
 
  
  output$zulassungen_plot <- renderPlotly({
    
    fit1 <- lm(Anzahl_Ladepunkte ~ Zulassung_BEV, data = data_merged())
    
    g <- ggplot(data_merged(), aes(x =Anzahl_Ladepunkte, y = Zulassung_BEV, colour=Jahr, label=Bundesland)) + 
      geom_point() +
      stat_smooth(method = "lm", col = "grey") +
      #ggtitle("Verhältnis: Fahrzeugzulassungen & Ladepunkte\n") +
      labs( 
        #title = "Verhältnis: Fahrzeugzulassungen & Ladepunkte\n",
           title  = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                         ", Intercept =",signif(fit1$coef[[1]],5 ),
                         ", Slope =",signif(fit1$coef[[2]], 5),
                         ", P =",signif(summary(fit1)$coef[2,4], 5)),
           x="Ladepunkte", y="Neuzulassungen") +
      theme(legend.title = element_blank(), plot.title = element_text(size=8))
  })
 
  output$text_11 <- renderText({ "Hier steht eine Erläuterung"
  })
  
  output$text_22 <- renderText({ "Hier steht noch eine Erläuterung"
  })
  
  #output$zulassungen_plot <- renderPlotly({
  #  plot_ly(data = df_merged, x = ~total, y = ~Elektro, type = "scatter", color= ~year, abline) +
  #  abline(lm(total ~ Elektro))
  #})
  
  
  data2 <- allData %>%
    filter(year > 2010) %>%
    group_by(year, Ort) %>%
    summarise(total1 = sum(Ladepunkte)) %>%
    arrange(desc(total1))
  
  data2 <- data2 %>% 
    group_by(Ort) %>%
    arrange(year) %>%
    mutate(total2 = cumsum(total1)) %>%
    group_by(year) %>%
    arrange(desc(total2)) %>%
    slice(1:10)
           
  
  data2a <- data2 %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-total2),
           Value_rel = total2/total2[rank==1],
           Value_lbl = paste("",total2)) %>%
    group_by(Ort) %>%
    filter(rank <=10) 
  
  data3 <- allData %>%
    filter(year > 2010) %>%
    group_by(year, Bundesland) %>%
    summarise(total1 = sum(Ladepunkte)) %>%
    arrange(desc(total1))
  
  data3 <- data3 %>% 
    group_by(Bundesland) %>%
    arrange(year) %>%
    mutate(total2 = cumsum(total1)) %>%
    group_by(year) %>%
    arrange(desc(total2)) %>%
    slice(1:10)

  
  data3a <- data3 %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-total2),
           Value_rel = total2/total2[rank==1],
           Value_lbl = paste("",total2)) %>%
    group_by(Bundesland) %>%
    filter(rank <=10)
  
  
  updateSelectInput(session, "animation_option", choices=list_choices, selected="Top 10 Städte")
  
  animation_option <- eventReactive(input$run_button,input$animation_option)
  
  output$animatedplot <- renderImage({
    
    if ( input$animation_option == "Top 10 Städte") { 
      return(list(
        src = "www/images/gif1/top_staedte.gif",
        contentType = "image/gif",
        width = 590,
        height = 400,
        alt = "Ranking der besten 10 Städte"
      ))
    } else if (input$animation_option == "Top 10 Bundesländer") {
      return(list(
        src = "www/images/gif1/top_bundeslaender.gif",
        filetype = "image/gif",
        width = 590,
        height = 400,
        alt = "Ranking der besten 10 Bundesländer"
      ))
    } else if (input$animation_option == "Top 10 Bundesländer") {
      return(list(
        src = "www/images/gif1/top_bundeslaender.gif",
        filetype = "image/gif",
        width = 590,
        height = 400,
        alt = "Ranking der besten 10 Bundesländer"
      ))
    } else if (input$animation_option == "Top 10 Bundesländer") {
      return(list(
        src = "www/images/gif1/top_bundeslaender.gif",
        filetype = "image/gif",
        width = 590,
        height = 400,
        alt = "Ranking der besten 10 Bundesländer"
      ))
    }
    
  }, deleteFile = FALSE)
  

  observeEvent(input$run_button,{
    
    if (input$animation_option == "Top 10 Städte") {
      
      output$animatedplot <- renderImage({
         

        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext=".gif")
        
        # now make the animation
        p = ggplot(data2a, aes(rank, group = Ort, 
                               fill = as.factor(Ort), color = as.factor(Ort))) +
          geom_tile(aes(y = total2/2,
                        height = total2,
                        width = 0.9), alpha = 0.8, color = NA) +
          geom_text(aes(y = 0, label = paste(Ort, " ")), vjust = 0.2, hjust = 1) +
          geom_text(aes(y=total2,label = Value_lbl, hjust=0)) +
          coord_flip(clip = "off", expand = FALSE) +
          scale_y_continuous(labels = scales::comma) +
          scale_x_reverse() +
          guides(color = "none", fill = "none") +
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x = element_line( size=.1, color="grey" ),
                panel.grid.minor.x = element_line( size=.1, color="grey" ),
                plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1),
                plot.subtitle=element_text(size=13, hjust=0.5, face="italic", color="grey", vjust=-1),
                plot.caption =element_text(size=11, hjust=0.5, face="italic", color="grey"),
                plot.background=element_blank(),
                plot.margin = margin(2,2, 2, 4, "cm")) +
          transition_states(year, transition_length = 5, state_length = 5, wrap = FALSE) +
          view_follow(fixed_x = TRUE)  +
          labs(title = 'Ladepunkte pro Jahr : {closest_state}',  
               subtitle  =  " ",
               caption  = "Anzahl der Ladepunkte (summiert)")
        
        anim_save("outfile.gif", animate(p, 200, fps = 20, width=600, height = 420, 
                                         renderer = gifski_renderer("gganimate.gif"))) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             #width = 400,
             #height = 800
             # alt = "This is alternate text"
        )}, deleteFile = TRUE)
    
      
    }
    else {
      output$animatedplot <- renderImage({

        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext=".gif")
        
        # now make the animation
        p = ggplot(data3a, aes(rank, group = Bundesland, 
                               fill = as.factor(Bundesland), color = as.factor(Bundesland))) +
          geom_tile(aes(y = total2/2,
                        height = total2,
                        width = 0.9), alpha = 0.8, color = NA) +
          geom_text(aes(y = 0, label = paste(Bundesland, " ")), vjust = 0.2, hjust = 1) +
          geom_text(aes(y=total2,label = Value_lbl, hjust=0)) +
          coord_flip(clip = "off", expand = FALSE) +
          scale_y_continuous(labels = scales::comma) +
          scale_x_reverse() +
          guides(color = "none", fill = "none") +
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x = element_line( size=.1, color="grey" ),
                panel.grid.minor.x = element_line( size=.1, color="grey" ),
                plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
                plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
                plot.caption =element_text(size=11, hjust=0.5, face="italic", color="grey"),
                plot.background=element_blank(),
                plot.margin = margin(2,2, 2, 4, "cm")) +
          transition_states(year, transition_length = 5, state_length = 5, wrap = FALSE) +
          view_follow(fixed_x = TRUE)  +
          labs(title = 'Ladepunkte pro Jahr : {closest_state}',  
               subtitle  =  " ",
               caption  = "Anzahl der Ladepunkte (summiert)")
        
        anim_save("outfile.gif", animate(p, 200, fps = 20, width=600, height = 420, 
                                         renderer = gifski_renderer("gganimate.gif"))) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, 
        deleteFile = FALSE) 
    
    }
  })
  
  #-----------------------
  #Animierte Zeitleiste
  
  data_zeit = reactive({
    d = allData %>%
      filter(Bundesland == input$country_zeit) %>%
      group_by(year, Ladeeinrichtung) %>%
      summarise(total=sum(Ladepunkte)) %>%
      group_by(Ladeeinrichtung) %>%
      mutate(total1 = cumsum(total))
  })
  
  countries = sort(unique(allData$Bundesland))[-1]
  
  updateSelectInput(session, "country_zeit", choices=countries, selected="Baden-Württemberg")
  
  observeEvent(input$run_button_zeit,{
  output$lineplot <- renderImage({
    
    outfile <- tempfile(fileext=".gif")
    
    plot = ggplot(
      data = data_zeit(),
      aes(year, total1, group=Ladeeinrichtung, color = Ladeeinrichtung)) +
      geom_line(size = 1) +
      geom_point(aes(group = seq_along(year))) +
      #scale_color_viridis_d() +
      labs(x = "Jahr", y = "Ladepunkte") +
      theme(legend.position = "bottom",
            legend.title = element_text(size=11, face="italic", colour = "grey"),
            axis.text=element_text(size=11, colour = "grey46"),
            axis.title =element_text(size=11, hjust=0.5, color="grey46"),
            legend.text=element_text(size=11, face="italic", colour = "grey"),
            plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1),
            plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),) +
      transition_reveal(readr::parse_number(year)) +
      labs(title = 'Wachstum der Ladepunkte pro Jahr',
           subtitle  =  " ")
    
    anim_save("outfile.gif", animate(plot, 200, fps = 20, width=590, height = 420)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         #width = 400,
         #height = 800
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  })
  
  
  countries = sort(unique(allData$Bundesland))[-1]
  
  updateSelectInput(session, "country_zeit1", choices=countries, selected="Baden-Württemberg")
  
    output$lineplot1 <- renderImage({
  
      if ( input$country_zeit1 == "Baden-Württemberg") { 
        return(list(
          src = "www/images/gif2/baden_wuerttemberg.gif",
          fileType = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Baden-Württemberg"
        ))
      } else if (input$country_zeit1 == "Bayern") {
        return(list(
          src = "www/images/gif2/bayern.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Bayern"
        ))
      } else if (input$country_zeit1 == "Berlin") {
        return(list(
          src = "www/images/gif2/berlin.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Berlin"
        ))
      } else if (input$country_zeit1 == "Brandenburg") {
        return(list(
          src = "www/images/gif2/brandenburg.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Brandenburg"
        ))
      } else if (input$country_zeit1 == "Bremen") {
        return(list(
          src = "www/images/gif2/bremen.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Bremen"
        ))
      } else if (input$country_zeit1 == "Hamburg") {
        return(list(
          src = "www/images/gif2/hamburg.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Hamburg"
        ))
      } else if (input$country_zeit1 == "Hessen") {
        return(list(
          src = "www/images/gif2/hessen.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Hessen"
        ))
      } else if (input$country_zeit1 == "Mecklenburg-Vorpommern") {
        return(list(
          src = "www/images/gif2/mecklenburg_vorpommern.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Mecklenburg-Vorpommern"
        ))
      } else if (input$country_zeit1 == "Niedersachsen") {
        return(list(
          src = "www/images/gif2/niedersachsen.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Niedersachsen"
        ))
      } else if (input$country_zeit1 == "Nordrhein-Westfalen") {
        return(list(
          src = "www/images/gif2/nordrhein_westfalen.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Nordrhein-Westfalen"
        ))
      } else if (input$country_zeit1 == "Rheinland-Pfalz") {
        return(list(
          src = "www/images/gif2/rheinland_pfalz.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Rheinland-Pfalz"
        ))
      } else if (input$country_zeit1 == "Saarland") {
        return(list(
          src = "www/images/gif2/saarland.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Saarland"
        ))
      } else if (input$country_zeit1 == "Sachsen") {
        return(list(
          src = "www/images/gif2/sachsen.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Sachsen"
        ))
      } else if (input$country_zeit1 == "Sachsen-Anhalt") {
        return(list(
          src = "www/images/gif2/sachsen_anhalt.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Sachsen-Anhalt"
        ))
      } else if (input$country_zeit1 == "Schleswig-Holstein") {
        return(list(
          src = "www/images/gif2/schleswig_holstein.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Schleswig-Holstein"
        ))
      } else if (input$country_zeit1 == "Thüringen") {
        return(list(
          src = "www/images/gif2/thueringen.gif",
          filetype = "image/gif",
          width = 590,
          height = 400,
          alt = "Entwicklung der Ladepunkte in Thüringen"
        ))
      }
  
    }, deleteFile = FALSE)
  
  
  
})