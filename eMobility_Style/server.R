
#library(shiny)
#library(plotly)
#library(readxl)
#library(leaflet)
#library(ggplot2)
#library(tidyverse)
#library(gganimate)
#library(gifski)
#library(shinycssloaders)
#library(directlabels)
#library(forecast)
#library(lubridate)
#library(geojson)
#library(prophet)
#library(Rcpp)


allData <- read_excel("data/Ladesaeulenkarte_v3.xlsx", 
                      col_types = c("text", "text", "text", 
                                    "text", "numeric", "numeric", "date", 
                                    "numeric", "text", "numeric", "text", 
                                    "numeric", "text", "numeric", "text", 
                                    "numeric", "text", "numeric", "text", 
                                    "text"))

data_zulassungen <- read_delim("data/zulassungen_bundesland2.csv", 
                                      "\t", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%Y-%m-%d")), 
                                      trim_ws = TRUE)

data_kosten <- read_excel("data/Kosten_Ladeinfrastruktur.xlsx")


allData$year <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
allData$month <- format(as.Date(allData$Inbetriebnahmedatum, format="%Y-%m-%d"),"%m")
allData$Month <- floor_date(allData$Inbetriebnahmedatum, "month")
data_zulassungen$year <- format(as.Date(data_zulassungen$Datum, format="%Y-%m-%d"),"%Y")

list_choices <- list("Top 10 Städte","Top 10 Bundesländer")

shinyServer(function(input, output, session) {
  
  #-------------------------------
  # Veränderung: Hinzukommende Ladepunkte in Deutschland
  
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
  states <- geojsonio::geojson_read("data/bundeslaender.geojson", what = "sp")
  class(states)
  
  allData_Map <- allData[1:10]
  allData_Map$year <- format(as.Date(allData_Map$Inbetriebnahmedatum, format="%Y-%m-%d"),"%Y")
  
  flaeche <- read_excel("data/Bundesland_flaeche.xlsx",
                        col_types = c("text", "numeric"))

  bins <- c(0,1,2,5, 10, 15, 20,50,100,150, Inf)
  
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
  
  einwohner <- read_excel("data/Bundesland_einwohner.xlsx",
                        col_types = c("text", "numeric"))
  
  bins2 <- c(0,1,2,5, 10, 20,40,50, Inf)
  
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
  
  #weitere Map mit Slider
  
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
  
  
  output$text_map <- renderText({ "Die Ladeinfrastruktur in Deutschland steht unter ständiger Veränderung und rapidem Wachstum. Die vorherigen Darstellungen
    zeigen die Entwicklung der Ladeinfrastruktur in der Vergangenheit. Damit der Ausbau der Ladeinfrastruktur unter den einzelnen Bundesländern vergleichbar gemacht werden kann,
    werden einmal die Anzahl der Ladepunkte in Relation zur Fläche und zur Anzahl der Einwohner gesetzt. Hierfür werden weitere externe Datenpunkte hinzugefügt. 
    Unter dem letztem Tab werden die Ladesäule geografisch angezeigt."
  })
  
  #----------------------
  #Forecasting mit Prophet
  
    output$prophet <- renderPlot({
      
      newData <- read_excel("data/Prediction_Test.xlsx", 
                            col_types = c("date", "numeric", "numeric"))
      
      
      newdata <- newData[c(1,3)]

      
      m <- prophet(newdata, daily.seasonality = TRUE)
      
      future <- make_future_dataframe(m, periods = 365)
      
      forecast <- predict(m,future)
      
      plot(m, forecast, 
           xlab="Zeit", ylab="Ladepunkte") 
      
    })
    
    
    output$forecast_total <- renderImage({
      list(
        src = "www/images/prophet.jpg",
        filetype = "image",
        width = 590,
        height = 400,
        alt = "Entwicklung der Ladeinfrastruktur in der Zukunft",
        deleteFile=TRUE 
      )
      
    }) 
    
    output$fc <- renderImage({
      list(
        src = "www/images/fc.jpg",
        filetype = "image",
        width = 230,
        height = 200,
        alt = "Entwicklung der Ladeinfrastruktur in der Zukunft"
      )
      
    }, deleteFile = FALSE) 
      
    output$text_forecasting <- renderText({"Im Tab <<Entwicklung bis Mitte 2021>> wird die Entwickung der Ladeinfrastruktur in Deutschland als Animation angezeigt. Die Animation bezieht sich auf das jeweils ausgewählte Bundesland.
    Die im zweiten Tab <<Ausblick in die Zukunft>> dargestellte Prognose wird mithilfe des Shiny Packages Prophet durchgeführt. Die Analyse bezieht sich auf Deutschland gesamt."})
    
  #------------------------
  #Analyse_pro_Bundesland   
  
  output$orderNum <- renderText({
    paste("+",as.character(round(veraenderung())),"%")
  })
  
  output$orderNum1 <- renderText({
    paste(as.character(round(cost()/1000)),"T€")
    # Kosten der Ladeinfrastruktur
  })
  
  output$orderNum2 <- renderText({
    paste(as.character(round(verhaeltnis_schnell())),"% /", as.character(round(verhaeltnis_normal())), "%")
    # Verhältnis Schnell/Normal 
  })
  
  preis_schnell <- data_kosten[data_kosten$Hardware_EUR == 5250, "Total_EUR"]/2
  
  preis_normal <- data_kosten[data_kosten$Hardware_EUR == 22500, "Total_EUR"]/2
  
  cost = reactive({
    ((schnelllade_max()-schnelllade_min())*preis_schnell)+((normallade_max()-normallade_min())*preis_normal)
  })
  
  verhaeltnis_schnell = reactive({
    ((schnelllade_max() - schnelllade_min())/(normallade_max() - normallade_min() + schnelllade_max() - schnelllade_min())*100)
  })
  
  verhaeltnis_normal = reactive({
    ((normallade_max() - normallade_min())/(normallade_max() - normallade_min() + schnelllade_max() - schnelllade_min())*100)
  })
  
  schnelllade_min = reactive({
    d <- allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0, Ladepunkte != 0, Inbetriebnahmedatum <= input$input_date_range[1], Ladeeinrichtung == "Schnellladeeinrichtung") %>%
      group_by(Ladeeinrichtung) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      select(total1) 
    
    if (dim(d)[1] == 0) {
      return(0)
    } else {
      return(d)
    }
  })
  
  normallade_min = reactive({
    d <- allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0, Ladepunkte != 0, Inbetriebnahmedatum <= input$input_date_range[1], Ladeeinrichtung == "Normalladeeinrichtung") %>%
      group_by(Ladeeinrichtung) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      select(total1)
    
    if (dim(d)[1] == 0) {
      return(0)
    } else {
      return(d)
    }
  })
  
  schnelllade_max = reactive({
    d <- allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0, Ladepunkte != 0, Inbetriebnahmedatum <= input$input_date_range[2], Ladeeinrichtung == "Schnellladeeinrichtung") %>%
      group_by(Ladeeinrichtung) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      select(total1)
    
    if (dim(d)[1] == 0) {
      return(0)
    } else {
      return(d)
    }
  })
  
  normallade_max = reactive({
    d <- allData %>%
      filter(Bundesland == input$country, Ladeeinrichtung != 0, Ladepunkte != 0, Inbetriebnahmedatum <= input$input_date_range[2], Ladeeinrichtung == "Normalladeeinrichtung") %>%
      group_by(Ladeeinrichtung) %>% 
      summarise(total=sum(Ladepunkte)) %>%
      mutate(total1 = cumsum(total)) %>%
      select(total1)
    
    if (dim(d)[1] == 0) {
      return(0)
    } else {
      return(d)
    }
  })

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

  
  output$text_2 <- renderText({ "Vorliegende Analyse zeigt sowohl die kumulierte als auch distinktive Entwicklung der Inebtrieb genommenen Ladeopunkte in den jeweiligen Bundesländern. Es wird außerdem unterschieden zwischen Schnell- und Normalladestationen. 
    Zudem werden die Kosten des Ladeinfrastrukturausbaus dynamisch angezeigt. Hierzu wird der Ladereport veröffentlich 2020 im Auftrag der EnBW Energie Baden-Württemberg AG hinzugezogen. "
  })

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
  
  year = sort(unique(allData$year))
  
  updateSelectInput(session, "checkYear", choices=year[-1], selected="2020")
  
  output$barplot1 <- renderPlotly({
    plot_ly(data = data1(),y=~Ort,x=~total1, type="bar",  orientation = 'h' ) #%>% 
  })
  
  output$datahead1 <- renderTable({
    data1()
  })

  
  # --------------------------------------------
  # Animation
  
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
           x="Ladepunkte", y="Neuzulassungen BEV") +
      theme(legend.title = element_blank(), plot.title = element_text(size=8))
  })

  
  output$text_22 <- renderText({ "Mithilfe der Regressionsanalyse lässt es sich veranschaulichen, wie gut sich die Werte einer Variable mit den Werten einer anderen Variablen vorhersagen lassen. Außerdem hilft die Analyse dabei,
    die Zusamenhänge der beiden Variablen zu  beschreiben. In vorliegendem Fall wird mithilfe der neu in Betrieb genommenen Ladepunkte die Zulassungen der Elektrofahrzeuge vorhergesagt bzw. Zusammenhänge erkannt. Zur Auswahl stehen die Jahre 2017 - 2019. Für diese Jahre stellt das Kraftfahrtbundesamt die öffentlich zugänglichen Daten zur Verfügung."
  })

  #-----------------------
  #Animierte Zeitleiste
  
  data_zeit = reactive({
    d = allData %>%
      filter(Bundesland != 0, Ladepunkte != 0) %>%
      group_by(year, Ladeeinrichtung) %>%
      summarise(total=sum(Ladepunkte)) %>%
      group_by(Ladeeinrichtung) %>%
      mutate(total1 = cumsum(total))
  })
  
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
            plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey")) +
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
  
  output$text_development <- renderText({ "Die Entwicklung der Ladeinfrastruktur: Vergangenheit bis Zukunft"
  })
  
  output$lineplot_development <- renderImage({
    
      list(
        src = "www/images/development_germany.gif",
        filetype = "image/gif",
        width = 590,
        height = 400,
        alt = "Entwicklungs der Ladeinfrastruktur in der Zukunft",
        deleteFile=TRUE 
      )
      
    })
  
  
  
})