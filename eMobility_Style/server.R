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
library(ggplot2)
library(tidyverse)
library(gganimate)
library(gifski)

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
  
  list_choices <- list("Top 10 Städte","Top 10 Bundesländer")
  
  #Ladesäulenkarte    
  allData_Map <- allData[5:9]
  
  data0 = reactive({
    d0 = allData %>%
      filter(Bundesland == input$Jahr) %>%
      group_by(year, Ladeeinrichtung) %>%
      summarise(total=sum(Ladepunkte))
  })
  
  years = sort(unique(allData$year))
  updateSelectInput(session, "Jahr", choices=years, selected="2008")

  # If you want to use predefined palettes in the RColorBrewer package:
  # Call RColorBrewer::display.brewer.all() to see all possible palettes
  pal <- colorFactor(
    palette = c('red', 'blue'),
    domain = allData_Map$Ladeeinrichtung
  )
  
  
  output$map <- renderLeaflet({
    leaflet(allData_Map) %>%
      addTiles() %>% 
      addCircles( ~Längengrad, ~Breitengrad, weight = 3, radius=40, 
                 color=~pal(Ladeeinrichtung), stroke = TRUE, fillOpacity = 0.8)
    
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
    data1()
  })
  
  # Preparation of data for animation of cities
  
  data2 <- allData %>%
    group_by(year, Ort) %>%
    summarise(total2 = sum(Ladepunkte)) %>%
    arrange(desc(total2)) %>%
    slice(1:10)
  
  data2 <- data2 %>%
    filter(year<2020)
  
  data2 <- data2  %>%
    filter(year>2014)
  
  data2a <- data2 %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-total2),
           Value_rel = total2/total2[rank==1],
           Value_lbl = paste("",total2)) %>%
    group_by(Ort) %>%
    filter(rank <=10) 
  
  data3 <- allData %>%
    group_by(year, Bundesland) %>%
    summarise(total2 = sum(Ladepunkte)) %>%
    arrange(desc(total2)) %>%
    slice(1:10)
  
  data3 <- data3 %>%
    filter(year<2020)
  
  data3 <- data3  %>%
    filter(year>2014)
  
  data3a <- data3 %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-total2),
           Value_rel = total2/total2[rank==1],
           Value_lbl = paste("",total2)) %>%
    group_by(Bundesland) %>%
    filter(rank <=10)
  
  
  updateSelectInput(session, "animation_option", choices=list_choices, selected="Top 10 Städte")
  
  animation_option <- eventReactive(input$run_button,input$animation_option)

  #animation_option <- eventReactive(input$run_button,input$animation_option)

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
                plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
                plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
                plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
                plot.background=element_blank(),
                plot.margin = margin(2,2, 2, 4, "cm")) +
          transition_states(year, transition_length = 5, state_length = 2, wrap = FALSE) +
          view_follow(fixed_x = TRUE)  +
          labs(title = 'Ladepunkte pro Jahr : {closest_state}',  
               subtitle  =  "Top 10 Länder",
               caption  = "Anzahl der Ladepunkte (summiert)")
        
        anim_save("outfile.gif", animate(p)) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, 
        deleteFile = TRUE) 
      
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
                plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
                plot.background=element_blank(),
                plot.margin = margin(2,2, 2, 4, "cm")) +
          transition_states(year, transition_length = 5, state_length = 2, wrap = FALSE) +
          view_follow(fixed_x = TRUE)  +
          labs(title = 'Ladepunkte pro Jahr : {closest_state}',  
               subtitle  =  "Top 10 Bundesländer",
               caption  = "Anzahl der Ladepunkte (summiert)")
        
        anim_save("outfile.gif", animate(p)) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, 
        deleteFile = TRUE) 
    }
    })
  
  
  #Animation of plot

  
  
  

  
})
