#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(install.load)

#install_load("shiny","plotly",  "shinycssloaders", " readxl","leaflet", "ggplot2",  "DT", "gganimate","gifski", "shinymaterial", "tidyverse", "directlabels", "lubridate", "data.table", "forecast", "geojson", "plotly", "prophet", "shinydashboard", "shinyWidgets", "dygraphs")
install_load("shiny","plotly",  "shinycssloaders", "readxl","leaflet", "ggplot2",  "DT", 
             "gganimate","gifski", "shinymaterial", "tidyverse", "directlabels", "lubridate", 
             "data.table", "forecast", "geojson", "geojsonio", "prophet", "shinydashboard", 
             "shinyWidgets", "dygraphs", "readr", "shinyjs", "scales")

shinyUI(navbarPage(title = "Ladeinfrastruktur Deutschland",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                  
                  
      
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon_e.png")
                            ),
                            
                   ),
                   
                
                   
                   # ----------------------------------
                   # tab panel 2 - Historische Entwicklung
                   tabPanel("Historische Entwicklung",
                            fluidRow(
                             
                              infoBox(
                                uiOutput("orderNum"), "Veränderung in %", icon = icon("chart-line", lib = "font-awesome")
                              ),
                              infoBox(
                                uiOutput("orderNum1"), "Investitionskosten", icon = icon("euro-sign", lib = "font-awesome")
                              ),
                              infoBox(
                                uiOutput("orderNum2"), "Verhätnis Schnell/Normal", icon = icon("credit-card")
                              )
                            ),
                            
                            
                            
                            sidebarLayout(
                              sidebarPanel(h4("Inbetriebnahme von Ladepunkten pro Bundesland"),
                                           selectizeInput("country", "Wähle Bundesland:", choices = NULL),
                                        
                                           dateRangeInput("input_date_range",
                                                          label="Wähle Zeitraum:",
                                                          start = "2016-01-01", 
                                                          end = "2021-01-01",
                                                          min = "2008-07-01",
                                                          max = "2021-06-31",
                                                          format = "dd-mm-yyyy",
                                                          language = "de",
                                                          startview = "year"
                                             
                                           ),
                                          
                                           ),
                            
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            #tabPanel("Test", verbatimTextOutput("test")),
                                            tabPanel("Kumuliert", plotlyOutput("kumuliert")),
                                            tabPanel("Distinktiv", plotlyOutput("barplot")),
                                            tabPanel("Erläuterung", textOutput("text_1"), 
                                                     textOutput("text_2"), textOutput("text_3"))),
                              
                            ))
                            
                   ),
                   
                   
                   
                   # ----------------------------------
                   # tab panel 3 - Ladesäulenkarte
                   tabPanel("Flächenabdeckung",
                            #includeHTML("scrollToTop.html"),
                            fluidRow(
                             
                              infoBox(
                                uiOutput("mapInfo"), "Pro 1000 qkm", icon = icon("road", lib = "font-awesome")
                              ),
                              
                              #Summe Ladepunkte
                              infoBox(
                                uiOutput("mapInfo1"), "Pro 1 Mio Einwohner", icon = icon("users", lib = "font-awesome")
                              ),
                              
                              #Verhältnis Schnell / Normal 
                              infoBox(
                                uiOutput("mapInfo2"), "Deutschlandweit", icon = icon("globe", lib = "font-awesome")
                              )
                            ),
                            
                            
                            
                            sidebarLayout(
                              sidebarPanel( h4("Entwicklung der Inbetriebnahme öffentlich zugänglichen Ladepunkte"), 
                                            sliderInput("Jahr2", label = "Wähle Jahr:",
                                                       min = 2008,
                                                       max = 2021,
                                                       step = 1,
                                                       value = 2021,
                                                       sep ='',
                                                       animate = animationOptions(interval = 2200, loop = FALSE)
                              )
                              ),
                              
                              mainPanel(
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("Pro 100qkm",leafletOutput("m")),
                                            tabPanel("Pro 100.000 Einwohner", leafletOutput("map_einwohner")),
                                            tabPanel("Deutschlandweit", leafletOutput("map2")),
                                            tabPanel("Erläuterung", textOutput("text_map"))),
                                
                                
                

                              )
                              
                            )

                   
                   ),
                   
                   
                   tabPanel("Ladepunkt & Neuzulassungen",
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(h4("Zusammenhang zwischen Ladepunkten & Neuzulassungen BEV"),
                                  checkboxGroupInput("jahr_zulassungen", "Jahr auswählen:", c("2017" = "2017",
                                                                                             "2018" = "2018",
                                                                                             "2019" = "2019"), selected = "2019")
                                ),
                                mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Regressionsanalyse", plotlyOutput("zulassungen_plot")),
                                              tabPanel("Erläuterung", textOutput("text_22"))
                                  )
                                ))
                            )),
                   
                   
                   # tab panel 4 - Prognosen
                   tabPanel("Prognosen",
                          
                            sidebarLayout(
                              sidebarPanel(
                                textOutput("text_development")
                              ),
                              mainPanel(
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("Entwicklung bis Mitte 2021", imageOutput("lineplot_development")),
                                            tabPanel("Ausblick in die Zukunft",imageOutput("forecast_total")),
                                            tabPanel("Erläuterung", textOutput("text_forecasting"))),
                                
                              )
                            ),
                            
                  
                            
                            useShinydashboard(),
                            
                            #includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/carousel.css"),
                              tags$script(src = "plugins/holder.js"),
                              tags$style("#orderNum{font-size: 29px}"),
                              tags$style("#orderNum1{font-size: 29px}"),
                              tags$style("#orderNum2{font-size: 29px}"),
                              tags$style("#text_1{font-size: 20px}"),
                              tags$style("#text_2{font-size: 15px}"),
                              tags$style("#text_development{font-size: 20px}"),
                              tags$style("#text_11{font-size: 20px}"),
                              tags$style("#text_22{font-size: 15px}")),
                        
                              tags$style(HTML("
                    label > input[type='radio'] {
                        opacity: 0;
                        position: absolute;
                    }
                    label > input[type='radio'] + *::before {
                        content: '';
                        margin: 4px 0 0;
                        width: 13px;
                        height: 13px;
                        position: absolute;
                        margin-left: -20px;
                        border-radius: 50%;
                        border-style: solid;
                        border-width: 0.1rem;
                        border-color: #282828;
                    }
                    label > input[type='radio']:checked + *::before {
                        background: radial-gradient(white 0%, white 30%, #282828 30%, #282828);
                                border-color: #282828;
                    }
                    label > input[type='checkbox'] {
                        opacity: 0;
                        position: absolute;
                    }
                    label > input[type='checkbox'] + *::before {
                      content: '';
                      position: absolute;
                      margin: 4px 0 0;
                      margin-left: -20px;
                      align: center;
                      width: 13px;
                      height: 13px;
                      margin-right: 1rem;
                      border-radius: 0%;
                      border-style: solid;
                      border-width: 0.1rem;
                      border-color: #282828;
                    }
                    label > input[type='checkbox']:checked + *::before {
                      content: '';
                      width: 13px;
                      height: 13px;
                      background-color: #282828;
                    }
                  ")),
                              tags$style("#mapInfo{font-size: 29px}"),
                              tags$style("#mapInfo1{font-size: 29px}"),
                              tags$style("#mapInfo2{font-size: 29px}")
                            ),

                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                                       
                                       
                                       #HTML(".fa{font-size: 24px;}")
                                       
                            )
                            
                            
                   )
                   
)
