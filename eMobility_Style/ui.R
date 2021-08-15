#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(install.load)

install_load("shiny","plotly",  "shinycssloaders", " readxl","leaflet", "ggplot2",  "DT", "gganimate","gifski", "shinymaterial", "tidyverse", "directlabels", "lubridate", "data.table", "forecast", "geojson", "plotly", "prophet", "shinydashboard", "shinyWidgets", "dygraphs")




# Define UI for application that draws a histogram
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
                            
                            #fluidRow(column(width=2),
                             #        column(
                            #           
                            #           br(),
                            #           p("Mit rund 400.000 Neuzulassungen im Jahr 2020 befindet sich Deutschland im weltweiten Vergleich nach China auf dem zweiten Platz. Dies bedeutet eine Erhöhung um fast 300% gegenüber dem Vorjahr [1].",style="text-align:justify;color:black;background-color:deepskyblue;padding:15px;border-radius:10px"),
                            #           br(),
                            #           
                            #           p("Der Bestand an E-Fahrzeugen kann bis zu den Jahren 2025 bzw. 2030 deutlich stärker ansteigen als heute angenommen – das zeigen vertrauliche Angaben der befragten Automobilhersteller. Bis zu 14,8 Millionen batterieelektrische E-Fahrzeuge und Plug-In-Hybride könnten 2030 in Deutschland zugelassen sein [2].",style="text-align:justify;color:black;background-color:deepskyblue;padding:15px;border-radius:10px"),
                            #           
                            #           width=8))
                            
                   ),
                   
                
                   
                   # ----------------------------------
                   # tab panel 3 - Analysen
                   tabPanel("Historische Entwicklung",
                            fluidRow(
                              #valueBox(value = "mean_mpg",
                              #         subtitle = "mean of mpg",
                              #         icon =x "tachometer",
                              #         color = "green"),
                              #valueBox(value = "mean_mpg1",
                              #         subtitle = "test",
                              #         icon = "calendar",
                              #         color = "blue"),
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
                                           selectizeInput("country", "Wähle Bundesland", choices = NULL),
                                           #airMonthpickerInput("input_var_month",
                                          #                     label = "Zeitraum",
                                          #                     range=TRUE,
                                           #                    #value = "2020-01-01",
                                            #                  maxDate = "2021-06-01",
                                             #                 minDate = "2008-07-01",
                                              #                #view = "months", #editing what the popup calendar shows when it opens
                                               #               #minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                #              dateFormat = "mm-yyyy",
                                                 #             language ="de"
                                                  #            ),
                                           dateRangeInput("input_date_range",
                                                          label="Zeitraum",
                                                          start = "2016-01-01", 
                                                          end = "2021-01-01",
                                                          min = "2008-07-01",
                                                          max = "2021-06-31",
                                                          format = "dd-mm-yyyy",
                                                          language = "de",
                                                          startview = "year"
                                             
                                           ),
                                          
                                           #sliderInput("DatesMerge",
                                          #             "Dates:",
                                          #             min = as.Date("2008/07/01"),
                                          #             max = as.Date("2021/06/31"),
                                          #             value=c(as.Date("2008/07/01"), as.Date("2021/06/31")),
                                          #             format = "%m %Y"
                                          #        )
                                           
                                           #min = as.Date("2010-01-01"),max =as.Date("2014-12-01"),value=as.Date("2014-12-01"),timeFormat="%b %Y"
                                      
                                           ),
                            
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            #tabPanel("Test", verbatimTextOutput("test")),
                                            tabPanel("Kumuliert", plotlyOutput("kumuliert")),
                                            tabPanel("Distinktiv", plotlyOutput("barplot")),
                                            tabPanel("Erläuterung", textOutput("text_1"), 
                                                     textOutput("text_2"), textOutput("text_3"))),
                              
                            ))
                            
                            
                            
                            
                            
                            #sidebarLayout(
                            #  sidebarPanel(h4("Wachstum der Ladepunkten pro Bundesland"),
                            #               selectizeInput("country_zeit", "Wähle Bundesland", choices = NULL)),
                            #  
                            #  mainPanel(
                            #   tabsetPanel(type = "tabs",
                            #                tabPanel("Lineplot", imageOutput("lineplot")),
                            #                tabPanel("Data", tableOutput("datahead2"))),
                            #  )
                            #),
                            
                            #sidebarLayout(
                            #  sidebarPanel(h4("Top 10 Städte"),
                            #               selectInput("checkYear", "Wähle Jahr", choices = NULL)
                            #  ),
                            #  
                            #  mainPanel(
                            #    tabsetPanel(type = "tabs",
                            #                tabPanel("Barplot", plotlyOutput("barplot1")),
                            #                tabPanel("Data", tableOutput("datahead1"))),
                            #  )
                            #),
                            
                            #sidebarLayout(
                            #  sidebarPanel(h4("Inbetriebnahme der Ladepunkte"),
                            #               selectInput("checkBundesland", "Wähle Bundesland", choices = NULL)
                            #  ),
                            #  
                            #  mainPanel(
                            #    tabsetPanel(type = "tabs",
                            #                tabPanel("Forecast", plotlyOutput("forecast")),
                            #                tabPanel("Data", tableOutput("datahead2"))),
                            #  )
                            #)
                            #propertyComparison()
                   ),
                   
                   
                   
                   # ----------------------------------
                   # tab panel 2 - Ladesäulenkarte
                   tabPanel("Flächenabdeckung",
                            #includeHTML("scrollToTop.html"),
                            fluidRow(
                              #valueBox(value = "mean_mpg",
                              #         subtitle = "mean of mpg",
                              #         icon = "tachometer",
                              #         color = "green"),
                              #valueBox(value = "mean_mpg1",
                              #         subtitle = "test",
                              #         icon = "calendar",
                              #         color = "blue"),
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
                              sidebarPanel(sliderInput("Jahr2", label = h4("Entwicklung der Inbetriebnahme öffentlich zugänglichen Ladepunkte"),
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
                                            #tabPanel("Test", verbatimTextOutput("test")),
                                            tabPanel("pro 100qkm",leafletOutput("m")),
                                            tabPanel("pro 100.000 Einwohner", leafletOutput("map_einwohner")),
                                            tabPanel("Total", leafletOutput("map2")),
                                            tabPanel("Erläuterung", textOutput("text_map"))),
                                
                                
                

                              )
                              
                            ),
                            
                      
                   
                  
                   
                   ),
                   
                   
                   tabPanel("Ladepunkt & Neuzulassungen",
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  checkboxGroupInput("jahr_zulassungen", "Jahr auswählen", c("2017" = "2017",
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
                   
                   
                   # tab panel 4 - About
                   tabPanel("Prognosen",
                            
                            
                            
                            #sidebarLayout(
                            #  sidebarPanel(
                            #    selectInput("animation_option","Ranking auswählen",choices=NULL)
                            #  ),
                            #  mainPanel(
                                #textOutput("test")
                                #imageOutput("animatedplot")
                                #imageOutput("animatedplot2")
                             # )
                            #),
                            
                            
                            sidebarLayout(
                              sidebarPanel(
                                imageOutput("fc")
                              ),
                              mainPanel(
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("Entwicklung bis Mitte 2021", selectInput("country_zeit1", "Wähle Bundesland", choices = NULL), imageOutput("lineplot1")),
                                            tabPanel("Ausblick in die Zukunft", textOutput("text_dauer"), plotOutput("prophet")),
                                            tabPanel("Erläuterung", textOutput("text_forecasting"))),
                                
                              )
                            ),
                            
                            #sidebarLayout(
                            #  sidebarPanel(
                            #    h4("Wachstum der Ladepunkten pro Bundesland"),
                            #    selectInput("country_zeit", "Wähle Bundesland", choices = NULL),
                            #    actionButton("run_button_zeit","Berechnen (ca. 1 Min.)",icon=icon("play"))
                            #  ),
                            #  mainPanel(
                            #    imageOutput("lineplot")
                            #  )
                            #),
                            
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
