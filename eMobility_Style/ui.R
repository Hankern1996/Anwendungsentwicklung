#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(leaflet)
library(shinycssloaders)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "eLectrify",
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
                            )
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Ladesäulenkarte
                   tabPanel("Ladesäulenkarte",
                            #includeHTML("scrollToTop.html"),
                            
                            sidebarLayout(
                              sidebarPanel(h4("Inbetriebnahme von Ladepunkten pro Jahr"),
                                           selectInput("Jahr", "Wähle ein Jahr", choices = NULL)
                                           ),
                              
                              mainPanel(
    
                                            leafletOutput("map")

                            )
                            
                   ),
                   
                   sidebarLayout(
                     sidebarPanel(
                                  sliderInput("dateSel", label = h4("Inbetriebnahme von Ladepunkten Entwicklung"),
                                              min = 2007,
                                              max = 2021,
                                              step = 1,
                                              value = 2007,
                                              animate = animationOptions(interval = 2200, loop = FALSE)
                                  )
                     ),
                     
                     mainPanel(
                       leafletOutput("map2") 
                       
                     )
                     
                   )
                   
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Analysen
                   tabPanel("Analysen",
                            sidebarLayout(
                              sidebarPanel(h4("Inbetriebnahme von Ladepunkten pro Bundesland"),
                                           selectizeInput("country", "Wähle Bundesland", choices = NULL)),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Barplot", plotlyOutput("barplot")),
                                            tabPanel("Data", tableOutput("datahead"))),
                              )
                            ),
                            
                            sidebarLayout(
                              sidebarPanel(h4("Top 10 Städte"),
                                           selectInput("checkYear", "Wähle Jahr", choices = NULL)
                              ),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Barplot", plotlyOutput("barplot1")),
                                            tabPanel("Data", tableOutput("datahead1"))),
                              )
                            )
                            #propertyComparison()
                   ),

                   
                   
                   # tab panel 4 - About
                   tabPanel("Animationen",
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("animation_option","Animation Options",choices=NULL),
                                actionButton("run_button","Berechnen",icon=icon("play"))
                              ),
                              mainPanel(
                                #textOutput("test")
                                withSpinner(imageOutput("animatedplot"))
                                #imageOutput("animatedplot2")
                              )
                            ),
                            
                            #includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/carousel.css"),
                              tags$script(src = "plugins/holder.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                   )
                   
))
