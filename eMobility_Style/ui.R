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
                            includeHTML("scrollToTop.html"),
                            
                            sidebarLayout(
                              sidebarPanel(h3("Inbetriebnahme von Ladepunkten pro Jahr"),
                                           selectizeInput("year", "Wähle ein Jahr", choices = NULL)),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            leafletOutput("map", width = "100%", height = "100%")                                         
                              )
                            )
                            
                            
                   )),
                   
                   # ----------------------------------
                   # tab panel 3 - Analysen
                   tabPanel("Analysen",
                            sidebarLayout(
                              sidebarPanel(h3("Inbetriebnahme von Ladepunkten pro Bundesland"),
                                           selectizeInput("country", "Wähle Bundesland", choices = NULL)),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Barplot", plotlyOutput("barplot")),
                                            tabPanel("Data", tableOutput("datahead"))),
                              )
                            )
                            #propertyComparison()
                   ),
                   
                   # ----------------------------------
                   # tab panel 4 - About
                   tabPanel("Fahrzeugzulassungen",
                            includeHTML("about.html"),
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
