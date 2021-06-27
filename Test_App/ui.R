

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Shiny File Upload Example"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file","Upload the file"),
            h5("Max file size: 5MB"),
            radioButtons("sep", "Separator", choices = c(Comma = ",", Period =".", Minus ="-")),
            checkboxInput("header","Header?"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tableOutput("inputfile")
        )
    )
))

