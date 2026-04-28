#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("VTPEH 6270 - Sowmya Srinivasan"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        #Add this below the slider
        radioButtons("color", "Bar color:",
                     choices = c("steelblue", "tomato", "seagreen", "purple"),
                     inline = TRUE)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        bins <- seq(floor(min(faithful$eruptions)), ceiling(max(faithful$eruptions)), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        library(ggplot2)
        
        ggplot(faithful, aes(eruptions)) +
          geom_histogram(breaks = bins, fill = input$color) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
