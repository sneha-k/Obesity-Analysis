#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Obesity Levels 
classification <- c("Underweight", "Normal", "Overweight", "Obesity I", "Obesity II", "Obesity III")
bmiValue <- c("Less than 18.5", "18.5 to 24.9", "25.0 to 29.9", "30.0 to 34.9", "35.0 to 39.9", "Higher than 40.0")
obesityLevel <- data.frame(classification, bmiValue)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$whoObesity <- renderTable(obesityLevel, align = "c")

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })

})
