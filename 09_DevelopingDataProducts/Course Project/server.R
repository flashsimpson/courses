
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(quantmod)

  shinyServer(function(input, output) {
 # grab data from google server
    x <- reactive({
      getSymbols(input$text, src = "yahoo", 
                         auto.assign = FALSE)
        
    })
      
# subset data based on user input
    output$date<- renderPrint({
     
      d<-input$date
      
      #p<- x()[,4]
      print(x()[d,4])

     
      #print(p[d,1])
    })
      

    })
   

