
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

library(quantmod)
shinyUI(pageWithSidebar(
  
  # Title
  headerPanel("Historic Stock Prices"),
  
#   # Sidebar with a with a text input box, and a date selection box
   sidebarPanel(
     textInput("text", label = h3("Enter Ticker"), 
               value = "AAPL"),
    dateInput("date", "Select Valid Date:", value="2014-01-02" ),
    submitButton("Submit") 
    ),
      
  # Print out the Closing price on selected date
   mainPanel(
     h2("Closing Share Price on Selected Date"),
    textOutput("ticker"),
   h3(textOutput("date"))
  )
))
