#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#   https://shiny.rstudio.com/gallery/custom-input-bindings.html


library(shiny)

fluidPage(
  titlePanel("What will I say next?"),
  
  fluidRow(
    column(4, wellPanel(
      textInput("userText", "Enter some text here!", ""),
      actionButton("reset", "Clear")
    )),
    column(8, wellPanel(
      uiOutput("PredictedWord") #tried HTMLOutput as well
    ))
  )
)
