#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)
library(parallel)
library(doParallel)
library(quanteda)
library(sqldf)

source("WordInputs.R")
source("MostLikely(revised)2.R")


# Define server logic required to output predicted word
shinyServer(function(input, output,session) {
  
  dataInput <- reactive({
  datareactive<-reactiveValues()
 
  datareactive<-input$userText
  datareactive
  })
  

  output$PredictedWord <- renderUI({
      
    list_of_words<-word_inputs(dataInput())

    list_to_test<-as.character(unlist(strsplit(list_of_words,",")))
 
    Predicted<-most_likely_word(list_to_test,all_data)
   
    if(class(Predicted)=="data.frame")
      Predicted$y
    else 
    Predicted
   })

   observeEvent(input$reset,{
  
      # Send an update to reset values
     output$PredictedWord <- renderText({""})
     session$reload()
     return()
    
    })
  
})
