# from github.com/yufree/nlpshiny
#
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('models.R')
badwords <- readLines('badword.txt')
load('ngram0.RData')

shinyServer(function(input, output) {
  dataInput <- reactive({
	if(input$radio == 1){
		predict0(input$entry,
				badwords,
				unigramDF,
				bigramDF,
				trigramDF,
				maxResults = input$n)
    }else{
      predictKN(input$entry,
							   badwords,
								  unigramDF,
								  bigramDF,
								  trigramDF,
								  maxResults = input$n)
    }
    })
  
  output$text <- renderText({
    dataInput()
  })
  
  output$sent <- renderText({
    input$entry
  })
})


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  # Set the page title
  titlePanel("Data Science Capstone: Simple SwiftKey"),
  
  sidebarPanel(
    textInput("entry",
        h5("Input the sentence"),
        "Data Science"),
    numericInput("n",
        h5("Numbers of predicted words"), 
        value = 3),
    radioButtons("radio", 
        h5("Smoothing selection"),
        choices = list("Stupid Back-off" = 1, "Kneser-Ney " = 2),
        selected = 1),
    submitButton("SUBMIT"),
    br(),
    img(src = "logo.jpg", height = 50, width = 50),
    "This app is created by ", 
    a("Miao Yu", href = "mailto:yufreecas@gmail.com")
    ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
       tabPanel("Tech"))
))