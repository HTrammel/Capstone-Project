#server.R

library(shiny)
library(dplyr)
library(shinythemes)

set.seed(1972)

setwd(".")

server <- function(input, output, session) {
    output$inOut <- renderText(paste("You entered:",input$text1,sep=" "))

    output$guesses <- renderTable(data.frame(word=c("I","don't","know"),freq=c(0,0,0)))
}

ui <- navbarPage(
    theme = shinytheme("united"),
    'Can I guess ...',
    tabPanel("Main",
             sidebarLayout(
                 sidebarPanel(
                    textInput('text1', 'Type three words...', value = "..."),
                    submitButton("submit"),
                    actionButton("reset", label = "Clear"),
                    hr()
                 ),
                 mainPanel(
                     textOutput("inOut"),
                     hr(),
                     tableOutput("guesses")
                 )
             )),
    tabPanel("Help",
             fluidRow(column(
                 6,
                 includeMarkdown("help.Rmd")
             ))),
    tabPanel("About",
             fluidRow(column(
                 6,
                 includeMarkdown("about.Rmd")
             )))
)



shinyApp(ui = ui, server = server)