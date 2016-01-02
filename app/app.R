#server.R

library(shiny)
library(dplyr)
library(stringr)
library(shinythemes)


set.seed(1972)
setwd(".")
voc_df <- readRDS("data/voc_collo.RDS")

checkText <- function(txt) {
    t <- as.character(str_split_fixed(txt, " ", n = 5))
    for (i in 1:5) {
        if (!t[i] == "" && !t[i] == "[0-9]*") {
            m <- c(t[i])
            err <- TRUE
        } else {
            err <- FALSE
        }
    }
    return (c(t[1], t[2], t[3], err))
}

server <- function(input, output, session) {
    inText <- reactive(checkText(input$text1))
    output$inOut <- renderText(paste("You entered:",input$text1, sep=" "))

    output$status <- renderText(inText[5])

    output$guesses <- renderTable(data.frame(word = inText[1:3], freq = c(0,0,0)))
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