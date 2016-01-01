# ui.R

library(shiny)
library(dplyr)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

set.seed(1972)

shinyUI(navbarPage(
    ui <- navbarPage(
        'Can I guess ...',
        tabPanel("Main",
                 sidebarLayout(
                     sidebarPanel(
                        textInput('text1', 'Type three words...'),
                        actionButton("submit", "Submit"),
                        actionButton("reset", "Clear"),
                        hr()
                     ),
                     mainPanel(
                         textOutput("tOut")
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
))
