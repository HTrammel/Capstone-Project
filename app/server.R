#server.R

library(shiny)
library(ggplot2)
library(DT)
library(caret)
library(randomForest)
library(gbm)
library(dplyr)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

set.seed(1972)

setwd(".")

shinyServer(function(input, output, session) {

    
})
