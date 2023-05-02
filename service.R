# Import libraries
library(shiny)
library(shinythemes)
library(RCurl)
library(plotly)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(GGally)
library(reshape2)
library(plotly)
library(reactable)
library(ggpubr)
library(gghalves)
library(markdown)
library(nnet)
library(readr)
library(tidyr)
library(dplyr)
library(DT)
setwd("/media/hoangnguyen/d1t/data/Document/Shinyapp_EC")

## import data 
train <- read.csv("train.csv")
test <- read.csv("test.csv")
## create model
source("model.R")


## Predict and make data.table
Predict <- round(as.data.frame(predict(model, test, type="prob")),4)
Predict$Prediction <- names(Predict)[1:4][apply(Predict[,1:4], 1, which.max)]
new_data <-Predict %>% mutate(highest_degree = pmax(UCEC_CN_HIGH, UCEC_CN_LOW, UCEC_MSI, UCEC_POLE,na.rm = TRUE))
head(new_data)
## make data for plot 
new_data$number <- seq(1,nrow(new_data),1)
h <- rep(paste0(seq(1,nrow(new_data),1),"_", new_data$Prediction),each=4)
c <- t(new_data[,c(1:4)])
mc <- melt(c)
dotplot1 <- as.data.frame(cbind(mc,h))
dotplot1 <- dotplot1[,-2]
colnames(dotplot1) <- c("Score_of_Classes", "Scores", "Predict")
samples <- paste0(seq(1,nrow(new_data),1),"_", Predict$Prediction)
dotplot1$Predict<- factor(dotplot1$Predict,levels=samples)



plot_ly(
  data = dotplot1,
  x = ~Predict,
  y = ~Scores,
  marker = list(size = 10),
  type = "scatter",
  mode = "markers",
  color = ~Score_of_Classes
)%>%
  layout(barmode="overlay",
         title = "Probaility of each class",
         xaxis = list(tickangle = 90),
         legend = list(orientation = "h",  
                       xanchor = "center",
                       x = 0.5, y=- 0.4))


datatable(
  new_data [,1:5],
  caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;','Scores in details and prediction'),
  options = list(
    scrollX = TRUE,
    scrollY = "250px"))        

head(new_data)






