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


ui <- fluidPage(theme = shinytheme("spacelab"),
                
                navbarPage("UCEC Prediction",
                           tabPanel("Welcome",
                                    titlePanel("About the study"), 
                                    div(includeMarkdown("https://github.com/EFPTTT-THYROID/Shiniapp/raw/main/about.md"))),
                           tabPanel("Multi-Prediction",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      # give name
                                      textInput("txt2", "Given Full Name:", ""), # txt: input
                                      # give age 
                                      numericInput("number", "Number of samples:", ""), # number 
                                      # give sex 
                                      selectInput("type2", label = "Type of samples:", 
                                                  choices = list("Tissue" = "Tissues", "Blood" = "Blood"), 
                                                  selected = "Tissue"), # type 
                                      # input file 
                                      fileInput("file2", "Choose CSV File", accept = ".csv"),
                                      checkboxInput("header", "Header", TRUE),
                                      actionButton("submitbutton2", "Run Anaysis", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      h1("Infomation"),
                                      verbatimTextOutput('contents5'), # output Name 
                                      verbatimTextOutput('contents6'), # output Number of sample 
                                      verbatimTextOutput('contents7'), # output with type
                                      h1("Data input"),
                                      fluidPage(DTOutput('tbl')),# table of file input 
                                      h1("Results"),
                                      fluidRow(
                                        box(width =500, plotlyOutput("graph")),
                                        box(height = 500, width = 500,
                                            DT::dataTableOutput("table")
                                        ))
                                    )),
                           tabPanel("About", 
                                    titlePanel("About the study"), 
                                    div(includeMarkdown("https://github.com/EFPTTT-THYROID/Shiniapp/raw/main/about.md"), align="justify"),
                                    uiOutput("image1")
                           )
                ))


server <- function(input, output) {
  
  ### multi-prediction 
  observeEvent(input$submitbutton2, {
    # output with name
    output$contents5 <- renderText({input$txt2})
    # output with number of sampple 
    output$contents6 <- renderText({input$number})
    # output with type
    output$contents7 <- renderText({input$type2})
    # out put put the file input 
    output$tbl <-  renderDT({
          file <- input$file2
          ext <- tools::file_ext(file$datapath)
          req(file)
          validate(need(ext == "csv", "Please upload a csv file"))
          test1 <-as.data.frame(fread(file$datapath, header = input$header))
          rownames(test1) <- paste('Sample', seq(1,nrow(test1),1)) 
          test1 <- round(test1[1:13],4)
          # create 19 breaks and 20 rgb color values ranging from white to red
          brks <- quantile(test1[1:13], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            {paste0("rgb(255,", ., ",", ., ")")}
          datatable(
            test1 [,1:13],
            caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black;  font-size:150% ;','DNA methylation of 13 CpGs') ,
            options = list(
              scrollX = TRUE,
              scrollY = "300px"
            ))  %>% formatStyle(names(test1[1:13]), backgroundColor = styleInterval(brks, clrs))
        })
    #Output plot 
    testdata <- reactive({
            inFile <- input$file2
            if (is.null(inFile)) return(NULL)     
            else read.csv(input$file2$datapath,
                          header = TRUE)
          })
          dotplot1 <- reactive({
            testdata <- testdata()
            req(input$file2)
      # creat model
      #setwd("/media/hoangnguyen/d1t/data/Document/Shinyapp_EC")
      ## create model
      source("https://raw.githubusercontent.com/projectt88/EC_project/main/model.R")
      # Predict and make data.table
      Predict <- round(as.data.frame(predict(model, testdata, type="prob")),4)
      Predict$Prediction <- names(Predict)[1:4][apply(Predict[,1:4], 1, which.max)]
      new_data <-Predict %>% mutate(highest_degree = pmax(UCEC_CN_HIGH, UCEC_CN_LOW, UCEC_MSI, UCEC_POLE,na.rm = TRUE))
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
      list(new_data=new_data,dotplot1=dotplot1)
    })
    output$graph <- renderPlotly({
      req(input$file2)
      dotplot1 <- dotplot1()$dotplot1
      print(dotplot1)
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
               title = "Probability Plot",
               xaxis = list(tickangle = 90),
               legend = list(orientation = "h",  
                             xanchor = "center",
                             x = 0.5, y=- 0.4))
      
    })
    output$table <- renderDataTable({
      req(input$file2)
      new_data <-dotplot1()$new_data
      rownames(new_data) <- paste('Sample', seq(1,nrow(new_data),1)) 
      datatable(
        new_data [,1:5],
        extensions = 'Buttons',
        caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;','Probability of each subtype and Prediction'),
        options = list(
          scrollX = TRUE,
          scrollY = "300px",
          dom = 'Bltp', 
          buttons= c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 7
            )
      
      )%>%
      formatStyle('Prediction',
                  fontWeight = "bold",
                  backgroundColor = styleEqual(c('UCEC_CN_HIGH', 'UCEC_CN_LOW', 'UCEC_MSI', 'UCEC_POLE'),
                                               c('lightgreen','orange','lightblue','lightpink'))
       )%>%
      formatStyle(
          c('UCEC_CN_HIGH', 'UCEC_CN_LOW', 'UCEC_MSI', 'UCEC_POLE'),
          background = styleColorBar( c(0,max(new_data[,c(1:4)],na.rm=T)*2) , 'darkred'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center',
          color = JS("value < 0 ? 'darkred' : value > 0? 'black' : 'black'")
          
      )%>%
        formatPercentage( c('UCEC_CN_HIGH', 'UCEC_CN_LOW', 'UCEC_MSI', 'UCEC_POLE'),digit = 1 ) %>%
        formatStyle( columns = c('UCEC_CN_HIGH', 'UCEC_CN_LOW', 'UCEC_MSI', 'UCEC_POLE'), `font-size`= '115%' )
      
    })
  })

}


# Create the shiny app             
shinyApp(ui = ui, server = server)




