# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(plotly)
library(shinydashboard)
library(shiny)
library(data.table)
library(fastDummies)
library(ggplot2)
library(multiROC)
library(caret)
library(pROC)
library(GGally)
library(glmnet)
library(randomForest)
library(reshape2)
library(DT)
library(plotly)
library(reactable)
library(ggpubr)
library(gghalves)
library(markdown)
urlfile<-'https://raw.githubusercontent.com/EFPTTT-THYROID/Shiniapp/main/traindata_model.tsv?token=GHSAT0AAAAAAB66ILNURVI3FNAG3LMM7MK6Y7QLJ4A'
traindata<- read.csv(url(urlfile), sep = '\t')

#traindata <- as.data.frame(fread('/home/minhhoang/Documents/Thyroid cancer/TOTAL/Build_model/Data/traindata_model.tsv'))

ui <- fluidPage(theme = shinytheme("lumen"),
                
                navbarPage("EFPTT Prediction",
                           
                           tabPanel("Personal Prediction",
                                    
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      # give name
                                      textInput("txt", "Given Full Name:", ""), # txt: input
                                      # give age 
                                      numericInput("age", "Age:", ""), # age
                                      # give sex 
                                      selectInput("sex", label = "Sex:", 
                                                  choices = list("Male" = "Male", "Female" = "Female"), 
                                                  selected = "TRUE"), # sex 
                                      #Give type of samples 
                                      selectInput("type ", label = "Type of samples:", 
                                                  choices = list("Tissue" = "Tissues", "Blood" = "Blood"), 
                                                  selected = "Tissue"),
                                      # input file 
                                      fileInput("file1", "Choose CSV File", accept = ".csv"),
                                      checkboxInput("header", "Header", TRUE),
                                      
                                      actionButton("submitbutton1", "Run Analysis", class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h1('Result')), # Status/Output Text Box
                                      p("These results are for reference only", style = "font-family: 'times'; font-si16pt"),
                                      verbatimTextOutput('contents1'),
                                      verbatimTextOutput('contents2'),
                                      verbatimTextOutput('contents3'),
                                      tableOutput("contents4"), # Prediction results table
                                      plotlyOutput('plot', height = "300px", width = "800px")
                                    )
                           ),
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
                                      verbatimTextOutput('contents5'), # output Name 
                                      verbatimTextOutput('contents6'), # output Number of sample 
                                      verbatimTextOutput('contents7'), # output with type
                                      fluidPage(DTOutput('tbl')),# table of file input 
                                      fluidRow(
                                        box(width =500, plotlyOutput("graph")),
                                        box(height = 500, width = 500,
                                            downloadButton("downloadData", "Download"),
                                            DT::dataTableOutput("table")
                                        ))
                                    )),
                                    
                            tabPanel("Training Data", 
                                     div("Information of training data", style = "text-align: center; font-size:150%"),
                                     reactable(
                                       traindata,
                                       defaultColDef = colDef(cell = function(value, index, name) {
                                         # Render a "show plot" button in the first row - this button won't
                                         # do anything by itself, but it will trigger the custom cell click action
                                         # when clicked.
                                         if (index == 1 && name %in% names(traindata)) {
                                           htmltools::tags$button(shiny::icon("line-chart"), `aria-label` = "Show plot")
                                         } else {
                                           value
                                         }
                                       }),
                                       # Custom cell click action on the "show plot" row to send an event to Shiny.
                                       # input$plot_column will be used to access the clicked column name/ID.
                                       onClick = JS("
                                        function(rowInfo, colInfo) {
                                        if (rowInfo.index === 0) {
                                        if (window.Shiny) {
                                        Shiny.setInputValue('plot_column', colInfo.id, { priority: 'event' })
                                        }
                                     }
                                   }     
                               ")
                                     ),
                                     plotlyOutput("graph1")),
                           tabPanel("About", 
                                    titlePanel("About the study"), 
                                    div(includeMarkdown("https://github.com/EFPTTT-THYROID/Shiniapp/raw/main/about.md"), align="justify"),
                                    uiOutput("image1")
                           )
                            ))


####################################
# Server                           #
####################################

server <- function(input, output) {
  observeEvent(input$submitbutton1, {
  ##### Personal Prediction
              # output with names 
              output$contents1 <- renderText({input$txt})
              # output with age
              output$contents2 <- renderText({input$age})
              # out out with sex
              output$contents3 <- renderText({input$sex})
              # output file 
              output$contents4 <- renderTable({
                file <- input$file1
                ext <- tools::file_ext(file$datapath)
                req(file)
                validate(need(ext == "csv", "Please upload a csv file"))
                test <-as.data.frame(fread(file$datapath, header = input$header))
                #input model: traindata_rf
                githubURL <- "https://github.com/EFPTTT-THYROID/Shiniapp/blob/main/RF_model.RData?raw=true"
                load(url(githubURL))
                #load('/home/minhhoang/Documents/Thyroid cancer/TOTAL/Build_model/Result/Random_forest/RF_model.RData')
                #output 
                Output <- data.frame(Prediction=predict(traindata_rf,test), round(predict(traindata_rf,test,type="prob"), 3))
                print(Output)})
                # output graph 
              output$plot <- renderPlotly({
                  file <- input$file1
                  ext <- tools::file_ext(file$datapath)
                  req(file)
                  validate(need(ext == "csv", "Please upload a csv file"))
                  test <-as.data.frame(fread(file$datapath, header = input$header))
                  #input model: traindata_rf
                  githubURL <- "https://github.com/EFPTTT-THYROID/Shiniapp/blob/main/RF_model.RData?raw=true"
                  load(url(githubURL))
                  #load('/home/minhhoang/Documents/Thyroid cancer/TOTAL/Build_model/Result/Random_forest/RF_model.RData')
                  #output 
                  Output <- data.frame(Prediction=predict(traindata_rf,test), round(predict(traindata_rf,test,type="prob"), 3))
                  data = as.data.frame(melt(Output))
                  colnames(data) = c("Prediction", "Subtypes", "Probability")
                  plot_ly(
                  data = data,
                  x = ~ Subtypes,
                  y = ~ Probability,
                  type = 'bar',
                  color = ~ Subtypes
                )
              })
  })
    ### multi-prediction 
  observeEvent(input$submitbutton2, {
              # output with name
              output$contents5 <- renderText({input$txt2})
              # output with number of sampple 
              output$contents6 <- renderText({input$number})
              # output with type
              output$contents7 <- renderText({input$type2})
              # out put put the file in put 
              output$tbl <-  renderDT({
                file <- input$file2
                ext <- tools::file_ext(file$datapath)
                req(file)
                validate(need(ext == "csv", "Please upload a csv file"))
                test1 <-as.data.frame(fread(file$datapath, header = input$header))
                rownames(test1) <- paste('Sample', seq(1,nrow(test1),1)) 
                datatable(
                  test1 [,1:13],
                  caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;','DNA methylation of 13 CpGs') ,
                  options = list(
                    scrollX = TRUE,
                    scrollY = "250px"
                  ))        
              })
            #Output plot 
              testdata <- reactive({
                inFile <- input$file2
                if (is.null(inFile)) return(NULL)     
                else read.csv(input$file2$datapath,
                              header = TRUE,
                              sep = "\t")
              })
              dotplot1 <- reactive({
                testdata <- testdata()
                req(input$file2)
                # creat model
                urlfile<-'https://raw.githubusercontent.com/EFPTTT-THYROID/Shiniapp/main/traindata_model.tsv?token=GHSAT0AAAAAAB66ILNURVI3FNAG3LMM7MK6Y7QLJ4A'
                traindata<- read.csv(url(urlfile), sep = '\t')
                traindata_rf <- randomForest(as.factor(subtype)~., data=traindata, ntree=1000, proximity=TRUE)
                methylclass <- predict(traindata_rf, newdata=traindata)
                rf_score <- as.data.frame(predict(traindata_rf, newdata=traindata, type="prob"))
                x <- as.matrix(rf_score) 
                y <- as.factor(methylclass)  
                cvfit <- cv.glmnet(x, y, family = "multinomial", type.multinomial = "grouped")
                testPred <- as.matrix(predict(traindata_rf, newdata=testdata, type="prob"))
                predic_new <- as.factor(predict(cvfit, newx = testPred, s = "lambda.min", type = "class"))
                predic_new_score  <- predict(cvfit, newx = testPred, type='response')
                # creat data frame 
                predict <- as.data.frame(predic_new_score)
                colnames(predict) <- c("FA","FC","fvPTC", "NIFTP")
                predict$Predict <- names(predict)[1:4][apply(predict[,1:4], 1, which.max)]
                new_data <-predict %>% mutate(highest_degree = pmax(FA, FC, fvPTC,NIFTP,na.rm = TRUE))
                # draw 
                library(ggplot2)
                new_data$number <- seq(1,29,1)
                h <- rep(new_data$predict,each=4)
                h <- rep(paste0(seq(1,29,1),"_", new_data$Predict),each=4)
                c <- t(new_data[,c(1:4)])
                library(reshape2)
                mc <- melt(c)
                dotplot1 <- as.data.frame(cbind(mc,h))
                dotplot1 <- dotplot1[,-2]
                colnames(dotplot1) <- c("Score_of_Classes", "Scores", "Predict")
                samples <- paste0(seq(1,29,1),"_", predict$Predict)
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
                         title = "Cabiration Scores Plot",
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
                  caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;','Scores in details and prediction'),
                  options = list(
                    scrollX = TRUE,
                    scrollY = "250px"
                  )        
                )
              })
  })
              # download button 
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste("myfile",Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                  write.csv(dotplot1()$new_data[,1:5], file, row.names = FALSE)
                }
              )
              # output training data 
              output$graph1 <- renderPlotly({
                
                req(input$plot_column %in% names(traindata))
                traindata2<- traindata[,c(input$plot_column,"subtype")]
                mtraindata2<- melt(traindata2)
                print(mtraindata2)
                mtraindata2 %>%
                  plot_ly() %>% 
                  
                  add_trace(x = ~subtype,y = ~value, color = ~subtype, type = "box", 
                            hoverinfo = 'name+y') %>%
                  add_markers(x = ~subtype, y = ~value, color = ~subtype,
                              marker = list(size = 10),
                              hoverinfo = "text",
                              text = ~paste0("Group: ",subtype,
                                             "<br>xval: ",value),
                              showlegend = FALSE) %>% 
                  
                  layout(title = 'DNA methylation in EFPTTs group',
                         legend = list(orientation = "h",
                                       x =0, xanchor = "center",
                                       y =1, yanchor = "bottom"
                         ),
                         xaxis = list(title =  paste("Plot for", input$plot_column),
                                      showticklabels = FALSE))
                
                
              })
              # output images 
              output$image1 <- renderUI({
                tags$img(src = "https://raw.githubusercontent.com/EFPTTT-THYROID/Shiniapp/main/Screenshot%20from%202023-02-18%2009-47-59.png",
                width = "100%", align = "center")
              })
             
  }

# Create the shiny app             
shinyApp(ui = ui, server = server)
