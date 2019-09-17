# ######################################################
# Initialize Packages required for the program
# ######################################################

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(quantmod)
library(ggplot2) 
library(forecast)
library(randomForest)
library(e1071)
library(nnet)
library(readr)
library(dplyr)
library(bigrquery)




## function to plot a regression graph

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


# Define server logic required to draw a graph and table



shinyServer(function(input, output, session) {
  
  # ######################################################
  # Initialize UI selectInput widgets using dataframe
  # ######################################################
  
  updateSelectInput(session = session, inputId = "show", selected ="THIS IS US")
  updateSelectInput(session = session, inputId = "model", selected ="SVM")
  
  # ######################################################
  # Prepare the data for the modeling process
  # 
  # ######################################################
  
  prepareData <- reactive({
    
    
    # install.packages('devtools') devtools::install_github("rstats-db/bigrquery")
    # Use your project ID here
    project <- "capstone-247602" # put your project ID here
    # Example query - select copies of files with content containing "TODO"
    sql <- "SELECT * FROM [combined_digital_linear.tbl_Joined_By_Series_Name_And_AirDate]"
    # Execute the query and store the result
    show_data <- query_exec(sql, project = project, useLegacySql = FALSE)
    
    show_data$season_number_factored <- as.factor(show_data$season_number)
  
    
    return(show_data)
    

  })
  
 
  # ######################################################
  # Get data by Show
  # 
  # ######################################################
  
  getDataExplor <- reactive({
    
    
    show_data <- prepareData()
    

    
    if (input$show == "THIS IS US") {
      
      
      show_data <- subset(show_data, show_data$series == input$show)
      
      

    } else if (input$model == "To Be Added Later") {
      
     
      show_data <- subset(show_data, show_data$series == input$show)
      

    }
    

    
      return(show_data)

  })

  # ######################################################
  # Get text for descritpions
  # 
  # ######################################################
  
  prepareText <- reactive({
    
    model_desc <- read_csv("model_desc.csv")
    
    return(model_desc)
    
  })
  
  # ######################################################
  # Do predictions
  # 
  # ######################################################
  
  getPredict <- reactive({
    
    
    show_data <- getDataExplor()
    
    daystopredict <- 1:30
    
    
    if (input$show == "THIS IS US") {
      set.seed(1234)
      
      #show_data <- subset(show_data, show_data$series == input$show)
      
      daystopredict <- 1:30
      #show_data <- subset(show_data, show_data$series == "THIS IS US")

      ## Subset Data for modeling
      show_data2 <- show_data[, c("average_mins_viewed","season_number","episode_number","Imps")]
      
      ## Remove Outliers
      outliers<-boxplot.stats(show_data2$Imps)$out
      show_data2 <- show_data2[-which(show_data2$Imps %in% outliers),]

      getModel <- reactive({
        
      if (input$model == "SVM") {
        fit <- svm(average_mins_viewed ~ . ,data=show_data2,cost=5,gamma=0.01, epsilon=0.1)
        
       } else if (input$model == "Random Forest") {  
       fit <- randomForest(average_mins_viewed ~ . ,data=show_data2)
       
       } else if (input$model == "Neural Network") {  
      
       fit <- nnet(average_mins_viewed ~ . ,data=show_data2, size = 10 ,decay = .01 ,maxit = 1000, linout = TRUE, trace = FALSE )
      
       }
      
      return(fit)
      
      })
      
      fit <-  getModel()
      
      fit.predict <- predict(fit, newdata=data.frame(average_mins_viewed = daystopredict,
                                                     season_number = daystopredict,
                                                     episode_number = daystopredict,
                                                     Imps = daystopredict))
      
      
      
    } else if (input$show == "To Be Added Later") {
      
      
      
      
      
      
    }
    
    
    return(fit.predict)
    
  })
  
  
  
  # ######################################################
  # Render Plot for stock prices
  # ######################################################     
  
  output$graph1 <- renderPlot({
        results <- getDataExplor()
    library(ggplot2)
    fit <- lm(average_mins_viewed ~ Imps, data = results)
    ggplotRegression(fit)
  })
  

  output$graph2 <- renderPlot({
    results <- getDataExplor()
    library(ggplot2)
    ggplot(data=results, aes(x=episode_number, y=average_mins_viewed, group=season_number_factored)) +
      geom_line(aes(color=season_number_factored ))+
      geom_point(aes(color=season_number_factored ))+ 
      theme(legend.position="top")
  }) 
  
  output$graph3 <- renderPlot({
    results <-  getPredict()
    plot(results)
  })
  
  output$graph4 <- renderPlot({
    results <- getDataExplor()
    boxplot(results$Imps)
  })
  

  output$desc <- renderText({
    model_desc <- prepareText()
    filter(model_desc, model_desc$model == input$model) %>% pull(desc)
    
  })
  
  
})