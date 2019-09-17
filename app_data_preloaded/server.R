# ######################################################
# Initialize Packages required for the program
# ######################################################

#specify the packages of interest
packages = c("shiny","shinydashboard","shinycssloaders","DT", 
             "quantmod" , "ggplot2" , "forecast", "randomForest", 
             "e1071","nnet","readr", "dplyr","bigrquery")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()


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
    
    
    show_data <- readRDS("show_data.rds")
    
    
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
    results <- as.data.frame(getPredict())
    names(results)[1] <- "AVs"
    results$AVs <- paste(format(round(results$AVs / 1e6, 1), trim = TRUE), "M")
    results$Day <- paste0('D',as.integer(rownames(results)))
    results$Day <- factor(results$Day, levels = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10",
                                                  "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20",
                                                  "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30"))
    ggplot(results, aes(x=results$Day, y=results$AVs)) +geom_point(color='black', size=3) +xlab("Predicted Day") + ylab("Predicted Average Viewing")
    
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
