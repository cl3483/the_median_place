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


# Define server logic required to draw a graph and table



shinyServer(function(input, output, session) {
  
  # ######################################################
  # Initialize UI selectInput widgets using dataframe
  # ######################################################
  
  updateSelectInput(session = session, inputId = "show", selected ="THIS IS US")

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
  # 
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


  
  getPredict <- reactive({
    
    
    show_data <- prepareData()
    
    daystopredict <- 1:30
    
    
    if (input$show == "THIS IS US") {
      
      
      #show_data <- subset(show_data, show_data$series == input$show)
      
      daystopredict <- 1:30
      #show_data <- subset(show_data, show_data$series == "THIS IS US")
      
      show_data2 <- show_data[, c("average_length","season_number","episode_number","Imps")]
      
      # fit <- randomForest(average_length ~ . ,data=show_data2)
      
      fit <- nnet(average_length ~ . ,data=show_data2, size = 10 ,decay = .01 ,maxit = 1000, linout = TRUE, trace = FALSE )
      
      
      fit.predict <- predict(fit, newdata=data.frame(average_length = daystopredict,
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
    ggplot(results, aes(x = average_length, y = Imps)) +
      geom_point()
  })
  

  output$graph2 <- renderPlot({
    results <- getDataExplor()
    library(ggplot2)
    ggplot(data=results, aes(x=episode_number, y=average_length, group=season_number_factored)) +
      geom_line(aes(color=season_number_factored ))+
      geom_point(aes(color=season_number_factored ))+ 
      theme(legend.position="bottom")
  }) 
  
  output$graph3 <- renderPlot({
    results <-  getPredict()
    plot(results)
  }) 
  
  
})
