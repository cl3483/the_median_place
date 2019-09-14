# ######################################################
# Initialize Packages required for the program
# ######################################################

#specify the packages of interest
packages = c("shiny","shinydashboard","shinycssloaders","DT", "quantmod" , "ggplot2" , "forecast", "randomForest", "e1071","nnet","readr", "dplyr")

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
  # Chris, drop your code in this section, if needed.
  # ######################################################
  
  prepareData <- reactive({
    
    
    # install.packages('devtools') devtools::install_github("rstats-db/bigrquery")
    # Use your project ID here
    project <- "capstone-247602" # put your project ID here
    # Example query - select copies of files with content containing "TODO"
    sql <- "SELECT * FROM [combined_digital_linear.tbl_Joined_By_Series_Name_And_AirDate]"
    # Execute the query and store the result
    show_data <- query_exec(sql, project = project, useLegacySql = FALSE)
    
    show_data$season_number <- as.factor(show_data$season_number)
    
    return(show_data)
    

  })
  
 
  # ######################################################
  # Model the data and return the prices
  # Chris, drop your code in this section.
  # ######################################################
  
  getDataExplor <- reactive({
    
    
    Stock_df <- prepareData()
    

    
    if (input$show == "THIS IS US") {
      
      
      show_data <- subset(joined_data, joined_data$series == input$show)
      
      

    } else if (input$model == "To Be Added Later") {
      
     
      show_data <- subset(joined_data, joined_data$series == input$show)
      

    }
    

    
      return(show_data)

  })


  
  
  
  
  
  # ######################################################
  # Render Plot for stock prices
  # Erik, drop your code in this section.
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
    ggplot(data=results, aes(x=episode_number, y=average_length, group=season_number)) +
      geom_line(aes(color=season_number))+
      geom_point(aes(color=season_number))+ 
      theme(legend.position="bottom")
  }) 
  
})
  
  
  
