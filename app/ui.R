# ######################################################
# Initialize the UI and receive user inputs
# ######################################################

Shinyui <- fluidPage(
  
  
  # Application title
  headerPanel("The Median Place - Assignemnt"),
  
  # Sidebar with a slider inputs for number of predicted days, Variables for stock and model choices
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "show", 
                  label = "Select a Show",
                  choices = c('THIS IS US','To Be Added Later'),
                  selected = 'THIS IS US',
                  multiple = FALSE
      )
    
    ),
    
    
    
    # Show a plot of the generated stock prices
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Exploratory Data Analysis", 
                           tags$br(),
                           tags$p("The graphs below show our data exploration."),
                           shinycssloaders::withSpinner(plotOutput("graph1")),
                           shinycssloaders::withSpinner(plotOutput("graph2"))
                           
                           
                  ),
                  tabPanel("Prediction Model", 
                           tags$br(),
                           tags$p("The graphs below show our prediction attepmts."),
                           shinycssloaders::withSpinner(DT::dataTableOutput("table1"))
                  )
      ),
      tags$head(tags$script(src="https://cdn.datatables.net/rowgroup/1.1.0/js/dataTables.rowGroup.min.js"))
      ,tags$head(tags$style(src="https://cdn.datatables.net/rowgroup/1.1.0/css/rowGroup.dataTables.min.css"))
      ,tags$head(tags$style(src="https://cdn.datatables.net/1.10.18/css/jquery.dataTables.min.css"))
      ,tags$head(tags$script(src="https://cdn.datatables.net/1.10.18/js/jquery.dataTables.min.js")),
      
      textOutput(outputId = "desc")
      
      
    )
  )
)  


