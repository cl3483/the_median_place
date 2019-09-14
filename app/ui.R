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
                           tags$p("The graphs below show our prediction Neural Network.  Neural Network (or Artificial Neural Network) has the ability to learn by examples. ANN is an information processing model inspired by the biological neuron system. It is composed of a large number of highly interconnected processing elements known as the neuron to solve problems. It follows the non-linear path and process information in parallel throughout the nodes. A neural network is a complex adaptive system. Adaptive means it has the ability to change its internal structure by adjusting weights of inputs."),
                           shinycssloaders::withSpinner(plotOutput("graph3"))
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


