
## Load needed packages and objects  
library(tidyverse)
library(shiny)
library(caret)
class_tree_model <- read_rds("class_tree_model.rds")
df_features <- read_rds("df_features.rds")
make_pred <- read_rds("make_pred.rds")

# Define UI for application that predicts the next inspection
ui <- fluidPage(
   
   # Application title
   titlePanel("When is your next inspection?"),
   
   # Sidebar with a numeric input 
   sidebarLayout(
      sidebarPanel(
         numericInput(inputId = "rest_id",
                      label = "Enter restaurant-ID:",
                      value = 30191841)
      ),
      
      # Show the next inspection
      mainPanel(
        verbatimTextOutput(outputId = "text")
      )
   )
)

# Define server logic required to predict next inspection
server <- function(input, output) {
   
   output$text <- renderPrint({
      
     make_pred(30075445) %>% 
       as.character() %>% 
       str_c("Next inspection ", ., ".")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

