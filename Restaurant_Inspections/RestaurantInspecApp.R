
# Shiny app 

library(shiny)

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

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$text <- renderPrint({
      
     make_df_pred(input$rest_id) %>% 
       predict(class_tree_model, .) %>% 
       as.character() %>% 
       str_c("Next inspection ", ., ".")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

