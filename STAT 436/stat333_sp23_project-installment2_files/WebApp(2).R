library(shiny)
library(dplyr)

# Load your data and final model
my_data <- read.csv("installment2_evaluation_data.csv")
my_model <- readRDS("model.rds")


ui <- fluidPage(
  titlePanel("Predict PRSM"),
  
  sidebarLayout(
    sidebarPanel(
      # Add input controls for each predictor variable
      sliderInput("FICO", "FICO:", min = 300, max = 850, value = 697),
      sliderInput("TotalAmtOwed", "TotalAmtOwed:", min = 0, max = 1000000, value = 232420),
      selectInput("WomanOwned", "WomanOwned:", choice=c("0", "1")),
      selectInput("CorpStructure", "CorpStructure:", choice=c("Corp", "Sole", "LLC", "Partner")),
      selectInput("NAICS444240", "NAICS444240:", choice=c("FALSE", "TRUE"))
      
    ),
    
    mainPanel(
      # Display the predicted PRSM and any other relevant information
      verbatimTextOutput("prediction_text")
    )
  )
)


server <- function(input, output) {
  # Define a reactive function to generate the predicted PRSM
  predicted_prsm <- reactive({
    # Create a data frame with the input values
    newdata <- data.frame(
      FICO = input$FICO,
      TotalAmtOwed = input$TotalAmtOwed,
      WomanOwned = input$WomanOwned,
      CorpStructure = input$CorpStructure,
      NAICS444240 = input$NAICS444240
    )
    # Use the model to predict PRSM for the input values
    predictions <- predict(my_model, newdata)
    # Return the predicted PRSM
    return(predictions)
  })
  
  # Display the predicted PRSM in the user interface
  output$prediction_text <- renderText({
    paste0("Predicted PRSM: ", round(predicted_prsm(), 4))
  })
}


shinyApp(ui = ui, server = server)