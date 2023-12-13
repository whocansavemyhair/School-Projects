
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)

annual <- read.csv("annual.csv")
bydeptDf <- read.csv("bydept.csv")
annual <- subset(annual, year != 2023)

ui <- fluidPage(
  titlePanel("Yearly Department Expenditures"),
  sidebarLayout(
    sidebarPanel(
      selectInput("department", "Select a Department:", choices = unique(annual$name))
    ),
    mainPanel(
      plotOutput("expPlot")
    )
  )
)

#Define the server
server <- function(input, output){
  # Generate the plot based on the selected department
  output$expPlot <- renderPlot({
    ggplot(data = annual[annual$name == input$department, ], aes(x = year, y = amount)) +
      scale_x_continuous(limits = c(2007, 2023),
                         breaks = seq(2008, 2023, by = 1)) +
      geom_col()+
      geom_line(aes(color = 'red'))+
      geom_point(aes(color = 'red', tooltip = amount))+
      scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"))+
      ggtitle(paste0(input$department, " Expenditures by Year"))+
      labs(x = "Year", y = "Expenditure Amount (in $ Billions)")+
      theme_bw()+
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)

```

