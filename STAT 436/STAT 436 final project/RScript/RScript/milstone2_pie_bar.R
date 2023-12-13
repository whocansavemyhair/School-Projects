library(ggplot2)
library(shiny)
library(tidyverse)
library(ggrepel)
library(plotly)

annual <- read.csv("D:/Shuohao/Courses/STAT406/GroupProject/Data/annual.csv")
head(annual)

depat <- read.csv("D:/Shuohao/Courses/STAT406/GroupProject/Data/bydept.csv")
head(depat)


pie_chart <- function(df, source){
  plot_ly(df,
          labels=~name, 
          values=~amount, 
          textposition=ifelse(df$percentage>1, "outside", "none"),
          textinfo = "label+percent",
          hoverinfo='label',
          type='pie',
          source=source,
          customdata = ~name) %>%
    layout(showlegend = FALSE) 
}

bar_chart <- function(df, title){
  df %>%
    mutate(catagory = fct_reorder(catagory, desc(total_expenditure))) %>%
  ggplot(aes(catagory, total_expenditure)) +
    geom_bar(stat = "identity") +
    labs(x="Catagory", y="Expenditure (dollor)", title = paste0("The expenditure of ",title) ) +
    theme_bw() +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


ui <- fluidPage(
  titlePanel("Expenditure by departments"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", label = "Year", 2008, 2022, c(2008, 2022)) 
    ),
    mainPanel(plotlyOutput(outputId = "pieChart"),
              plotOutput(outputId = "barchart")
              )),
  
  )


server <- function(input, output, session){
  
  dept_reactive <- reactiveValues(dept_nm="University of Wisconsin System")
  
  output$pieChart <- renderPlotly({
    annual %>%
      filter(year >= input$year[1] & year <= input$year[2] & !is.na(code)) %>%
      group_by(name) %>%
      summarise(amount = sum(amount)) %>%
      arrange(desc(amount)) %>%
      mutate(percentage=amount/sum(amount)*100) %>%
      pie_chart(source="pie")
  })
  
  
  observeEvent(event_data("plotly_click", source = "pie"), {
    click_data <- event_data("plotly_click", source = "pie")
    dept_reactive$dept_nm <- click_data$customdata[[1]]
    print(dept_reactive$dept_nm)
    })
  
  output$barchart <- renderPlot({
    depat %>%
      left_join(annual, by=c("department"="code", "year"="year")) %>%
      mutate(catagory=name.x, cata_amount=amount.x, department_name=name.y, total_amount=amount.y) %>%
      filter(year >= input$year[1] & year <= input$year[2] 
             & !is.na(code) & dept_reactive$dept_nm == department_name) %>%
      group_by(catagory) %>%
      summarise(total_expenditure = sum(cata_amount)) %>%
      bar_chart(dept_reactive$dept_nm)
  })
  
}


shinyApp(ui, server)







