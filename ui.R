require(shiny)
require(ggplot2)

# read the data
load('titanictrain.Rdata')
load('titanictest.Rdata')

# join data
dataset <- rbind(train, test)

shinyUI(pageWithSidebar(

  headerPanel("Titanic: machine learning from disaster"),

  sidebarPanel(

    selectInput("dataset", "Choose a dataset:",
                choices = c("train", "test", "combined")),

    selectInput('x', 'X', names(dataset), 'PassengerID'),

    selectInput('y', 'Y', names(dataset), 'Survived'),

    selectInput('color', 'Color',
                c('None', names(dataset)), 'Sex'),

    checkboxInput('smooth', 'Smooth')

  ),

  # show a tabset with summary and plot views
  mainPanel(
    tabsetPanel(
      tabPanel("Summary",
               verbatimTextOutput("summary_of_x"),
               verbatimTextOutput("summary_of_y"),
               verbatimTextOutput("summary_of_color")),
      tabPanel("Plot", plotOutput("plot"))
    )
  )

))