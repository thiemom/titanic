require(shiny)
require(ggplot2)

# read the data
load('titanictrain.Rdata')
load('titanictest.Rdata')

# join data
combi <- rbind(train, test)

# server logic required to run ui.R
shinyServer(function(input, output) {

  dataset <- reactive({
    switch(input$dataset,
           "train" = train,
           "test" = test,
           "combined" = combi)
  })

  output$summary_of_x <- renderPrint({
    v <- input$x
    data <- dataset()
    summary(data[v])
  })

  output$summary_of_y <- renderPrint({
    v <- input$y
    data <- dataset()
    summary(data[v])
  })

  output$summary_of_color <- renderPrint({
    if (input$color != 'None') {
      v <- input$color
      data <- dataset()
      summary(data[v])
    }
  })

  output$plot <- renderPlot({
    data <- dataset()
    p <- ggplot(data,
                aes_string(x=input$x,
                           y=input$y)) + geom_point()

    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)
    }

    if (input$smooth) {
      p <- p + geom_smooth()
    }

    print(p)

  })


})