library('shiny')
library('ggplot2')
library('plyr')

shinyServer(function(input, output) {
  
  dat <- read.csv("dittes_data.csv")
  dat$date <- as.Date(paste(as.character(dat$year), "-01-01", sep = ""), "%Y-%m-%d")
  dat <- dat[ !dat$habitat == 2, ] # drop habitat=32
  
  dataset <- reactive({  dat  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y="rdm")) +
      theme_grey(base_size = 18) + 
      labs(y="Residual Dry Matter (RDM)\n")
    
    plottype <- switch(input$type,
                       point=geom_point(),
                       boxplot=geom_boxplot(),
                       both=list(geom_point(), geom_boxplot()))
    p <- p + plottype
    
    scaling <- switch(input$scaling,
                      linear=scale_y_continuous(),
                      log10=scale_y_log10())
    p <- p + scaling
    
    if(input$colorby)
      p <- p + aes_string(color='transect')
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
  
    print(p)
    
  }, height=700)
  
})
