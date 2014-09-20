library('shiny')
library('ggplot2')

vars <- c('date','transect','landform','habitat','slope_class')
varsnamed <- c('date','transect','landform','habitat','slope_class')
names(varsnamed) <- vars

shinyUI(pageWithSidebar(
  
  headerPanel("Dry Creek Data"),
  
  sidebarPanel(
    selectInput('x', 'X Variable', vars),
    
    selectInput('scaling', 'RDM axis', c('linear','log10'), 'linear'),
    
    selectInput('type', 'Plot type', c('point','boxplot','both'), 'point'),
    
    checkboxInput('jitter', 'Jitter'),
    
    selectInput('facet_row', 'Facet Row', c(None='.', varsnamed)),
    selectInput('facet_col', 'Facet Column', c(None='.', varsnamed)),
    
    checkboxInput('colorby', 'Color by transect?')
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))
