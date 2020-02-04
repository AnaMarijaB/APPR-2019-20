library(shiny)

shinyUI(fluidPage(
  
  titlePanel('Dostopnost dobrin po starosti in spolu'),
  
  sidebarPanel(
    selectInput('spol','SPOL',
                c('moski','zenski')),
    selectInput('starost','STAROST',
                levels(as.factor(DOSTOPNOST$STAROST))),
    selectInput('dobrina','DOBRINA',
                levels(as.factor(DOSTOPNOST$DOBRINA)))
  ),
  
  mainPanel(
    dygraphOutput('dostopnost')
  )
  
))
