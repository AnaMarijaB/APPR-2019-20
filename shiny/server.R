library(dygraphs)
library(shiny)

function(input, output, session) {
  
  output$dostopnost <- renderDygraph({
    graf.dostopnost(input$spol, input$starost, input$dobrina)
  })

}
