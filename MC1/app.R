library(shiny)


ui <- fluidPage( 
    mc1UI("mc1")
) 

server <- function(input, output, session) {
    mc1Server("mc1") 
    }

shinyApp(ui, server)