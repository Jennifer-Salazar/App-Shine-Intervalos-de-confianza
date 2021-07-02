

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    h1(strong("App para Intervalos de Confianza"), align= "center",
       style="color: purple;font-family: cursive"), hr(),
    
    
    navbarPage("App shiny",
               
               wellPanel(
                   
                   uiOutput("widget") 
                   
               )

    )
    
   


))
