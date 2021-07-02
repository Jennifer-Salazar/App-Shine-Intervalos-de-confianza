
library(shiny)
data <- read.table("www/Ejercicio6.txt", sep=",", header = TRUE)

shinyServer(function(input, output) {
    
    output$widget <- renderUI({
        selectInput("etiquetas", "Seleccione la variable de interes",
                    choices=colnames(data))
        
    })


})
