
library(shiny)


data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")

shinyServer(function(input, output) {
    
    output$widget <- renderUI({
        selectInput("etiquetas", "Seleccione la variable de interes",
                    choices=colnames(data))
        
    })


})
