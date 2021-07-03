
library(shiny)


data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")

shinyServer(function(input, output) {
    
    output$widget <- renderUI({
        selectInput("etiquetas", "Seleccione la variable de interes",
                    choices=colnames(data))
        
        
        
    })
    
    observeEvent(input$Normalidad, {
        
        variable <- data[,input$etiquetas]
        
        output$qqplot <- renderPlot({
            qqnorm(variable, pch=19)
            qqline(variable)
            grid()
            
        })
        
        output$Shapiro <- renderPrint({
            shapiro.test(variable)

        })
    })
    


})
