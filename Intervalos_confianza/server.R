
library(shiny)
library(DT)

# Se cargan los datos
data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")


# Server ------------------------------------------------------------------

shinyServer(function(input, output) {
    

    # Mostrar base de datos e información -------------------------------------


    output$print_datos <- renderDataTable(

        DT::datatable({data},
        options = list(lengthMenu = list(c(5,10,-1),  c("5", "10", "All")),
                       pageLength=5),
        filter = "top",
        selection = "multiple",
        style = "bootstrap")
    )
    
    output$num_filas <- renderText({
        
        paste(dim(data)[1])
    })
    
    output$num_columnas <- renderText({
        
        paste(dim(data)[2])
    })
    

    # Selección de las variables ----------------------------------------------
    
    output$widget <- renderUI({
        selectInput("etiquetas", "Seleccione la variable de interes",
                    choices=colnames(data)) 
        
    })
    

    # Prueba de normalidad ----------------------------------------------------
    
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
