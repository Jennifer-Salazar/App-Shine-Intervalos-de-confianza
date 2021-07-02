
library(shiny)


shinyServer(function(input, output) {
    
    data <- reactive({
        infile <- input$file
        if(is.null(infile)){
            return()
        }
        datafile <- read.table(infile$datapath, header = TRUE, sep=",",encoding = "UTF-8")
        return(datafile)
        
    })
    
    output$table <- DT::renderDataTable({
        
        DT::datatable({
            datos <- data()
            if(input$Datos){
                datos
                
            }
        },
        options = list(lengthMenu = list(c(5,10,-1),  c("5", "10", "All")),
                       pageLength=10),
        filter = "top",
        selection = "multiple",
        style = "bootstrap")
        
    })
    
    output$widget <- renderUI({
        selectInput("etiquetas", "Seleccione la variable",
                    choices=names(data()))
        
    })
    
    
    
})