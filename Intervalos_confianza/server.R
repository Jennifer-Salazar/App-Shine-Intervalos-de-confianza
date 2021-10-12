
library(shiny)
library(DT)

# Se cargan los datos
data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")


# Server ------------------------------------------------------------------

shinyServer(function(input, output) {
    

    # Preguntar la variable a usar para el IC ---------------------------------
    
    observeEvent(input$parametro, {
    
        
        if(input$parametro != ""){
                
            output$preguntar_variable <- renderUI({
                
                tagList(
                    
                    h4("Datos muestrales: "),
                    
                    selectInput(inputId = "nombre_variable", 
                                label = "",
                                choices= c("", colnames(data)) ) 
                    
                )
                
            })
        
        }
        
    })

    # Preguntar si el otro parámetro es concido o desconocido -----------------

    # observeEvent(c(input$parametro, input$nombre_variable), {
    #     
    #     # Identificar cual es el parámetro a preguntar si es conocido o no
    #     
    #     parametro_dos <- "\u03C3²"
    #     
    #     if(input$parametro == "\u03C3²"){
    #         
    #         parametro_dos <- "\u03BC"
    #         
    #     }
    #     
    #     
    #     if(input$nombre_variable != ""){
    #         
    #         output$preguntar_conocido <- renderUI({
    #             
    #             tagList(
    #                 
    #                 actionButton(inputId = "param2_conocida", paste(parametro_dos, " conocida")),
    #                 actionButton(inputId = "param2_desconocida", paste(parametro_dos, " desconocida"))
    #                 
    #             )
    #         
    #             
    #         })
    #     }
    # 
    # })
    
    
    
    # Mostrar base de datos e información -------------------------------------
    
    # Se imprime la base de datos
    output$print_datos <- renderDataTable(
        
        DT::datatable({data},
                      options = list(lengthMenu = list(c(5,10,-1),  c("5", "10", "All")),
                                     pageLength=5),
                      filter = "top",
                      selection = "multiple",
                      style = "bootstrap")
    )
    
    # Se imprime el número de filas
    output$num_row <- renderText({
        
        dim(data)[1]
    })
    
    # Se imprime el número de columnas
    output$num_column <- renderText({
        
        dim(data)[2]
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
    
    

    # Calculo de intervalos de confianza --------------------------------------
    
    observeEvent(input$parametro, {
        

        # Intervalos de confianza para la media -----------------------------------

        if(input$parametro == "\u03BC"){
            
            
            
        

        # Invetervalos de confianza para la varianza ------------------------------
            
        }else if(input$parametro == "\u03C3²"){
            
            
        }
        
        
    })
    

    
    


})
