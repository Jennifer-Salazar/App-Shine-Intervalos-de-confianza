
library(shiny)
library(DT)

# Se cargan los datos
data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")


# Server ------------------------------------------------------------------

shinyServer(function(input, output) {
    

    # Mostrar cuestionario para la variable a usar en el IC -------------------
    
    observeEvent(input$parametro, {
    
        
        if(input$parametro != ""){
                
            output$preguntar_variable <- renderUI({
                
                tagList(
                    
                    h4("Variable de interés: "),
                    
                    selectInput(inputId = "nombre_variable", 
                                label = "",
                                choices= c("", colnames(data)) ) 
                    
                )
                
            })
            
            
            # Preguntar si el otro parámetro es concido o desconocido -----------------
            
            # Identificar cual es el parámetro a preguntar si es conocido o no
            
            if(input$parametro == "\u03C3²"){
                
                parametro_dos <- "\u03BC"
                
            }else{
                parametro_dos <- "\u03C3²"
            }
            
            # vector para mostrarle al usuario las opciones
            
            opciones <- paste(parametro_dos, c("conocido", "desconocido"))
            
            
            observeEvent(input$nombre_variable, {
                
                # Mostrar las opciones de conocido y desconocido
                
                if(input$nombre_variable != ""){
                    
                    output$preguntar_parametro2 <- renderUI({
                        
                        tagList(
                            h4("Seleccionar: "),
                            
                            radioButtons(inputId = "parametro2_conocido",
                                         label = "", choices = opciones)
                        )
                        
                        
                    })
                    
                }
                
            })
            
            # Desplegar numeric input si el parámetro2 es conocido --------------------
            
            observeEvent(input$parametro2_conocido, {
                
                if(input$parametro2_conocido == opciones[1]){
                    
                    output$preguntar_conocido <- renderUI({
                        
                        numericInput(
                            inputId = "conocido",
                            label = "",
                            value = NULL,
                            min = 0,
                            max = 999999
                        )
                        
                    })
                    
                }else{
                    
                    output$preguntar_conocido <- renderUI({
                        
                    })
                }
                
                
            })
        }
        
    })

    
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
        
        variable <- data[,input$nombre_variable]
        
        output$qqplot <- renderPlot({
            qqnorm(variable, pch=19)
            qqline(variable)
            grid()
            
        })
        
        output$histograma <- renderPlot({
            hist(variable, col = "cyan4")
            grid()

        })
        
        
        output$Shapiro <- renderPrint({
            shapiro.test(variable)

        })
    })
    
    

    # Calculo de intervalos de confianza --------------------------------------
    
    observeEvent(input$calcular_ic, {
        
        source("IC_functions3.R")
        
        variable <- data[,input$nombre_variable]

        # Intervalos de confianza para la media -----------------------------------
        
        if(input$parametro == "\u03BC"){
            
            x_barra <- mean(variable)
            
            n <- length(variable)
            
            alpha <- 1 - input$nivel_de_confianza * 0.01
            
            # Normalidad
            p_val_normalidad <- shapiro.test(variable)$p.value
            
            normalidad <- ifelse(p_val_normalidad > 0.1, T, F)
            
            
            # Varianza o no conocida
            
            if( strsplit( input$parametro2_conocido, " ")[[1]][2] == "conocido"){
                conocida <- TRUE
                desv <- (input$conocido)^0.5
            }else{
                conocida <- FALSE
                desv <- sd(variable)
            }
            
            # Calculo de los intervalos de confianza
            
            ic_pivote <- ic_pivote_media(x_barra, desv, n, alpha, conocida, normalidad)
            
            ic_mv <- ic_mv_media(x_barra, desv, n, alpha)
            
            ic_boostrap <- ic_boostrap_media(variable, alpha)
            
            
            # Mostrar los intervalos como un dataframe
            
            metodos <- c("Método del pivote", "Máxima verosimilitud", "Boostrap BCA")
            
            intervalos <- cbind(ic_pivote, ic_mv, ic_boostrap)
            colnames(intervalos) <- metodos
            row.names(intervalos) <- c("límite inferior", "límite superior")
            
            output$IC <- renderTable({
                
                intervalos
                
            }, include.rownames=TRUE)
            
            
            output$parametros_estimados <- renderUI({
                
                tagList(
                    
                    h5(paste("Estimación puntual media: ", x_barra)),
                    
                    h5(paste("Estimación puntual varianza: ", round(desv^2, 3)))
                
                )
            })
            
            
        # Invetervalos de confianza para la varianza ------------------------------
            
        }else if(input$parametro == "\u03C3²"){
            
            
        }
        
    })
        
        

    

    
    


})
