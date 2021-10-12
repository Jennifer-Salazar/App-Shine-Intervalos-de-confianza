# Librerias
library(shiny)


# UI ----------------------------------------------------------------------

shinyUI(fluidPage(
    
    
    # Título ------------------------------------------------------------------
    
    tags$div(class="jumbotron",
             
             h2(strong("Aplicación de"), align= "center",
                style="color: black;font-family: cursive"),
             
             h2(strong("Intervalos de Confianza"), align= "center",
                style="color: black;font-family: cursive"),
    ),


    hr(),
    
                    
    fluidRow(
        

        # Selección del parámetro para el cual se desea el IC ---------------------
        
        column(width = 2, offset = 1,
               
               
               h4("Parámetro a estimar:"),
               
               selectInput(inputId = "parametro",
                           label = "",  
                           choice = c(
                               "",
                               "\u03BC",   # media
                               "\u03C3²"   # varianza
                           )  
               ),
        ),
        
        
        # Selección de la variable de interes -------------------------------------
        
        column(width = 2, offset = 0,
               
               uiOutput(outputId = "preguntar_variable")
        ),
        

        # El párametro restante es conocido o desconocido -------------------------
        
        # column(width = 2, offset = 0,
        #        
        #        uiOutput(outputId = "preguntar_conocido")
        # ),
        
        
        # column(width = 2, offset = 0,
        # 
        #        # Si el IC es para la media
        #        
        #        conditionalPanel(condition = " input.nombre_variable != '' & input.parametro == '\u03BC' ",
        #                         
        #                         
        #                         # Escoger entre simga cuadrado conocida o desconocida
        #                         actionButton(inputId = "var_conocida", "\u03C3² conocida"),
        #                         actionButton(inputId = "var_desconocida", "\u03C3² desconocida"),
        #                         
        #                         # Selectinput para varianza conocida
        #                         conditionalPanel(
        #                             
        #                             condition = "(input.var_conocida)%2 >= 0 &
        #                                          (input.var_conocida) > (input.var_desconocida)",
        #                             
        #                             numericInput(inputId = "var_Poblacional",
        #                                          label = "",
        #                                          value = NULL,
        #                                          min = 0,
        #                                          max = 99999999999999999         
        #                             ) 
        #                         )
        #                         
        #                         
        #                         
        #        ),
        #        
        #        # Si el IC es para la varianza 
        #        
        #        
        #        conditionalPanel(condition = " input.nombre_variable != '' & input.parametro == '\u03C3²' ",
        #                         
        #                         
        #                         br(),
        #                         
        #                         # Escoger entre mu conocida o desconocida
        #                         
        #                         actionButton(inputId = "mu_conocida", "\u03BC conocida"),
        #                         actionButton(inputId = "mu_desconocida", "\u03BC desconocida"),
        #                         
        #                         # Selectinput para mu conocida
        #                         conditionalPanel(condition = "(input.mu_conocida)%2 >= 0 &
        #                                          (input.mu_conocida) > (input.mu_desconocida)",
        #                                          
        #                                          numericInput(inputId = "media_Poblacional",
        #                                                       label = "",
        #                                                       value = NULL,
        #                                                       min = -99999,
        #                                                       max = 99999         
        #                                          )                
        #                         )
        #        )
        # 
        # ),
        

        # Nivel de significancia --------------------------------------------------
        
        column(width = 2, offset = 0,
               
               conditionalPanel(condition = "input.media_Poblacional != undefined",
                   
                   h4("Nivel de significancia: "),
                   
                   numericInput(inputId = "alpha",
                                
                                label = "",
                                value = NULL,
                                min = 0,
                                max = 0.2         
                   ) 
               )
               

               
        )
    ),
    
    hr(),
    
    
    # Panel lateral -----------------------------------------------------------    
    
    navlistPanel("App shiny", widths = c(2, 10),
                 
                 # Mostrar base de datos e información general -----------------------------
                 
                 tabPanel("Inicio",  icon = icon("table"),
                          
                          # Se divide la página horizontalmente
                          fluidRow(
                              
                              # lado izquierdo
                              column(width = 9,
                                     
                                     # Mostrar base de datos
                                     dataTableOutput(outputId = "print_datos"),
                              ),
                              
                              # lado derecho
                              column(width = 3,
                                     
                                     # Se divide la página verticalmente
                                     verticalLayout(
                                         
                                         
                                         # Mostrar número de filas
                                         wellPanel("Número de filas",
                                                   
                                                   h2(textOutput(outputId = "num_row"))
                                                   
                                         ),
                                         
                                         
                                         # Mostrar número de columnas
                                         wellPanel("Número de columnas", 
                                                   
                                                   h2(textOutput(outputId = "num_column"))
                                                   
                                         )
                                         
                                     ),
                                     
                              )
                              
                          )        
                 ),
                 

                 # Verificar normalidad ----------------------------------------------------

                 tabPanel("Prueba de normalidad", icon = icon("chart-bar"),
                          
                          wellPanel(
                              
                              actionButton("Normalidad", "Realizar prueba de normalidad"),
                              
                              plotOutput("qqplot"),
                              
                              verbatimTextOutput("Shapiro")   
                          )      
                 ),
                 

                 # Calcular intervalos de confianza ----------------------------------------
                 
                 tabPanel("Intervalos de confianza", icon = icon("calculator"),
                          
                          
                          
                          
                 )
                 
                 
                 
    )
    
    
    
    
))
