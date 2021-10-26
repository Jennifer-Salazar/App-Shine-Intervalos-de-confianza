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
        
        column(width = 3, offset = 1,
               
               
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
        
        column(width = 3, offset = 0,
               
               uiOutput(outputId = "preguntar_variable")
        ),
        

        # El párametro restante es conocido o desconocido -------------------------
        
        column(width = 2, offset = 0,

               uiOutput(outputId = "preguntar_parametro2"),
               
               uiOutput(outputId = "preguntar_conocido")
        ),
        
        # Botón para prueba de normalidad -----------------------------------------
        
        conditionalPanel(
            condition = "input.parametro != '' && input.nombre_variable != ''",
            
            column(width = 3, offset = 0,
                   
                   br(),
                   
                   actionButton("Normalidad", "Realizar prueba de normalidad")
                   
            ),
            
            
            
        ),

    ),
    
    hr(),
    
    
    # Panel lateral -----------------------------------------------------------    
    
    navlistPanel("App shiny", widths = c(2, 10),
                 
                 # Mostrar base de datos e información general -----------------------------
                 
                 tabPanel("Inicio",  icon = icon("table"), value = "inicio",
                          
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

                 tabPanel("Prueba de normalidad", icon = icon("chart-bar"), value = "p_normalidad",
                          
                          wellPanel(
                              
                              #actionButton("Normalidad", "Realizar prueba de normalidad"),
                              
                              fluidRow(
                                  column(width = 6,
                                         plotOutput("qqplot")
                                  ),
                                  column(width = 6,
                                         plotOutput("histograma")
                                  )
                              ),
                              
                              # plotOutput("qqplot"),

                              # plotOutput("histograma"),
                              
                              verbatimTextOutput("Shapiro")   
                          )      
                 ),
                 

                 # Calcular intervalos de confianza ----------------------------------------
                 
                 tabPanel("Intervalos de confianza", icon = icon("calculator"), value = "ic",
                          
                          
                          h4("Ingrese el nivel de confianza (%): "),
                          
                          fluidRow(
                          
                              column(width = 1,
                                     
                                     numericInput(inputId = "nivel_de_confianza", label = "", 
                                                  value = 95, min = 0, max = 100),
                                     
                                     actionButton(inputId = "calcular_ic", label = "calcular IC", icon = icon("calculator"))
                              ),
                              
                              column(width = 8, offset = 3,
                                     
                                     tableOutput("IC")
                                  
                              )
                          ),
                          
                          fluidRow(
                              
                              column(width = 8, offset = 4,
                                     
                                     uiOutput("parametros_estimados")
                                     
                              )
                          )                          
                 )
                 
                 
                 
    )
    
    
    
    
))
