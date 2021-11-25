options(encoding = 'UTF-8')

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
                   
                   h4("Ingrese el nivel de confianza (%): "),
                   
                   sliderInput(inputId = "nivel_de_confianza", label = "", 
                               value = 95, min = 90, max = 99),
                   
                   actionButton("Normalidad", "Realizar prueba de normalidad"),
                   
                   actionButton(inputId = "calcular_ic", label = "calcular IC", icon = icon("calculator"))
                   
            ),
            
            
            
        ),

    ),
    
    hr(),
    
    
    # Panel lateral -----------------------------------------------------------    
    
    tabsetPanel(#"", widths = c(1, 11),
                 
                 # Mostrar base de datos e información general -----------------------------
                 
                 tabPanel("",  icon = icon("table"), value = "inicio",
                          
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

                 tabPanel("", icon = icon("chart-bar"), value = "p_normalidad",
                          
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
                 
                 tabPanel("", icon = icon("calculator"), value = "ic",
                          
                          
                          column(width = 4,
                                 
                                 verticalLayout(
                                     
                                     wellPanel(
                                         
                                         plotOutput(outputId = "graf_pivote")
                                         
                                     ),
                                     
                                     wellPanel(
                                         #tableOutput("IC")
                                     ),
                                     
                                     wellPanel(
                                         #uiOutput("parametros_estimados")
                                     )
                                     
                                     
                                 )
                                 
                               
                          ),
                          
                          column(width = 4,
                                 
                                 wellPanel(
                                     
                                     plotOutput(outputId = "graf_MV")
                                     
                                 ),
                                 
                                 wellPanel(
                                     #tableOutput("IC")
                                 ),
                                 
                                 wellPanel(
                                     #uiOutput("parametros_estimados")
                                 )
                                 
                          ),
                          
                          
                          column(width = 4,
                                 
                                 wellPanel(
                                     
                                     plotOutput(outputId = "graf_boostrap")
                                     
                                 ),
                                 
                                 wellPanel(
                                     tableOutput("IC")
                                 ),
                                 
                                 wellPanel(
                                     uiOutput("parametros_estimados")
                                 )
                                 
                          )
                              
                 )
                 
                 
                 
    )
    
    
    
    
))
