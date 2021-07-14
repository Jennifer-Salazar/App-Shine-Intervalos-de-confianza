# Librerias
library(shiny)


# UI ----------------------------------------------------------------------

shinyUI(fluidPage(
    
    
    # Título ------------------------------------------------------------------
    
    tags$div(class="jumbotron",
             
             h1(strong("App para Intervalos de Confianza"), align= "center",
                style="color: purple;font-family: cursive"),
    ),    

    # Selección de la variable de interes -------------------------------------
    

    hr(),
    
    fluidRow(

        
        column(width = 12, offset = 5,
               
               h4("Seleccione la variable de interes"),
               uiOutput("widget")
               
        )
    ),
    
    hr(),
    
    
    # Panel lateral -----------------------------------------------------------    
    
    navlistPanel("App shiny",
                 
                 # Mostrar base de datos e información general -----------------------------
                 
                 tabPanel("Inicio",
                          
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

                 tabPanel("Prueba de normalidad",
                          
                          wellPanel(
                              
                              actionButton("Normalidad", "Realizar prueba de normalidad"),
                              
                              plotOutput("qqplot"),
                              
                              verbatimTextOutput("Shapiro")   
                          )      
                 ),
                 

                 # Calcular intervalos de confianza ----------------------------------------
                 
                 tabPanel("Intervalos de confianza",
                          

                          # Selección del párametro de interes --------------------------------------
                          
                          selectInput(inputId = "parametro",
                                      label = "",  
                                      choice = c("\u03BC",
                                                 "\u03C3²")
                          ),

                          

                          # mu seleccinado ----------------------------------------------------------
                          
                          conditionalPanel(condition = "input.parametro == '\u03BC'",
                                           
                                           # Escoger entre simga cuadrado conocida o desconocida
                                           actionButton(inputId = "var_conocida", "\u03C3² conocida"),
                                           actionButton(inputId = "var_desconocida", "\u03C3² desconocida"),
                                           
                                           # Selectinput para varianza conocida
                                           conditionalPanel(condition = "input.var_conocida%2 == 1",
                                                            
                                                            numericInput(inputId = "var_Poblacional",
                                                                         
                                                                         label = "",
                                                                         value = NULL,
                                                                         min = 0,
                                                                         max = 99999999999999999         
                                                            )
                                                            
                                           )
                                           
                          ),
                          

                          # Varianza seleccionada ---------------------------------------------------

                          conditionalPanel(condition = "input.parametro == '\u03C3²'",
                                           
                                           # Escoger entre mu conocida o desconocida
                                           actionButton(inputId = "mu_conocida", "\u03BC conocida"),
                                           actionButton(inputId = "mu_desconocida", "\u03BC desconocida"),
                                           
                                           # Selectinput para mu conocida
                                           conditionalPanel(condition = "input.mu_conocida%2 == 1",
                                                            
                                                            numericInput(inputId = "var_Poblacional",
                                                                         
                                                                         label = "",
                                                                         value = NULL,
                                                                         min = -99999999999999999,
                                                                         max = 99999999999999999         
                                                            )
                                                            
                                           )
                                           
                                           
                          ),
                          
                          
                          
                          
                 )
                 
                 
                 
    )
    
    
    
    
))
