# Librerias
library(shiny)


# UI ----------------------------------------------------------------------

shinyUI(fluidPage(
    
    
    # Título ------------------------------------------------------------------
    
    tags$div(class="jumbotron",
             
             h1(strong("App para Intervalos de Confianza"), align= "center",
                style="color: purple;font-family: cursive"),
    ),
    
    
    
    # Mostrar base de datos e información general -----------------------------
    
    hr(),
    
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
                 
                 # Panel para verificar normalidad
                 tabPanel("Prueba de normalidad",
                          
                          wellPanel(
                              
                              actionButton("Normalidad", "Realizar prueba de normalidad"),
                              
                              plotOutput("qqplot"),
                              
                              verbatimTextOutput("Shapiro")   
                          )      
                 ),
                 
                 # Panel para mostrar los intervalos de confianza
                 tabPanel("Intervalos de confianza",
                          
                          
                          actionButton(inputId = "mu", "\u03BC"),
                          actionButton(inputId = "sigma", "σ²"),
                          
                          conditionalPanel(condition = "input.mu == 1",
                                           
                                           h1("hola")
                                           
                          ),
                          
                          conditionalPanel(condition = "input.sigma == 1",
                                           
                                           h1("Como")
                                           
                          ),
                          
                          
                          
                     
                 )
                 
                 
                 
    )
    
    
    
    
))
