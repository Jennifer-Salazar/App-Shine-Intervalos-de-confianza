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
        column(width = 11,
               
               # Mostrar base de datos
               dataTableOutput(outputId = "print_datos"),
        ),
        
        # lado derecho
        column(width = 1,
               
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
    
    hr(),
    

    # Panel lateral -----------------------------------------------------------    
    
    navbarPage("App shiny",
               
               wellPanel(
                   
                   uiOutput("widget"),
                   
                   actionButton("Normalidad", "Realizar prueba de normalidad"),
                   
                   plotOutput("qqplot"),
                   
                   verbatimTextOutput("Shapiro"),
                   
                   
                   
               )

    )
    
   


))
