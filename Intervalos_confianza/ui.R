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
    
    fluidRow(
        
        column(width = 11,
               
               dataTableOutput(outputId = "print_datos"),
        ),
        
        column(width = 1,
               
               verticalLayout(
                   
                   
                   # Numero de filas
                   wellPanel("Número de columnas",
                             
                             textOutput(outputId = "num_filas")
                   ),
                   
                   wellPanel("Número de columnas", 
                             
                             textOutput(outputId = "num_columnas")
                             
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
