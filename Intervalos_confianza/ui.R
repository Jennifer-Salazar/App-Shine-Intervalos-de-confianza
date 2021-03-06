options(encoding = 'UTF-8')

# Librerias
library(shiny)
library(shinyjs)
library(rpivotTable)
library(shinycssloaders)

# UI ----------------------------------------------------------------------

shinyUI(fluidPage(
    # shinyjs
    useShinyjs(),
    
    # CSS
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # Logo de la app en la web
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo.png"),
    ),
    
    # Titulo de la app en la web
    titlePanel("", windowTitle = "Intervalos de confianza" ),
    
    # Ventana emergente de cargando... ----------------------------------------
    tags$div(
        id = "loading-content",
        tags$h1("Cargando...", class = "cargando"),
    ),
    
    # Contenido oculto por la ventana emergente
    hidden(
        
        tags$div(
            id = "app-content",
    
    

    # Panel para cargar una base de datos -------------------------------------

    conditionalPanel(condition = "input.ir_app == 0",
                     
                     tags$div(id="jumbotron",
                              
                              tags$div(
                                  
                                  h2(strong("Aplicación de"), align= "center",
                                     style="color: black;font-family: cursive"),
                                  
                                  h2(strong("Intervalos de Confianza"), align= "center",
                                     style="color: black;font-family: cursive"),
                              )
                     ),
                     
                     br(),
                     
                    wellPanel(
                        
                        fileInput(inputId = "archivo",  
                                  label = "Ingrese un conjunto de datos", 
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")
                        )
                        
                    ),
                     
                    DT::dataTableOutput("tabla"),
                    
                    tags$div(id = "habilitar_ir_app",

                        actionButton(inputId = "ir_app", label = "Continuar", icon = icon("home")),
                    
                    )
                     
    ),
    

    # Panel principal de la app -----------------------------------------------

    conditionalPanel(condition = "input.ir_app != 0",
    
    
    # Título ------------------------------------------------------------------
    
    tags$div(id="jumbotron",
             
             tags$div(
                 
                 h2(strong("Aplicación de"), align= "center",
                    style="color: black;font-family: cursive"),
                 
                 h2(strong("Intervalos de Confianza"), align= "center",
                    style="color: black;font-family: cursive"),
             )
    ),

    tags$div(id = "seleccion",
    
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
                            
                            actionButton("Normalidad", "Verificar normalidad"),
                            
                            actionButton(inputId = "calcular_ic", label = "calcular IC", icon = icon("calculator"))
                            
                     ),
                     
                     
                     
                 ),
                 
             ),
             
             hr(),
    
    ),
    
    # Panel lateral -----------------------------------------------------------    
    
    tabsetPanel(id = "Tabset", #"", widths = c(1, 11),
                 
                 # Mostrar base de datos e información general -----------------------------
                 
                 tabPanel("",  icon = icon("home"), value = "inicio",
                          
                          # Se divide la página horizontalmente
                          fluidRow(
                              
                              br(),
                              
                              # lado izquierdo
                              column(width = 9,
                                     
                                     # Mostrar base de datos
                                     DT::dataTableOutput("print_datos"),
                                     
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
                          
                          hr(),
                          
                          rpivotTableOutput(outputId = "EDA"),
                          
                          hr(),
                 ),
                 

                 # Verificar normalidad ----------------------------------------------------

                 tabPanel("", icon = icon("chart-bar"), value = "p_normalidad",
                          
                          wellPanel(
                              
                              fluidRow(
                                  column(width = 6,
                                         plotOutput("qqplot")
                                  ),
                                  column(width = 6,
                                         plotOutput("histograma")
                                  )
                              )   
                          )      
                 ),
                 

                 # Calcular intervalos de confianza ----------------------------------------
                 
                 tabPanel("", icon = icon("calculator"), value = "ic",
                          
                          
                          column(width = 4,
                                 
                                 verticalLayout(
                                     
                                     wellPanel(
                                         withSpinner(
                                             plotOutput(outputId = "graf_pivote"),
                                             type = 6, 
                                             color = "#FF000080", 
                                             size = 1
                                         )
                                     ),
                                     
                                     wellPanel(
                                         uiOutput("IC_pivote")
                                     ),
                                     
                                     wellPanel(
                                         uiOutput("pivote_latex")
                                     )
                                     
                                     
                                 )
                                 
                               
                          ),
                          
                          column(width = 4,
                                 
                                 wellPanel(
                                     withSpinner(
                                        plotOutput(outputId = "graf_MV"),
                                        type = 6, 
                                        color = "#FF000080", 
                                        size = 1
                                     )
                                     
                                 ),
                                 
                                 wellPanel(
                                     uiOutput("IC_MV")
                                 ),
                                 
                                 wellPanel(
                                     uiOutput("MV_latex")
                                 )
                                 
                          ),
                          
                          
                          column(width = 4,
                                 
                                 wellPanel(
                                     withSpinner(
                                         plotOutput(outputId = "graf_boostrap"),
                                         type = 6, 
                                         color = "#FF000080", 
                                         size = 1
                                     )
                                     
                                 ),
                                 
                                 wellPanel(
                                     uiOutput("IC_boostrap")
                                 ),
                                 
                                 wellPanel(
                                     uiOutput("boostrap_latex")
                                 )
                                 
                          )
                              
                 )
                 
                 
                 
    )
    

    )
    
    
    )), # app completa y contenido oculto 
    
    div(id = "footer",
        column( width = 1,
                br(),
                img(src="logo_escuela.jpeg", height = 80, width = 80),
        ),
        column( width = 1,
                br(),
                img(src="Nacional.png", height = 66, width = 150)
        ),
        div(id = "autores", 
            p(tags$u(strong(em("Autores:")))),
            column( width = 3,
                    br(),
                    p('Miguel Angel Londoño Ciceros'),
                    p('Carlos Mario Lopera Gomez')
            ),
            column( width = 3,
                    br(),
                    p('Jennifer Salazar Galvis'),
                    p('Mario Cesar Jaramillo Elorza'),
            )
        )
    )
    
))
