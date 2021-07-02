
library(shiny)

shinyUI(fluidPage(
    
    h1(strong("Intervalos de confianza"), align= "center",
       style="color: purple;font-family: cursive"), hr(),
    
    navbarPage("App",
               tabPanel("CargarDatos",
                        
                   wellPanel(fileInput("file", "Choose CSV File", multiple = TRUE,
                             accept = c("text/csv","text/comma-separated-values,text/plain",
                                    ".csv"))),
                   
                   DT::dataTableOutput("table"),
                   checkboxInput("Datos", "Visualizar Datos", value=FALSE)
                   ),
               tabPanel("Analisis",
                        uiOutput("widget"),
                        h2("Análisis de la normalidad de la variable",
                           style="color: black;font-family: cursive"),
                        sidebarLayout(
                            sidebarPanel(
                                         
                                actionButton("grafica", "Graficamente"),
                                actionButton("analitica", "Analiticamente")
                            ),
                            mainPanel(
                                plotOutput("normal"), 
                                hr(),
                                verbatimTextOutput("test")
                            )
                        ))
               )
    
    
    
))