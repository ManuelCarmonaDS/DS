library(shiny)

shinyUI(fluidPage(
        
        titlePanel("Generacion de muestras aleatorias"),
        sidebarLayout(
                
                
                sidebarPanel(
                        
                        
                        numericInput("n", label = h3("Muestra"), min= 1, max= 10000, value = 100, step= 10),                 # TamaÃ±o de la muestra
                        numericInput("correlacion", label = h3("Correlacion Pearson"), min= -1, max=1, value = 1, step=.1),  # Correlacion
                        actionButton("generala", label = "Generar muestra aleatoria")                                        # Boton generar muestra
                        
                        
                        
                ),
                mainPanel(
                        fluidRow(
                                plotOutput("grafica", click="punto"),                                                            # Output de la grafica e input de los clicks
                                verbatimTextOutput("exitcorrelacion")                                                                   # Output del texto con la correlacion
                        )
                )
                
        )
        
        
))