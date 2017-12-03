library(shiny)
library(ggplot2)

ui<- shinyUI(
        fluidPage(
                titlePanel("Practica eventclicks"),
                
                mainPanel(
                        
                        # 1? hacemos el grafico (un output), y en funcion de lo que se toque del grafico se van a generar distintos inputs.
                        plotOutput("grafica1", click="clickGrafica", dblclick = "dbclickGrafica", hover = "hoverGrafica"), 
                        
                        # tabla en la que veo los resultados segun elija 1click, dobleclick o hover(pasar por encima el raton).
                        tabsetPanel(type = "tabs",
                                    tabPanel("Click",  tableOutput("click")),
                                    tabPanel("Doble-Click", tableOutput("dbclick")),
                                    tabPanel("Hover", tableOutput("hover"))
                        )
                        
                        
                )
        )
)

server<- shinyServer(function(input, output){
        
        # click
        output$grafica1 <- renderPlot({
                
                ggplot(mpg,aes(x=cty, y=hwy)) +   
                        geom_point()
        })
        
        output$click<- renderTable({
                
                nearPoints(mpg, input$clickGrafica, maxpoints=4)}) # Vamos a limitar a 4 (el navegador me colapsa con mas)
        
        # doble click
        
        output$dbclick<- renderTable({
                
                nearPoints(mpg, input$dbclickGrafica, maxpoints=4)}) 
        
        # HOVER
        
        
        output$hover<- renderTable({
                
                nearPoints(mpg, input$hoverGrafica, maxpoints=4)})  
        
})






shinyApp(ui,server)

