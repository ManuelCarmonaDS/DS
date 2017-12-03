library(shiny)
library(MASS)
library(ggplot2)

shinyServer(function(input, output) {
        
        misDatos<- reactiveValues(muestra=NULL, x=NULL, y= NULL) # En esta lista tipo reactive creamos tres variables que las dejamos en blanco. "muestra" "x" "y".
        #Para modificar el reactivevalues tenemos que utilizar un observer.
        
        observe({
                input$generala # Quiero que se ejecute cada vez que pulses el boton.
                Sigma <- isolate(matrix(c(1,input$correlacion,input$correlacion,1),2,2)) # Isolate lo uso para aislar los inputs. Creo una matriz de correlacionn con la corr introducida por el usuario.
                misDatos$muestra<-as.data.frame(mvrnorm(isolate(input$n),c(0,0),Sigma)) # Para generar la muestra multivariante. Con la matriz de correlacionn indicamos que son 2 variables.
                # El tamano de la muestra es introducida por el usuario, aislada tambien.
                
        })
        
        observe({   #Con observe no hace falta poner isolate en cada uno de los apartados.
                input$punto  # Se va a ejecutar cada vez que se haga click en el grafico
                isolate({
                        misDatos$x<-c(misDatos$x,input$punto$x)  # Guardamos la info de coordenada del eje x en el reactive.
                        misDatos$y<-c(misDatos$y, input$punto$y) # Guardamos la info de coordenada del eje y en el reactive.
                        misDatos$muestra<- rbind(misDatos$muestra, cbind(input$punto$x,input$punto$y))    # Los meto en "muestra".
                        
                })
        }   )  
        
        output$exitcorrelacion<- renderText({   # Este es el texto que cambia con el reactive.
                
                paste0( "La correlacion es :  ", cor(misDatos$muestra)[2], input$punto$x, input$punto$y)
                
                
        })
        
        output$grafica<- renderPlot({   # Hacemos la grafica que va a cambiar con el reactive.
                
                
                ggplot(misDatos$muestra) + geom_point(aes(x=misDatos$muestra$V1, y = misDatos$muestra$V2)) +
                        labs(x = "Variable 1",y = "Variable 2") 
                
        })
        
        
        
})