#Valentina Diaz Torres
#Ejercicio 3
#--------------------------------------------------------------------------------------------------
#Librerias que vamos a necesitar
#--------------------------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(MASS)
#--------------------------------------------------------------------------------------------------
#ui
#--------------------------------------------------------------------------------------------------

ui <- fluidPage(
        titlePanel("Ejercicio 3"),
        sidebarLayout(
          sidebarPanel(
            
            #numeric input para el tama?o de la muestra
            
            numericInput("sampleSize",  
                         label = "Tamaño de la muestra", 
                         value = 10), # valor por defecto del tama?o muestral
            
            #Primer boton para actualizar el tama?o de la muestra
            
            actionButton("Generar_muestra",
                         label= "Generar muestra"),
            
            #Primer boton para actualizar el tama?o de la muestra
            
            actionButton("nuevo_punto",
                         label= "Añadir un punto nuevo")
          ),
          
          # En el main pantel se introduce la tabla de los datos y el grafico de disperion
          # El grafico se pone en primero lugar porrque queremos que aparezca encima de la 
          # tabla con los datos, por eleccion personal
          
          mainPanel(
            plotOutput("grafico_dispersion"),
            dataTableOutput("tabla")
          )
        )
)

server<-function(input,output){

          # Para que no se genere una nueva base de datos, se usa reactive
          
         datos<-reactive({ 
            
            input$nuevo_punto #Si el usuario pulsa a?adir un punto nuevo, se modificara el dataset actual
                               #añadiendo un punto adicional (una fila mas) en una posicion aleatoria usando
                               #rnorm
           
           
            input$Generar_muestra #Cuando se pulse Generar muestra se creara una muestra aleatoria de 
                                  #dos variables X e Y del tamaño elegido por el usuario usando rnorm
           
             #se aislan los datos, mediante isolate, para que los cambios solo se apliquen cuando se pulse el boton
          
            isolate(tibble(x = rnorm(n = input$sampleSize), #usar rnorm para añadir la muestra y los puntos
                           y = rnorm(n = input$sampleSize)))
            
          })
          
          #Output de la tabla con los datos
        
          output$tabla<-renderDataTable({
            datos()
          })
          
          #Output del grafico de dispersion
          
          output$grafico_dispersion <-renderPlot({ 
            ggplot(data=datos(),
                   aes(x =x,
                       y=y)) +
              geom_point(color = "darkred")
              
          })   
        
        }

shinyApp(ui = ui, server = server)