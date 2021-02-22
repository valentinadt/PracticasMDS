#Valentina Diaz
#Ejercicio 2
#--------------------------------------------------------------------------------------------------
#Librerias que vamos a necesitar
#--------------------------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(nycflights13)
library(dplyr)
#--------------------------------------------------------------------------------------------------
#carga de datos
#--------------------------------------------------------------------------------------------------
data("flights")
attach(flights)
#--------------------------------------------------------------------------------------------------
#ui
#--------------------------------------------------------------------------------------------------

ui <- fluidPage(
        titlePanel("Ejercicio 2"),
        sidebarLayout(
          sidebarPanel(
            #primer numeric input, para el tama?o de la muestra
            
            numericInput("sampleSize",  
                         label = "Tamaño de la muestra", 
                         value = 5), # valor por defecto del tama?o muestral
            
            #segundo numeric input para introducir el tiempo de vuelo maximo
            
            numericInput("Air_max",  
                         label = "Air_timeMaximo", 
                         value = 100) # valor por defecto de Air_max
          ),
          #main panel para introducir la tabla de datos y los dos histogramas que queremos que aparezcan
         mainPanel(
            dataTableOutput("tabla_vuelos"),
            plotOutput("histograma1"), #primer histograma
            plotOutput("histograma2"), #segundo histograma
          )
        )
      )

#Server

server<-function(input,output){
  
  #se aplica un reactive a datos para que todo use la misma base de datos
  #en el interior del reactive se indican el filtro  y los datos que queremos seleccionar
  
        datos<-reactive({
          
            indicesMuestra<- sample(1:length(flight), input$sampleSize)
            flights[indicesMuestra,] %>% 
            filter(arr_delay <= input$Air_max)
            
        #este filtro indica que la cantidad de muestra que se va a elegir simpre aquellos que
        #como máximo tengan en la variable air_time maximo
        
        })
          
        output$tabla_vuelos <- renderDataTable({
          
         datos() #se introducen los datos dentro, que son los que se han elegido en el reactive anterior
        #es seguido de (), porque se ha utilizado reactive.
          
        })
          
        #Representacion del primer histograma
        
        output$histograma1 <- renderPlot({  #empeieza el histograma usando renderplot
          
          ggplot(data=datos(), #se usan los datos del reactive
            aes(x =arr_delay)) + #la variable en este caso es arr_delay
               geom_histogram(fill = 'salmon')+
                   labs(title="arr_delay histogram") #titulo del grafico
          
          })   
          
        #Representacion del segundo histograma
          
        output$histograma2 <- renderPlot({ 
          ggplot(data=datos(), #se usan los datos del reactive
            aes(x = dep_delay)) + #la variable en este caso es "dep_delay"
              geom_histogram(fill = 'lightgreen')+
              labs(title="dep_delay histogram") #titulo del grafico
            
          })
  }


shinyApp(ui = ui, server = server)
