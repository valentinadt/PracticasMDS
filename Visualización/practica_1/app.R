
#Valentina Díaz Torres
#Tecnicas de Visualización-MDS
#Práctica 1

#--------------------------------------------------------------------------------------------------
#Librerias que vamos a necesitar
#--------------------------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(tidyr)
library(tidyverse)
#--------------------------------------------------------------------------------------------------
#carga de datos
#--------------------------------------------------------------------------------------------------
data("mpg")
?mpg
#--------------------------------------------------------------------------------------------------
#ui
#--------------------------------------------------------------------------------------------------
    
    
ui <- fluidPage(
      titlePanel("Práctica 1 Shiny"), #Título de la práctica
      sidebarLayout(
          sidebarPanel(
              numericInput("TamañoMuestra", #el nombre asignado a la muestra, al que más tarde se hará referencia
                           label = "Muestra",
                           min = 2,   #valor mínimo de la muestra
                           max = 234, #valor máximo de la muestra (número de registros del dataset)
                           value = 10), #valor por defecto
            
              selectInput("selector1",#aquí se crea el primer selector
                          label = ("Variable 1 "), 
                          choices = list("año"="year",  #las posibles elecciones dentro de la variab
                                       "Cilindros"="cyl",
                                       "Cilindrada en litros"="displ",
                                       "Consumo en ciudad"="cty",
                                       "Consumo en carretera"="hwy") ,
                          selected = "hwy"),  #selecciono consumo en carretera para este selector
            
              selectInput("selector2", #segundo selector
                          label = ("Variable 2 "),
                          #le cambio el nombre a todas las variables, para que se entiendan
                          choices = list("Año"="year",
                                       "Cilindros"="cyl",    
                                       "Cilindrada en litros"="displ",
                                       "Consumo en ciudad"="cty",
                                       "Consumo en carretera"="hwy") ,
                          selected = "year"), #selecciono año para este selector, que tiene que estar con su nombre original
            #introducir un botón, que actualice los datos cambiados de muestra, variable 1 y variable 2
              actionButton("Actualizar", 
                           label = 'Empezar') 
          ),
        #en mainpanel se introducen los gráficos que van a aparecer
          mainPanel(
              plotOutput("histograma1"), #representación del primer histograma
              verbatimTextOutput('summary1'), #verbatimTextOutput para visualizar mejor los datos
              plotOutput("histograma2"), #representación del histograma 2
              verbatimTextOutput('summary2'),
              plotOutput('grafico_correlacion'), #representación del gráfico de dispersión, que es el tercer gráfico que aparece
              verbatimTextOutput('correlacion') #muestra la correlación de Pearson
            
          )
      )
)

#--------------------------------------------------------------------------------------------------
#Server
#--------------------------------------------------------------------------------------------------
server <- function(input, output) {
    
    #creación de una submuestra, que es reactiva, lo que va a hacer que el gráfico vaya cambiando. Cada vez que use "submuestra" deberá ir seguido de
    #(), como si fuera una función, pero no lo es, para indicar que es reactiva.
       submuestra <- reactive({  
           input$Actualizar  #asignamos el imput al boton (Actualizar)
           isolate(mpg[sample(nrow(mpg), input$Tama�oMuestra),]) #  de la base de datos mpg coge de todas las filas, x numeros (input$TamañoMuestra) los que yo ponga en el grafico
        
       })  
    #Se aislan los selectores, usando isolate, para que solo se modifiquen cuando se pulse el botón "empezar"
       output$histograma1 <- renderPlot({ # histograma 1 
           var1 <- isolate(input$selector1) #en var 1 se recoge el selector 1
           ggplot(submuestra() , aes_string(x = submuestra()[[var1]])) +
               geom_histogram(fill = 'salmon')
    }) 
    
       output$summary1 <- renderPrint({ #summary histograma 1 
       muestra1 <- isolate(input$selector1)
       summary(submuestra()[[muestra1]])
        
    })
    
       output$histograma2 <- renderPlot({ # representación histograma 2
           var2 <- isolate(input$selector2) #en var2 se recoge el selector 2 
           ggplot(submuestra() , aes_string(x = submuestra()[[var2]])) +
               geom_histogram(fill = 'salmon')
        
    })   
    
       output$summary2 <- renderPrint({ #summary histograma 2 
          muestra2 <- isolate(input$selector2)
           summary(submuestra()[[muestra2]])
        
    })   
    
    
       output$grafico_correlacion <- renderPlot({ #gráfico de dispersión 
        
           var1 <- isolate(input$selector1) #en este objeto se recoge el selector 1 aislado
           var2 <- isolate(input$selector2) #en este objeto se recoge el selector 2 aislado
        
           ggplot(submuestra(), aes_string(x = submuestra()[[var1]], y = submuestra()[[var2]])) + #parámetros del gráfico con ggplot
               geom_point(colour = 'salmon', size = 5) #5 es el número parámetros que van a salir en el gráfico 
        
    })
    
       output$correlacion <- renderText({ #output de la correlación de Pearson
        
           var1 <- isolate(input$selector1)
           var2 <- isolate(input$selector2)
        
           print(paste0('Correlacion: ',  
                        round(cor(submuestra()[[var1]], submuestra()[[var2]]),2))) #el 2 significa el número de decimales que he elegido (2)
        
    })
    
}

#hay que poner esto al final del documento para que funcione como una app

shinyApp(ui = ui, server = server)
