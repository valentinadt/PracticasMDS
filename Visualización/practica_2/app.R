
## Valentina D√≠az Torres
## T√©cnicas de Visualizaci√≥n - MDS, CUNEF

#Librer√≠as
library(shiny)
library(ggplot2)
#Ui
ui <- fluidPage(
    titlePanel("Ejercicio 2 de Shiny"),
    sidebarLayout(
        sidebarPanel(
            numericInput("sampleSize",  
                         label = "TamaÒoo de la muestra", 
                         min = 30, #asignar el tamaÒo de la muestra, con valores, como maximo de 30
                         value = 150) #el valor se ha definido como 150
            
        ),
        
        mainPanel(
            
            plotOutput("PlotPoint", 
                       hover = "hover"),
            verbatimTextOutput("resultHover"), #para que aparezca en una caja y sea mas visual, se usa verbatim
            dataTableOutput('nearPoints')
            
        )
    )
)

#Server
server <- function(input, output) {
    
    dataset <- reactive({ 
        data.frame(x = rnorm(input$sampleSize), #generar el dataset aleatoriamente con rnorm
                   y = rnorm(input$sampleSize)) 
    })
    
    output$PlotPoint <- renderPlot({ 
        ggplot(dataset(), aes(x = x, y = y)) + 
            geom_point(color="red") #gra°fico de puntos
    })
    
    
    output$nearPoints <- renderDataTable({
        nearPoints(dataset(),
                   coordinfo = input$hover,#posicion en la que se encuentran las coordenadas 
                   addDist = TRUE,  #aÒado la distancia al punto mas cercano
                   maxpoints = 2, #los dos puntos mas cercanos en la tabla
                   threshold = 5) #la distancia maxima es de 5 puntos
    })
    
}


shinyApp(ui = ui, server = server)
