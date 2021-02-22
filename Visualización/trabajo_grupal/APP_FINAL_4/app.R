library(shiny)
library(shinydashboard)
library(MASS)
library(ppcor)
library(magrittr)
library(readxl)
library(ggplot2)
library(dashboardthemes) #para elegir el tema de la app
library(readr)
library(tidyverse)
library(shinydashboardPlus)
library(dplyr)

# -------------------------------------------------------------------------
# CARGA DE DATOS

# Existen dos bases de datos, la primera que recoge distintos datos relacionados con el sueldo y la posicion 
# de hombres y mujeres en EEUU. La segunda, engloba la brecha salarial de en EEUU, entre mujeres y hombres.
# -------------------------------------------------------------------------

data <- read_delim("HRDataset_v14.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

gender_gap_EEUU <- read_csv("EEUU_gender_gap.csv")

# Se eligen unicamente las filas de la 25 a la 36 y las columnas "Time" y "Value" de la base de datos

gender_gap_EEUU <- gender_gap_EEUU[25:36, ] 
gender_gap_EEUU <- gender_gap_EEUU %>% select(10, 17)

# A la columna "Value" le cambiamos el nombre a gender_gap y la de "Time" a year.

colnames(gender_gap_EEUU)[2] <- "gender_gap"
colnames(gender_gap_EEUU)[1] <- "year"
gender_gap_EEUU <- gender_gap_EEUU[-c(1, 2), ]


# -------------------------------------------------------------------------
# ui
# -------------------------------------------------------------------------


ui <- dashboardPage( 
  
  # HEADERS
  dashboardHeader( title = shinyDashboardLogo(theme = "purple_gradient",
                                              boldText = "P-Hacking",
                                              mainText = "Shiny",
                                              badgeText = "v1.1")),
  
  # MENU LATERAL: En total hay 4 items en la barra lateral, los creamos con un menuItem.
  
  dashboardSidebar( 
    
    #En total hay 4 items, cada uno creado a continuacion con menuItem. Aparecen los nombres asignados
    # y se le atribuyen los iconos
    
    sidebarMenu(
      menuItem("Select your trick!", tabName = "select", icon = icon("table")),
      
      menuItem("Subsample", tabName = "subsample", icon = icon("bar-chart-o")),
      
      menuItem("Choose Variables", tabName = "choose", icon = icon("list-alt")),
      
      menuItem("Descriptive Analysis", tabName = "histograms", icon = icon("line-chart"))
      
    )
  ),
  
  # Elegir tema de la App, con la libreria "dashboardthemes", el elegido ha sido "blue gradient".
  
  dashboardBody(shinyDashboardThemes(
    
    theme = "blue_gradient"
  ),
  
  # Se crea el contenido de cada pestaña, cada una con un tabItems
  
  # Pestaña principal
  
  tabItems(
    
    # Primero se ha creado un carousel de tal forma que muestre las distintas opciones que 
    # ofrece la App. Para ello, es necesario crear una carpeta en el mismo directorio bajo 
    # el nombre "www" que tenga las imagenes que se han utilizado para dicho carousel.
    
    tabItem("select",
            
            fluidRow(tags$head(tags$style
                               
            # Asignar estilo al carousel. 
            
            (HTML("#mycarousel {
            width:600px;
            height:400px;
            margin-left: 50%;
            }
            .carousel-control{
            color:#FF0000;
            }"
          ))
          ),
          
          carousel(
            id = "mycarousel",
            
            # A continuacion, hacemos Items para el carousel. Cada Item contiene una foto. En total 
            # vamos a querer que se muestren 4 imagenes por lo que va a contener 4 items. 
            # A cada item se le asigna un formato para el tamaño.
            
            carouselItem( align = "center", width = 3,
                          img(src = 'foto4.png', height = '400px', width = '600px')
            ),
            carouselItem( align = "center", width = 3,
                          img(src = 'foto1.png', height = '400px', width = '600px')
              ),
            
            carouselItem( align = "center", width = 3,
                          img(src = 'foto2.png', height = '400px', width = '600px')
            ),
            
            carouselItem( align = "center", width = 3,
                          img(src = 'foto3.png', height = '400px', width = '600px')
            )

            )),
          
          
          # Para que los elementos aparezcan ordenados en filas es necesario especificarlo 
          # con el fluidRow.
          
            fluidRow(
              
              # valueBox estatico: Este es otro tipo de box el cual va a señalar el valor de  
              # la brecha de 2019. 
              
              valueBox("18.5","GENDER GAP (2019)", 
                       icon = icon("line-chart"), 
                       color = "light-blue", 
                       width = 2),
              
              # infoBox: El primero especifica que la información de que el 0 se utiliza para 
              # identificar a las mujeres y el segundo box que el 1 es para los hombres.
              
              # box mujeres
              infoBox(value = "Female = 0", 
                      title = "GENDER",
                      color = "fuchsia", 
                      icon = icon("venus"), 
                      width = 3),
              
              # box hombres
              infoBox(value = "Male = 1", 
                      title = "GENDER",
                      color = "blue", 
                      icon = icon("mars"), 
                      width = 3)
              
              
              ),
          
          # fluidRow otra vez para ordenar
            fluidRow(
            
            # A continuacion se crean 5 boxes, uno para cada grafico que queremos mostrar y demas informacion
            # como la correlacion o el p-value:

            box(title = "United States Gender Wage Gap", 
                status = "primary", 
                plotOutput("gender_gap"), # Este box va a contener un plot.
                width = 4),
            
            box(title = "Histogram", 
                status = "warning",
                plotOutput(("histogram_plot")), # Este box va a contener un plot.
                width = 4),
            
            box(title = "Correlation", 
                status = "danger",
                solidHeader = TRUE, 
                width = 2,
                textOutput("correlation")), # Este box va a contener un texto.
            
            box(title = "P-value", 
                status = "primary",
                solidHeader = TRUE,
                width = 2,
                textOutput("pvalue")), # Este box va a contener un texto.
            
            box(title = "Features", 
                status = "warning", 
                collapsible = TRUE, # Para que se pueda minimizar el box.
                collapsed = TRUE, # Ponemos esto para que salga minimizado directamente.
                selectInput("feature1", 
                            "Feature 1",
                            c( "EmpSatisfaction")), # Para  poder seleccionar la variable "EmpSatisfaction" en feature1  
                
                selectInput("feature2", 
                            "Feature 2",
                            c("Salary")),  # Para poder seleccionar "Salary" en Feature2
                
                selectInput("feature3", 
                            "Feature 3", 
                            c("GenderID")), # Para poder seleccionar "GenderID" en Feature3
                width = 2),
          
            )
    ),
    
    # PESTAÑA DE SUBSAMPLE
    # Se puede ir cambiando la submuestra en esta pestaña
    
    
    tabItem("subsample",
            
            fluidRow(
            
              # Creamos un valueBox, para introducir la informacion sobre que se puede observar en la pestaña
            
              valueBox(value = "SUBSAMPLE",
                      subtitle = "NOTICE HOW CORRELATION AND P-VALUE VARIES WHEN CHANGING THE SAMPLE SIZE ",
                      color = "teal", 
                      icon = icon("info-circle"),
                      width = 10),
              
              # Creamos un box que recoja el histograma original.
              
              box(title = "Original Histogram",
                  status = "primary",  
                  collapsible = TRUE,
                  plotOutput("correlation_original"), 
                  width = 4
              ),
              
              # Creamos otro box para la submuestra que recoja el plot de esta. 
              
              box(title = "Subsample",
                  status = "warning",
                  collapsible = TRUE, # Para que se pueda minimizar.
                  plotOutput("correlation_new"),
                  width = 4
              ),
              
              # Creamos otro box el cual es un numeric Input que recoge la muestra añadiendole
              # un boton para que actualice la grafica al cambiar el tamaño de la muestra. 
              
              box( status = "warning",
                   numericInput("obs",  
                                label = "Sample size",
                                min = 2,
                                max = 300,
                                value = 100),
                   
                   actionButton ("update", 
                                 label = "Random"), 
                   width = 2
              ),
              
              # Estos boxes recogen la correlacion y p-value de la submuestra seleccionada
              
              
              # Para la ccorrelación
              
              box(title = "Correlation", 
                  status = "danger",
                  solidHeader = TRUE,
                  textOutput("corr_new"),
                  width = 2),
              
              # Para el p-value
              
              box(title ="P-value", 
                  status = "primary", 
                  solidHeader = TRUE,
                  textOutput("pvalue_new"), 
                  width = 2),
              
              # Para elegir entre feature1 y feature2, con la variable Salary y EmpSatisfaction respectivamente
              
              box(title = "Features",
                  collapsible = TRUE, 
                  collapsed = TRUE,
                  selectInput("feature1", 
                              "Feature 1",
                            c( "Salary")), 
                  selectInput("feature2", 
                              "Feature 2", 
                              c("EmpSatisfaction")), 
                  width = 2)
            )
    ),
    # PESTAÑA CHOOSING VARIABLES
    
    # Este tabItem va a serveir para elegir variables y representarlas 
    
    tabItem("choose",
            
            fluidRow(
              
              # Creamos un valueBox el cual especifique lo que se puede observar en dicha pestaña
              
              valueBox(value = "CHOOSING VARIABLES",
                       subtitle = "NOTICE HOW CORRELATION AND P-VALUE VARIES WHEN CHOOSING DIFFERENT VARIABLES",
                       color = "teal", 
                       icon = icon("info-circle"),
                       width = 10),
              
              # Estos boxes recogen los graficos de correlacion original y actualizado.
              
              
              # Histogrma original
              
              box(title = "Original Histogram", 
                  status = "primary",
                  plotOutput("correlation_orig"), 
                  width = 4
              ),
              
              
              # Histograma con la variable seleccionada
              
              box(title = "Choosing variables graph", 
                  status = "warning",
                  plotOutput("correlation_choosing"), 
                  width = 4
              ),
              
              box( status = "info",
                   
                   # Hacemos un selectinput para poder seleccionar las variables que 
                   # queramos representar. Son todas aquellas que aparecen en el eje X
                   
                   selectInput("variablex", 
                               "Variable X",
                               c("MarriedID","PositionID", "DeptID", 
                                 "MaritalStatusID", "EmpSatisfaction"), 
                               selected = "MarriedID"),
                   
                   # En el eje Y siempre van a estar salary, para poder compararlo con el
                   # resto de variables y comprobar como afecta
                   
                   selectInput("variabley", 
                               "Variable Y", 
                               c("Salary"), 
                               selected = "Salary"),
                   
                    selectInput("variablez", 
                                "Variable gÃ©nero", 
                                c("Sex"), 
                                selected = "Sex"),
                   
                   # Creamos un actionButton para que actualice todos los boxes realizados

                   actionButton ("update_choosing", 
                                 label = "Update"), 
                   width = 2
              ), 
              
              # Estos boxes recogen la correlacion y pvalue de las variables seleccionadas
              
              # Para la correlacion
              
              box(title = "Correlation", 
                  status = "danger",
                  solidHeader = TRUE,
                  textOutput("corr_choosing"), 
                  width = 2),
              
              # Para el p-value
              
              box(title = "P-value", 
                  status = "primary",
                  solidHeader = TRUE,
                  textOutput("pvalue_choosing"), 
                  width = 2),
              
              box(title = "Data Set", status = "warning", collapsible = TRUE, solidHeader = TRUE , collapsed = TRUE,
                  tableOutput("Crop_data"), width = 5),
              
             # Hacemos un gradientBox que muestre la definicion de las variables, 
             # a modo de diccionario y se quede recogido en un box.
             # Para que salga con bulletpoints es necesario poner "yags$li" antes del texto.
              
              gradientBox(
                
                title = "Variables",
                icon = "fa fa-th",
                gradientColor = "teal", 
                boxToolSize = "sm",  
                tags$li("MarriedID: Takes values 1 if the person is married and 0 otherwise."),
                tags$li("PositionID: ID corresponding to the position that the person is in."),
                tags$li("DeptID: ID of the department that the person works in."),
                tags$li("MaritalStatusID: ID corresponding to the marital status (Single, Married, Divorced, Widowed or
                        Separated)."),
                tags$li("EmpSatisfaction: Satisfaction rate of each employee"),
                tags$li("Absences: The number of times each employee has been absent from work."),
                tags$li("EngagementStatus: Results from the last engagement survey, managed by an external partner."),
                width = 5, 
                collapsed = TRUE)
             
              )
              
    ),
    # PESTAÑA DESCRIPTIVE ANALYSIS 
    
    # Incluye histograma y boxplot para mujer y para hombre
    
    
    tabItem("histograms",
            
            fluidRow(
              
              # Creamos un valueBox el cual especifique lo que se puede observar en la pestaña 

              valueBox(value = "DESCRIPTIVE ANALYSIS",
                       subtitle = "TAKE A LOOK TO THE SALARY DISTRIBUTION BY GENDER",
                       color = "teal", 
                       icon = icon("info-circle"),
                       width = 10),
              
              # Para hombres
              
             infoBox(value = "Males Salary distribution", 
                     title = "MALES",
                     color = "blue", 
                     icon = icon("mars"), 
                     width = 3),
             
             # Histograma para hommbres
             
              box(title = "Males Salary distribution", 
                  status = "primary",
                  plotOutput("hist_M"), 
                  width = 4),
             
             # Boxplot para hombres
             
              box(title = "Males Salary boxplot", 
                  status = "primary",
                  plotOutput("boxplot_M"), width = 4),
             
             # Para mujeres
             
             infoBox(value = "Females Salary distribution", 
                     title = "FEMALES",
                     color = "fuchsia", 
                     icon = icon("venus"), 
                     width = 3),
             
             # Histograma para mujeres
             
             box(title = "Females Salary distribution", 
                 status = "warning",
                 plotOutput("hist_F"), 
                 width = 4),
             
             # Boxplot para mujeres
             
              box(title = "Females Salary boxplot", 
                  status = "warning",
                  plotOutput("boxplot_F"), 
                  width = 4),
             
             # Analisis descriptivo
             
             infoBox(value = "Descpriptive analysis insights", 
                     title = "ADDITIONAL INFORMATION",
                     color = "yellow", 
                     icon = icon("info"), 
                     width = 3),
             
             # Hacemos un gradientBox que muestre la descripcion del analisis 
             
              gradientBox(
                title = "Descpriptive analysis insights",
                icon = "fa fa-th", 
                width = 8,
                gradientColor = "teal", 
                
                # Para que salga con bulletpoints es necesario poner "yags$li" antes del texto
                
                boxToolSize = "sm", 
                tags$li("The mean salary is higher for males than for females: 70,629 against 67,787 dollars."),
                tags$li("The median salary is slightly higher for males than for females: 63,353  against 62,066 dollars."),
                tags$li("The most frequent Salaries are around 50,000 dollars for both genders. However, there are 125 females
                        with that salary and only 75 males."), 
                tags$li("Regarding dispersion and outliers, female salaries are less concentrated around the mean
                         and there are more outliers."),
                tags$li("Higher Salary positions belong to females, having the highest Salary of 250,000, while
                        the highest Salary for males is 178,000 dollars."),
                tags$li("To summarize, males earn slightly more money in this company but higher positions 
                        belong to women.")
                
                
              ),
             )
            )
    )
  )
)
  

#-------------------------------------------------------------------------
#server
#-------------------------------------------------------------------------


server <- function(input, output){
  
  # Bar Plot de la pestaña principal
  
  output$histogram_plot <- renderPlot({
    
  #ggplot para el grafico
    
    ggplot(data=data, aes(x= EmpSatisfaction, y=Salary, fill=Sex)) + 
      geom_bar(stat="identity", position="dodge") 
    
  })
  
  # Bar Plot de la pestaña subsambple
  
  output$correlation_original <- renderPlot({
    
    # El grafico de barras de la pestaña subsample con la variable salario y EmpSatisfaction
    
    ggplot(data, aes(x = EmpSatisfaction, y = Salary, fill = factor(Sex)))+ geom_bar(stat="identity", position="dodge")  
     
    
  })
  
  # Muestra la correlación de la pestaña principal
  
  output$correlation <- renderText({
    
    # Creamos un objeto llamado "correlacion". Este recoge la correlacionn con varias variables 
    # mediante la funcion pcor.test (la correlacion aparece en estimate).
    
    correlacion <- pcor.test( x= data$EmpSatisfaction, y = data$Salary , z = data$GenderID , method = 'pearson')
    
    print(paste0(round((correlacion$estimate),4)))
    
  })

  colnames(data)
  
  # Muestra el p-valor de la pestaña principal
  
  output$pvalue <- renderText({
    
    # Creo un objeto llamado "pvalue". Este recoge la correlacion con varias variables 
    # mediante la funcion pcor.test (el pvalue aparece en p.value).
    
    pvalue <- pcor.test(x= data$EmpSatisfaction, y = data$Salary , z = data$GenderID , method = 'pearson')
    
    print(paste0(round((pvalue$p.value),4)))
    
    
  })
  
  # Boton update de la pestaña choosing variables 
  
  distribution <- reactive({ # Se hace reactiva la submuestra para que los valores vayan cambiando
    input$update
    input$update_choosing
    
    # Isolate con la muestra. Para ello, utilizamos la funcion sample mediante la base de 
    # datos y el numero de observaciones definidas en el ui.
    
    isolate(data[sample(nrow(data), input$obs), ])  
    
  })
  
  # Bar Plot de la pestaña Subsample

  output$correlation_new <- renderPlot({
    
    colors <-  c("0" = "blue", "1" = "deeppink") #definir los colores para el genero
    
    # representacion del grafico con EmpSatisfaction y Salary
    
    ggplot(distribution(), aes(x = EmpSatisfaction, y = Salary, fill = factor(Sex)))+ 
      geom_bar(stat="identity", position="dodge") +  scale_color_manual(values = colors)
      
    
  },bg="transparent")
  
  # Correlacion de la pestaña subsample
  
  output$corr_new <- renderText({
    
    # se aislan las variables 1, 2 y 3 para que se actualicen solo pulsando el boton
    
    var1 <- isolate(input$feature1)
    var2 <- isolate(input$feature2)
    var3 <- isolate(input$feature3)
    
    cor <- pcor.test(distribution()[[var1]], distribution()[[var2]], distribution()[[var3]])
    
    print(paste0( "Correlation: ", 
                  round(cor$estimate,3))) # para mostrar el valor de la correlacion, redondeado a 3 decimales
  })
  
  # P-value de la pestaña subsample
  
  output$pvalue_new <- renderText({
    
    # se aislan las variables 1, 2 y 3 para que se actualicen solo pulsando el boton
    
    var1 <- isolate(input$feature1)
    var2 <- isolate(input$feature2)
    var3 <- isolate(input$feature3)
    
    pvalue <-  pcor.test(distribution()[[var1]], distribution()[[var2]], distribution()[[var3]])
    
    
    print(paste0(round(pvalue$p.value,2))) # para mostrar el valor del p-value, redondeado a 2 decimales
  })

 
  # Creacion del subsample
  
  output$Crop_data <- renderTable ({
    distribution() # reactive del dataset que escoja el usuario
  })
  
  # Bar Plot original de la pestaña choosing variables
 
  output$correlation_orig <- renderPlot({
    
    colors <-  c("0" = "blue", "1" = "deeppink") # definicion del color por genero
    
    # grafico de barras con EmpSatisfaction y salario 
    
    ggplot(data, aes(x = EmpSatisfaction, y = Salary, fill = factor(Sex)))+ 
      geom_bar(stat="identity", position="dodge")  +  scale_color_manual(values = colors)
      
  })
  
  # Bar plot de la pestaña choosing variables
  
  output$correlation_choosing <- renderPlot({
    # se aislan las variables x, y, z para que se actualicen solo pulsando el boton
    
    variablex <- isolate(input$variablex)
    variabley <- isolate(input$variabley)
    variablez <- isolate(input$variablez)
    
    colors <-  c("0" = "blue", "1" = "deeppink") # definicion del color por genero
    
    # Grafico de barras con variable x, y , z 
    
    ggplot(distribution(), aes_string( x = distribution()[[variablex]], y = distribution()[[variabley]], 
                                       fill = factor(distribution()[[variablez]]))) + 
      geom_bar(stat="identity", position="dodge") 
    
  })
  
  # Correlacion de la pestaña choosing variables
  
  output$corr_choosing <- renderText({
    
    # Aislar las variables x e y para que cambien solo con el boton
    
    var11 <- isolate(input$variablex)
    var22 <- isolate(input$variabley)

    cor_choosing <- cor.test(distribution()[[var11]], distribution()[[var22]])
    
    # para mostrar la correlacion, redondeada a 3 decimales
    
    print(paste0( "Correlation: ", 
                  round(cor_choosing$estimate,3)))
  })
  
  # P-value de la pestaña choosing variables
  
  output$pvalue_choosing <- renderText({
    
    # Aislar las variables 11 y  22 para que cambien solo con el boton
    
    var11 <- isolate(input$variablex)
    var22 <- isolate(input$variabley)
    
    pvalue_choosing <-  cor.test(distribution()[[var11]], distribution()[[var22]])
    
    # Mostrar el p-value, redondeado a 3 decimales
    
    print(paste0("P-value: ", round(pvalue_choosing$p.value,3)))
    
  })
  
  # Analisis descriptivo
  
  # Histograma de hombres
 
  output$hist_M <- renderPlot({
    
    hist_males <- data %>%
      filter(Sex == "M") %>%
      ggplot(aes(x = Salary)) + 
      geom_histogram(binwidth= 20000 , fill = "turquoise3", color = "grey") + # color turquesa para hombres
      ylim(0,120) + xlim(0, 250000)
    
    
    hist_males

  })
  
  # Histograma para mujeres
  
  output$hist_F <- renderPlot({
    
    hist_females <- data %>%
      filter(Sex == "F") %>%
      ggplot(aes(x = Salary)) +
      geom_histogram(binwidth= 20000, fill = "lightcoral", color = "grey") + # color coral para muejeres
      ylim(0,120) + xlim(0, 250000) 
    
    
    hist_females
    
  })
  
 # Grafica de gender gap
  
  output$gender_gap <- renderPlot({
    
    # grafico de lineas, años va en la x y gender gap en la y 
    
    ggplot(gender_gap_EEUU, aes(x = year, y = gender_gap)) + geom_line(color = "turquoise3") + 
      scale_x_continuous(breaks = gender_gap_EEUU$year) + labs(x = "Year", y = "Gender Wage Gap")
    
  })
  
  # Box Plot para gender gap para hombres
  
  output$boxplot_M <- renderPlot({
    data %>%
      filter(Sex == "M") %>%
      ggplot(aes(y = Salary)) + geom_boxplot(fill = "turquoise3") + # color turquesa para hombres
      ylim(40000, 255000)
  })
  
  # Box Plot para gender gap para mujeres
  
  output$boxplot_F <- renderPlot({
    data %>%
      filter(Sex == "F") %>%
      ggplot(aes(y = Salary)) + geom_boxplot(fill = "lightcoral")+  # color coral para mujeres
      ylim(40000, 255000)
    
  })
  
 
}

# Para que se muestre como aplicacion Shiny
shinyApp(ui, server)