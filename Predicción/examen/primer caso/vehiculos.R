###################################
#Valentina DÃ­az Torres
#Examen de PredicciÃ³n 2020-2021. MDS, CUNEF
#PREGUNTA 1
###################################

#Libraries

library(readr)
library(skimr)
library(ggplot2)
library(gplots)
library(dplyr )
library(graphics)
library(corrplot)
library(ggcorrplot)
library(PerformanceAnalytics)
library(dplyr)
library(Hmisc)
library(ggthemes)
library(car)
library(mctest)
library(tidyverse)
library(foreign)
library(varhandle)
require(fpc)
library(gridExtra)
library(fastDummies)
library(car)
library(leaps)
library(rsample)
library(glmnet)
library (boot)
library(MASS)
  
   


#Cargar datos
vehicles <-  read_csv("vehicles.csv")
View(vehicles)
colnames(vehicles)
skim(vehicles)
dim(vehicles)

#Elegir las variables con las que se va a trabajar, previamente. Primera selección de variables

df_vehicles = vehicles[,c(6,7,10:13,15,18)]
View(df_vehicles)
dim(df_vehicles) #458213 filas y 8 columnas
colnames(df_vehicles)


## Ingenieria de variables

#modificar los valores de la variable precio

df_vehicles <- df_vehicles %>% filter(df_vehicles$price !=0)
max(df_vehicles$price)
df_vehicles <- df_vehicles %>% filter(df_vehicles$price < 30000000) 
min(df_vehicles$price)
df_vehicles <- df_vehicles %>% filter(df_vehicles$price > 99)


#modificar la variable cilindros. La categoria "other" ha sido sustituida por 0 porque es desconocida.
df_vehicles$cylinders[df_vehicles$cylinders == "8 cylinders"] <- 8
df_vehicles$cylinders[df_vehicles$cylinders == "4 cylinders"] <- 4
df_vehicles$cylinders[df_vehicles$cylinders == "6 cylinders"] <- 6
df_vehicles$cylinders[df_vehicles$cylinders == "10 cylinders"] <- 10
df_vehicles$cylinders[df_vehicles$cylinders == "12 cylinders"] <- 12
df_vehicles$cylinders[df_vehicles$cylinders == "3 cylinders"] <- 3
df_vehicles$cylinders[df_vehicles$cylinders == "5 cylinders"] <- 5
df_vehicles$cylinders[df_vehicles$cylinders == "other"] <- 0
as.factor(df_vehicles$cylinders)
df_vehicles$cylinders <- replace_na(df_vehicles$cylinders, 0)

##comprobar los valores na

apply(df_vehicles, 2, function(x) {sum(is.na(x))}) 

#Las variables que tienen pocos valores nan se eliminan estos
df_vehicles <- drop_na(df_vehicles, transmission, fuel, year)

#valores categoricos sustituidos por "no data", ya que se podria perder informacion o proporcionar alguna erronea
#en caso de realizar otras sustituciones o eliminar la fila.

df_vehicles$condition <- replace_na(df_vehicles$condition, 'no data')
df_vehicles$size <- replace_na(df_vehicles$size, 'no data')

#sustituir odometer por la media

df_vehicles$odometer[is.na(df_vehicles$odometer)]<-mean(df_vehicles$odometer,na.rm=TRUE)

#comprobar que todos los valores NA han sido eliminados
apply(df_vehicles, 2, function(x) {sum(is.na(x))})
skim(df_vehicles)
colnames(df_vehicles)
#visualización de algunas de las variables resultantes
#condicion
ggplot(df_vehicles) + geom_bar(aes(y = condition))
#size
ggplot(df_vehicles) + geom_bar(aes(y = size))

#fuel
ggplot(df_vehicles, aes(y = fuel)) +
  geom_bar(aes(fill = condition), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")

cilindros_fuel <- ggplot(df_vehicles, aes(fuel, fill=cylinders)) +
  geom_bar() 
cilindros_fuel

##transformacion de las variables categoricas a dummies, mediante el metodo de One Hot Encoding

df_vehicles <- fastDummies::dummy_cols(df_vehicles)
colnames(df_vehicles)

#Nueva reduccion de variables con las que se va a trabajar
df<- df_vehicles[,-c(9,10,14,15,16,28,31,35)]
colnames(df)
# Informacion sobre ellas
colnames(df)#28 columnas en total
#han quedado las variables anteriores--> quitarlas
df<- df_vehicles[,-c(3:5,7,8)]
colnames(df)
skim(df)
#Modificación de algunas variables 
#Antes de realizar los modelos hay que ajustar algunas variables que ocasionan error para su llamada
df$`condition_like new` = names(df)[8]="condition_like_new"
df$`size_full-size` = names(df)[28]="full_size"
colnames(df)
df<- df[,-c(7,29:32)] #eliminar las que se han duplicado al renombrar

#comprobar cuales son las variables mas siginficativas para predecir el precio
set.seed(123)
multiLinealReg <- lm(price~.,data=df)
stepAIC(multiLinealReg, direction = "both") 
# RESULTADO: year, cylinders_4,cylinders_8,fuel_diesel son las más relevantes
#otro resultado con más variables: price ~ year + condition_good + cylinders_4 + cylinders_6 + cylinders_8 + 
#fuel_diesel + transmission_automatic + `size_full-size`

## VIF

model_vif <- lm(price~.,data=df,singular.ok = TRUE)
#comprobar si hay problema de multicolonilealidad
alias(lm(price~.,data=df)) # hay un problema en "fuel_other" y "transmissiion_other"
#eliminarlas
df= df[,-c(8,10,22,25)] #categorias creadas por na
colnames(df)
#se vuelve a crear el modelo vif sin estas dos variables y se comprueba
model_vif_1 <- lm(price~.,data=df,singular.ok = TRUE)
vif_values <- car::vif(model_vif_1)
#bar chart para cada valor de vif
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
knitr::kable(vif_values)
#los valores mas altos son: fuel_diesel, fuel_gas, transmission_automatic, transimission_manual

##SELECCION DE MODELO

set.seed(123)#crear una semilla

#separar en training y test

sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ] #75%
test  <- df[-sample, ] #25%
#seleccionar modelo
model_selection <- regsubsets(price~. , data =train, method = "seqrep")
#caracteristicas
model_selection_summary <- summary(model_selection)
model_selection_summary #las más significativas son: year, odometer, fuel_gas, condition_good,cylinders_4,
#cylinders_8, full_size, fuel_diesel

data.frame(
  Adj.R2 = (model_selection_summary$adjr2), #adjusted coefficient of determination
  CP = (model_selection_summary$cp),
  BIC = (model_selection_summary$bic)
)
colnames(df)
model_selection_summary$outmat #las más significativas son: fuel_gas, condition_good,cylinders_4,
#cylinders_8, full_size, fuel_diesel

plot(model_selection, scale = "bic", main = "BIC") #se representan las mismas de antes como 
#las mas significativas

data.frame(
  Adj.R2 = which.max(model_selection_summary$adjr2),
  CP = which.min(model_selection_summary$cp),
  BIC = which.min(model_selection_summary$bic)
) #adej.R2=2 | cp = 1 | bic = 1

#coeficiente de bic, adjR2 y cp
coef(model_selection,which.min(model_selection_summary$adjr2)) #resultado: year,  condition_good
coef(model_selection,which.min(model_selection_summary$cp))#cylinders_8 y fuel_diesel
coef(model_selection,which.min(model_selection_summary$bic)) #cylinders_8



#-------adjR2 Model-----------#

#variables mas sifnificativas en el coeficiente adjR2
vehiculos_r2<- lm(price~ cylinders_8+year+odometer, data =train) 
summary(vehiculos_r2) #pvalor < 0.05
fitted(vehiculos_r2) #valores predecidos en el modelo

#-------cp Model-----------#
#modelo con las variables mas significativas segun cp

vehiculos_cp<- lm(price~cylinders_8+fuel_diesel, data=train)
summary(vehiculos_cp) # las mas siginificativas son fuel_diesel y full_size
#RSE=6625000 pvalor<0.05
fitted(vehiculos_cp)

#-------BIC Model-----------#
#modelo con las variables mÃ¡s significativas segÃºn BIC

vehiculos_bic<- lm(price~cylinders_8, data=train)
summary(vehiculos_cp) #pvalor <0.05
fitted(vehiculos_bic)

#AIC para comparar modelos, el valor mÃ¡s bajo es el modelo que mejor se ajusta

AIC(vehiculos_cp, vehiculos_bic, vehiculos_r2)
#vehiculos_cp->7831526 : el mas pequeño
#vehiculos_bic->7832176
#vehiculos_r2->7830988


##------------ Prediccion----------##

# adjR2

prediction_R2 <- predict(vehiculos_r2,newdata = test)
knitr::kable(cbind(prediction_R2,test$price))
exp(cbind(prediction_R2,test$price))
mean((test$price-prediction_R2)^2) #mean: 618085699941
sqrt(mean((test$price-prediction_R2)^2)) #sqrt: 786184.3


# cp


prediction_cp <- predict(vehiculos_cp,newdata = test)
cbind(prediction_cp,test$price)
exp(cbind(prediction_cp,test$price))
mean((test$price-prediction_cp)^2) #mean:619954621065
sqrt(mean((test$price-prediction_cp)^2)) #sqrt:787372

# BIC

prediction_bic <- predict(vehiculos_bic,newdata = test)
cbind(prediction_bic,test$price)
exp(cbind(prediction_bic,test$price))
mean((test$price-prediction_bic)^2)#mean: 619252571367
sqrt(mean((test$price-prediction_bic)^2))#sqrt: 786926

##------------ Diagnostico de los modelos anteriores -------####

#test anova
anova(vehiculos_r2, vehiculos_cp, vehiculos_bic)

####-------CROSS VALIDATION--------####

apply(train, 2, function(x) {sum(is.na(x))})

colnames(df)
regres_train <- lm(price~condition_good+fuel_diesel+full_size, data = train )
regres_train_1 <- lm(price~year+fuel_diesel+condition_good+ cylinders_8+size_full-size+transmission_automatic+transmission_manual, data=train)
regres_train_2 <- lm(price~fuel_diesel+cylinders_8, data=train)
#AIC
c(AIC(regres_train),AIC(regres_train_1)) #regres_train tiene un valor de AIC mÃ¡s pequeño


#predicion
#error cuadratico medio (MSE)
predict_0 <- predict(regres_train,newdata = test)
MSE0 <- mean((test$price-predict_0)^2)
predict_1 <- predict(regres_train_1,newdata = test)
MSE1 <- mean((test$price-predict_1)^2)
predict_2 <- predict(regres_train_2,newdata = test)
MSE2 <- mean((test$price-predict_2)^2)
c(MSE0,MSE1,MSE2)#--> regres_train_2 tiene un menor error cuadrÃ¡tico medio 

colnames(df)

df<- df[,-c(7,8,10,13,14,19,23,24)]
df<- df[,-c(19,20)]

####-------RIDGE--------------------####

#separar la base de datos en training y en test otra vez, esta vez 70% y 30%
set.seed(123)
df_split <- initial_split(df, prop = .7, strata = "price")
df_train <- training(df_split)
df_test  <- testing(df_split)

#separar x e y train y test
train_x <- model.matrix(price ~., df_train)
df_train_y <- log(df_train$price)
df_test_x <- model.matrix(price ~ .,df_test)
df_test_y <- df_test$price
#dimension de la matriz
dim(train_x) #316677x3 
#Aplicar la regresión de Ridge, con lambda = 0 se comporta igual que la OLS. 
library(glmnet)
vehiculos_ridge <- glmnet(
  x = train_x,
  y = df_train_y,
  alpha = 0
)

plot(vehiculos_ridge, xvar = "lambda")

#aplicar lambda

vehiculos_ridge$lambda %>% head()#338.0980 308.0623 280.6949 255.7587 233.0378 212.3354



####-------LASSO--------------------####

vehiculos_lasso <- glmnet(
  x = train_x,
  y = df_train_y,
  alpha = 1
)

plot(vehiculos_lasso, xvar = "lambda")

# Apply CV Ridge regression to ames data
vehiculos_lasso_cv <- cv.glmnet(
  x = train_x,
  y = df_train_y,
  alpha = 1
)
# plot results
plot(vehiculos_lasso_cv)

#MSE
min(vehiculos_lasso_cv$cvm)      # 0.846
#valor de lambda para ese MSE
vehiculos_lasso_cv$lambda.min     #0.076
#primer error del minimo MSE
vehiculos_lasso_cv$cvm[vehiculos_lasso_cv$lambda == vehiculos_lasso_cv$lambda.1se]  #0.8502
#el correspondiente lambda
vehiculos_lasso_cv$lambda.1se  

#representacion
plot(vehiculos_lasso, xvar = "lambda")
abline(v = log(vehiculos_lasso_cv$lambda.min), col = "blue", lty = "dashed")
abline(v = log(vehiculos_lasso_cv$lambda.1se), col = "blue", lty = "dashed")

#variables más importantes segun Lasso
coef(vehiculos_lasso_cv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

# minimo Ridge MSE
min(vehiculos_ridge$cvm)

# minimo Lasso MSE
min(vehiculos_lasso_cv$cvm)
