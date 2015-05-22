library(ggplot2)
library(plyr)
library(lubridate)
library(caret)
library(Metrics)
library(segmented)
library(kernlab)

##Importacion de los datos
training <- read.csv('training_set.csv', stringsAsFactors=FALSE)
test <- read.csv('test_set.csv', stringsAsFactors = FALSE)

#Se obtienen los eventos que ocurrieron en todo el set de datos
columnas_training <- colnames(training)
eventos_training <- unique(unlist(training$eventos))

columnas_test <- colnames(test)
eventos_test <- unique(unlist(test$eventos))

##Ciclos para convertir la informacion en eventos en variables "dummy"
for (j in eventos_training)
{
  evento <- training$eventos
  for (i in 1:137 ) {if (identical(evento[i], j)){evento[i] <- 1}else{evento[i] <- 0}}
  training <- cbind(training, evento)
}

for (k in eventos_test)
{
  evento <- test$eventos
  for (l in 1:46 ) {if (identical(evento[l], k)){evento[l] <- 1}else{evento[l] <- 0}}
  test <- cbind(test, evento)
}

names(training) <- c(columnas_training, eventos_training) 
names(test) <- c(columnas_test, eventos_test) 

##Se separan las combinaciones de casos a los eventos climaticos mas sencillos.
##Por ejemplo, se agregan los eventos en Niebla-Lluvia tanto a lluvia como a niebla

niebla_lluvia <- which(training$'Niebla-Lluvia' %in% 1)
lluvia_nieve <- which(training$'Lluvia-Nieve' %in% 1)
niebla_nieve <- which(training$'Niebla-Nieve' %in% 1)
niebla_lluvia_nieve <- which(training$'Niebla-Lluvia-Nieve' %in% 1)
lluvia_tormenta <- which(training$'Lluvia-Tormenta' %in% 1)

niebla_lluvia_test <- which(test$'Niebla-Lluvia' %in% 1)
lluvia_nieve_test <- which(test$'Lluvia-Nieve' %in% 1)
niebla_nieve_test <- which(test$'Niebla-Nieve' %in% 1)
lluvia_tormenta_test <- which(test$'Lluvia-Tormenta' %in% 1)

#Se agregan los eventos que tienen dos o tres variables a cada una evento sencill
training$Lluvia <- replace(training$Lluvia, c(niebla_lluvia, lluvia_nieve, niebla_lluvia_nieve, lluvia_tormenta) ,1)
training$Niebla <- replace(training$Niebla, c(niebla_lluvia, niebla_nieve, niebla_lluvia_nieve) ,1)
training$Nieve <- replace(training$Nieve, c(lluvia_nieve, niebla_nieve, niebla_lluvia_nieve) ,1)

test$Lluvia <- replace(test$Lluvia, c(niebla_lluvia_test, lluvia_nieve_test, lluvia_tormenta_test) ,1)
test$Niebla <- replace(test$Niebla, c(niebla_lluvia_test, niebla_nieve_test) ,1)
test$Nieve <- replace(test$Nieve, c(lluvia_nieve_test, niebla_nieve_test) ,1)

training <- rename(training, c("Lluvia-Tormenta"="Tormenta"))
test <- rename(test, c("Lluvia-Tormenta"="Tormenta"))


##Se eliminan la variables que contienen dos observaciones
training <- training[c(-8, -13,-14, -16, -17)]
test <- test[c(-7, -11,-14, -15)] 

#Conversion de la fecha a tipo fecha
training$fecha <- as.Date(training$fecha, "%Y-%m-%d")
test$fecha <- as.Date(test$fecha, "%Y-%m-%d")

#se convierte el cod_calendario en variable tipo factor
training$cod_calendario <- factor(training$cod_calendario)
test$cod_calendario <- factor(test$cod_calendario)

#Se convierte a la fecha en objeto tipo numero para poder usarlo en regresión
training$numfecha <- as.numeric(training$fecha)
test$numfecha <- as.numeric(test$fecha)

#Se crean variables adicionales resultantes de la multiplicacion para analizar si son relevantes en la prediccion
training$rest_rest <- (training$conteo_restaurantes)**2
training$temp_temp <- (training$temp_max)**2
training$fecha_fecha <- (training$numfecha)**2
training$rest_temp <- training$conteo_restaurantes*training$temp_max
training$rest_fecha <- training$conteo_restaurantes*training$numfecha
training$temp_fecha <- training$temp_max*training$numfecha

test$rest_temp <- test$conteo_restaurantes*test$temp_max
test$temp_temp <- (test$temp_max)**2
test$rest_rest <- (test$conteo_restaurantes)**2


##Prediccion

#Se hace un analisis para ver de todas las variables cuales son relevantes
test_model <- lm(conteo_ordenes ~ cod_calendario + conteo_restaurantes + temp_max + temp_min + precipitacion + Lluvia + Tormenta + Ninguno + Niebla + Nieve + numfecha + rest_temp + rest_rest + temp_temp + temp_fecha + fecha_fecha + rest_fecha, data = training)
anova(test_model)

test2_model <- lm(conteo_ordenes ~ cod_calendario + conteo_restaurantes + temp_max + temp_min + Ninguno + Niebla + numfecha + rest_temp + rest_rest, data = training)

#Se propone un modelo lineal unicamente con las variables relevantes despues de hacer el analisis con anova
linear_model <- lm(conteo_ordenes ~ cod_calendario + conteo_restaurantes + temp_max + Ninguno + Niebla + numfecha + rest_temp + rest_rest,training)

#se hace un analisis de la grafica para determinar los puntos de corte
ggplot(training) + geom_point(aes(numfecha, conteo_ordenes, color = Ninguno)) + theme_bw()

#Usando el modelo lineal, se crea un modelo a trozos teniendo en cuenta algunos puntos de la variable numfecha.
#Estos puntos se obtuvieron haciendo el analisis de la grafica
#IMPORTANTE: es posible que se deba correr varias veces esta linea ya que en ocasiones no encuentra los puntos de corte por lo que la funcion colapsa. Sin embargo
#siempre llega a un resultado
segment_fit <- segmented(linear_model, seg.Z = ~ numfecha, psi=list(numfecha=c(15613, 15645,15707,15736, 15752)))

#Se realiza una comprobacion del resultado del modelo
pred <- predict(segment_fit, training)
rmse(pred, training$conteo_ordenes)
error <- pred- training$conteo_ordenes
ggplot(training) + geom_point(aes(fecha, error)) 
ggplot(training) + geom_point(aes(pred, conteo_ordenes))  

#Se usa el modelo para realizar las predicciones en el set test
conteo_ordenes <- predict(segment_fit, test)
conteo_ordenes <- round(conteo_ordenes)

#Se crea el data frame con las predicciones y posteriormente se exporta al csv
submission_data <- data.frame(test$fecha, conteo_ordenes)
names(submission_data) <-c("fecha", "conteo_ordenes")

write.csv(submission_data,  "prediccion_conteo_ordenes.csv", row.names = FALSE)
