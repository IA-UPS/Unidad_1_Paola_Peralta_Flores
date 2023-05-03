######################## METODOS DE CLASIFICACION ###########################

# Resumen de todos los métodos, incluyendo Knn y Naive Bayes. 
# Tener en cuenta que es un método de clasificación multiclase con más de 2 niveles.

## Cargar librerías

library(ggplot2)
library(ggpubr)
library(dplyr)
library(glmnet) ## regresiones logisitcas
library(caret) ### bayes y knn
library(e1071) ## bayes

## Cargar datos

# Quitar primera columna

datos <- read.table("./yeast.data",header = F)[,-1]

# Crear las funciones de transformación

min.max.mean <- function(X) apply(X,2,function(x) (x-mean(x))/(max(x)-min(x)))
min.max.median <- function(X) apply(X,2,function(x) (x-median(x))/(max(x)-min(x)))
min.max <- function(X) apply(X,2,function(x) (x-min(x))/(max(x)-min(x)))
zscore <- function(X) apply(X,2,function(x) (x-mean(x))/sd(x))
l2 <- function(X) apply(X,2,function(x) x/sqrt(sum(x^2))) 

# Para las transformaciones, necesitamos las variables numéricas.

datos <- as.data.frame(datos)
datos.numericos <- datos[, which(unlist(lapply(datos, is.numeric)))]
clase <- datos$V10 <- as.factor(datos$V10)
colnames(datos.numericos) <- paste0("Var", rep(1:8))

## Crear una lista con todas las transformaciones

datos.lista <- list(
  raw = bind_cols(datos.numericos,clase=clase),
  zscore = bind_cols(zscore(datos.numericos),
                     clase = clase),
  l2 = bind_cols(l2(datos.numericos), clase = clase),
  media = bind_cols(min.max.mean(datos.numericos), clase =
                      clase),
  mediana = bind_cols(min.max.median(datos.numericos), clase =
                        clase),
  min_max = bind_cols(min.max(datos.numericos),
                      clase = clase))

## Descriptiva Gráfica

# Al ser demasiadas variables, podemos realizar un `melt`

lista_graficos <- vector("list",length=length(datos.lista))
datos.melt <- lapply(datos.lista,reshape2::melt)

##Podemos ver la cabecera de alguna transfomacion para ver el nombre nuevo de las variables

head(datos.melt$zscore)

for(l in 1:length(datos.melt)){
  
  X <- datos.melt[[l]]
  nombre <- names(datos.melt)[l]
  lista_graficos[[l]] <- ggplot(X,aes(y=value,fill=clase))+geom_boxplot()+ggtitle(nombre)+xlab("")+ylab("")
  
  
}

names(lista_graficos) <- paste0("plot",1:length(datos.lista))

lista_graficos$plot1
lista_graficos$plot2
lista_graficos$plot3
lista_graficos$plot4
lista_graficos$plot5
lista_graficos$plot6

# Así por ejemplo la normalización min-max es la mejor, puesto que no tenemos outliers
# Otra forma de ver la transfomración es mediante gráficos de densidad

for(l in 1:length(datos.melt)){
  
  X <- datos.melt[[l]]
  nombre <- names(datos.melt)[l]
  lista_graficos[[l]] <- ggplot(X,aes(x=value))+geom_density()+ggtitle(nombre)+xlab("")+ylab("")
  
}

names(lista_graficos) <- paste0("plot",1:length(datos.lista))

lista_graficos$plot1
lista_graficos$plot2
lista_graficos$plot3
lista_graficos$plot4
lista_graficos$plot5
lista_graficos$plot6

# Sin embargo, al ver la densidad, no tenemos una transformacion uniforme.

corrplot::corrplot(cor(datos.numericos))
corrplot::corrplot(cor(datos.lista$media[,-ncol(datos)]))

## Partición de datos

# NOTA: PODEMOS CREAR LA PARTICIÓN CON `caret` o a mano, el 70 porciento de los datos. A mano sería

set.seed(123456789)
n  <- nrow(datos)
idx <- sample(1:n,n*0.7)

### para conjunto de datos podemos realizar el split

datos.train.lista <- lapply(datos.lista, function(x) x[idx,])
datos.test.lista <- lapply(datos.lista, function(x) x[-idx,])

### Ejemplo regresión logística
# https://rstudio-pubs-static.s3.amazonaws.com/38437_18a39a6487134d67b5f5e0d47221ec8d.html
# https://rpubs.com/jkylearmstrong/logit_w\_caret
# alpha=1 es lasso y 0 es ridge

set.seed(123456789)
trControl <- trainControl(method = 'cv',
                          number = 10)
myfnlog <- function(x) train(clase ~ ., data = x, method = "multinom", trControl = trControl, trace = F)

logistica.lista <- lapply(datos.train.lista,myfnlog)

logisita.pred <- vector("list",length = length(datos.lista))

for(l in 1:length(datos.lista)){
  
  logisita.pred[[l]] <- predict(logistica.lista[[l]],datos.test.lista[[l]])
  
}

names(logisita.pred) <- names(datos.lista)
accuracy <- vector("numeric",length = length(datos.lista))

for(l in 1:length(datos.lista)){
  
  accuracy[l] <- confusionMatrix(datos.test.lista$raw$clase,logisita.pred[[l]])$overall[1]
  
}

names(accuracy) <- names(datos.lista)

### Este valor lo tienen que guardar solamente haremos por accuracy y kappa
### tenemos que mirar el objeto matconf

set.seed(123456789)

### para conjunto de datos podemos realizar el split
### lasso 1 ridge 0

datos.train.lista <- lapply(datos.lista, function(x) x[idx,])
datos.test.lista <- lapply(datos.lista, function(x) x[-idx,])

cvfit_lasso <- cv.glmnet(as.matrix(datos.train.lista$raw[,-ncol(datos)]),as.numeric(datos.train.lista$raw$clase),type.measure = "class",family="multinom", alpha = 0,nfolds = 4)