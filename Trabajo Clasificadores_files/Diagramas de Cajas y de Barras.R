########################## DIAGRAMAS DE CAJAS ##################################

### PREPARAR LOS DATOS
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

### DIAGRAMAS DE CAJA BASICO
library(ggplot2)
# Diagrama de caja básico
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot()
p
# Rotar el diagrama de caja
p + coord_flip()
# diagrama de caja con muescas
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(notch=TRUE)
# Cambiar valores atípicos, color, forma y tamaño
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
# Diagrama de caja con puntos medios
p + stat_summary(fun=mean, geom="point", shape=23, size=4)
#Elegir que elemnetos mostrar
p + scale_x_discrete(limits=c("0.5", "2"))

##DIAGRAMA DE CAJAS CON PUNTOS##
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
#Diagrama de caja con puntos jittered
# 0.2: grado de fluctuación en la dirección x
p + geom_jitter(shape=16, position=position_jitter(0.2))

## CAMBIAR COLORES
# Cambiar los colores de las lineas por grupos
p<-ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_boxplot()
p
# Usar paletas de colores personalizadas
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Usa paletas de colores cerveceros
p+scale_color_brewer(palette="Dark2")
# Usa escala de grises
p + scale_color_grey() + theme_classic()
## Cambiar los colores de relleno del diagrama de caja
# Usar un solo color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
# Cambiar los colores del diagrama de caja por grupos
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_boxplot()
p
# Usar paletas de colores personalizadas
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Usa paletas de colores cerveceros
p+scale_fill_brewer(palette="Dark2")
# Usa escala de grises
p + scale_fill_grey() + theme_classic()

## CAMBIAR LA POSCICION DE LA LEYENDA
p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Eliminar leyenda

## CAMBIAR EL ORDEN DE LOS ELEMENTOS EN LA LEYENDA
p + scale_x_discrete(limits=c("2", "0.5", "1"))

### DIAGRAMA DE CAJA CON MULTIPLES GRUPOS
# Cambiar los colores del diagrama de caja por grupos
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot()
# Cambiar la posicion
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot(position=position_dodge(1))
p
# Agregar puntos
p + geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(1))
# Cambiar colores
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

### DIAGRAMAS DE CAJA PERSONALIZADOS
# Diagrama de caja básico
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(fill="gray")+
  labs(title="Plot of length per dose",x="Dose (mg)", y = "Length")+
  theme_classic()
# Cambiar automáticamente el color por grupos
bp <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + 
  geom_boxplot()+
  labs(title="Plot of length  per dose",x="Dose (mg)", y = "Length")
bp + theme_classic()
# Colores continuos
bp + scale_fill_brewer(palette="Blues") + theme_classic()
# Colores discretos
bp + scale_fill_brewer(palette="Dark2") + theme_minimal()
# Colores degradados
bp + scale_fill_brewer(palette="RdBu") + theme_minimal()

########################## GRAFICOS DE BARRAS ##################################

### DATOS
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
head(df)

### CREAR DIGRAMAS EN BARRAS
library(ggplot2)
# Diagrama de barras básico
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")
p
# Gráfico de barras horizontales
p + coord_flip()
# Cambiar el ancho de las barras
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)
# Cambiar colores
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", color="blue", fill="white")
# Tema minimal + color de relleno azul
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p
# Elegir que elementos mostrar
p + scale_x_discrete(limits=c("D0.5", "D2"))
# Etiquetas fuera de las barras
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()
# Etiquetas dentro de las barras
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

### DIAGRAMAS DE BARRAS DE CONTEOS 
head(mtcars)
# No asigne una variable a y  ### ERROR ###
ggplot(mtcars, aes(x=factor(cyl)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()
# Colores de contorno
p<-ggplot(df, aes(x=dose, y=len, color=dose)) +
  geom_bar(stat="identity", fill="white")
p
# Colores de relleno
p<-ggplot(df, aes(x=dose, y=len, fill=dose)) +
  geom_bar(stat="identity")+theme_minimal()
p
# Usa contorno negro
ggplot(df, aes(x=dose, y=len, fill=dose))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_minimal()

## POSCICION DE LEYENDA
# Cambiar los colores de relleno de la barra a azules
p <- p+scale_fill_brewer(palette="Blues")
p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none")
# Orden de elementos en la leyenda
p + scale_x_discrete(limits=c("D2", "D0.5", "D1"))

### DIAGRAMAS DE BARRAS CON VARIOS GRUPOS
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

# Gráfica de barras apiladas con múltiples grupos 
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")
# Usar position=position_dodge()
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())
# Cambiar colores
p <- ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
# Colores personalizados
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Paleta de colores cerveceros
p + scale_fill_brewer(palette="Blues")

## AGREGAR ETIQUETAS
# Barras esquivado
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
# Barras apilado
library(plyr)
# Ordenar por dosis y suplemento
df_sorted <- arrange(df2, dose, supp) 
head(df_sorted)
# Calcule la suma acumulada de len para cada dosis
df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))
head(df_cumsum)
# Crear el grafico de barras
ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
# Etiquetas en medio de las barras
df_cumsum <- ddply(df_sorted, "dose",
                   transform, 
                   label_ypos=cumsum(len) - 0.5*len)
# Crear el grafico 
ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

### DIAGRAMA DE BARRAS CON UN EJE X NUMERICO
# Crear datos
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("0.5", "1", "2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)
# Eje x tratado como variable continua
df2$dose <- as.numeric(as.vector(df2$dose))
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
# Eje tratado como variable discreta
df2$dose<-as.factor(df2$dose)
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

### GRAFICAS DE BARRAS CON BARRAS DE ERROR
# Función para calcular la media y la desviación estándar para cada grupo
# data: un marco de datos
# varname: el nombre de una columna que contiene la variable para ser resumido
# groupnames : vector de nombres de columna que se utilizará como variables de agrupación
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
# Resumen de datos
df3 <- data_summary(ToothGrowth, varname="len", 
                    groupnames=c("supp", "dose"))
# Convertir dose en una variable de factor
df3$dose=as.factor(df3$dose)
head(df3)
# Desviación estándar de la media como barra de error
p <- ggplot(df3, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal()

### DIAGRAMAS DE BARRAS PERSONALIZADOS
# Cambiar color por grupos
# Agregar barras de error
p + labs(title="Plot of length  per dose", 
         x="Dose (mg)", y = "Length")+
  scale_fill_manual(values=c('black','lightgray'))+
  theme_classic()
# Cambiar colores
p + scale_fill_brewer(palette="Greens") + theme_minimal()

p + scale_fill_brewer(palette="Reds") + theme_minimal()

########################## GRAFICAS DE DENSIDAD ################################

# ggplot(datos,aes(y=mivariable,fill=miclase))+geom_density()