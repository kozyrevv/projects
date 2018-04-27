# title: "BBKLive"
# course: Sunpers Sunglasses - Estrategias omnicanal
# author: Alex Rayón
# date: September 2, 2017
#install.packages("rstudioapi") install this package first !!! 
# Cargamos todas las librerías necesarias para trabajar con los datos en este caso
packages <- c("class","caret","neuralnet","DMwR","ROSE","gridExtra","ggplot2","ggrepel","clusterSim","rpart","reshape2","C50","arules","arulesViz", "rattle","rpart.plot","corrplot")
new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)

# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# Leemos los datos
datosEstrategiaDigital = read.csv("AnalyticsSunpers.csv", header =TRUE, sep =",", fill=TRUE)
# ¿De qué os dais cuenta?

# La lectura de datos siempre puede dar problemas de codificación de caracteres
datosEstrategiaDigital = read.csv("AnalyticsSunpers.csv", header =TRUE, sep =",", fill=TRUE, encoding="UTF-8")

# Y ahora nos aparecen nuevos problemas de datos....

#####################################
# METODOLOGÍA CRISP-DM
# 
#   1. Business understanding
#   2. Data understanding
#   3. Data preparation
#   4. Modeling
#   5. Evaluation
#   6. Deployment
#####################################

#####################################
# 1. Business understanding
#####################################

# Mostrar la presentación


#####################################
# 2. Data understanding
#####################################

# Vemos las estadísticas de los datos
dim(datosEstrategiaDigital)
summary(datosEstrategiaDigital)
str(datosEstrategiaDigital)

#####################################
# 3. Data preparation
#####################################
# Para manejarnos a lo largo de todo el script
datos<-datosEstrategiaDigital

# 1. La fecha en este caso va a ser un campo de identificación: lo hacemos factor
colnames(datos)[1]<-"fecha"
datos$fecha <- factor(datos$fecha)
str(datos)
summary(datos)

# 2. En este caso, ya tiene el formato que nos gustaría, así que simplemente actualizamos el nombre de la columna
colnames(datos)[2]<-"busquedasOrganicas"

# 3. Avg..Session.Duration
colnames(datos)[3]<-"duracionSesionMedia"
x<-strptime(datos$duracionSesionMedia,"%H:%M:%S")
y<-strftime(x, '%H:%M:%S')
datos$duracionSesionMedia<-as.numeric(substring(y,4,5))*60 + as.numeric(substring(y,7,8))

# 4. Bounces
colnames(datos)[4]<-"rebotes"
datos$rebotes<-gsub(",", "", datos$rebotes)
datos$rebotes<-as.numeric(datos$rebotes)

# 5. Exits
colnames(datos)[5]<-"salidas"
datos$salidas<-gsub(",", "", datos$salidas)
datos$salidas<-as.numeric(datos$salidas)

# 6. New.Users
colnames(datos)[6]<-"nuevosUsuarios"
datos$nuevosUsuarios<-gsub(",", "", datos$nuevosUsuarios)
datos$nuevosUsuarios<-as.numeric(datos$nuevosUsuarios)

# 7. Sessions
colnames(datos)[7]<-"sesiones"
datos$sesiones<-gsub(",", "", datos$sesiones)
datos$sesiones<-as.numeric(datos$sesiones)

# 8. Pageviews
colnames(datos)[8]<-"paginasVistas"
datos$paginasVistas<-gsub(",", "", datos$paginasVistas)
datos$paginasVistas<-as.numeric(datos$paginasVistas)

# 9. Avg..Order.Value
colnames(datos)[9]<-"carritoMedio"
datos$carritoMedio<-as.character(datos$carritoMedio)
datos$carritoMedio<-substring(datos$carritoMedio,2,nchar(datos$carritoMedio))
datos$carritoMedio<-as.numeric(datos$carritoMedio)

# 10. Buy.to.Detail.Rate
#   number of products purchased per number of product-detail views)
colnames(datos)[10]<-"BDR"
datos$BDR <- as.character(datos$BDR)
datos$BDR <-substring(datos$BDR,1,nchar(datos$BDR)-1)
datos$BDR <- as.numeric(datos$BDR)

# 11. Cart.to.Detail.Rate 
#   (number of products added per number of product-detail views)
colnames(datos)[11]<-"CDR"
datos$CDR <- as.character(datos$CDR)
datos$CDR <-substring(datos$CDR,1,nchar(datos$CDR)-1)
datos$CDR <- as.numeric(datos$CDR)

# 12. Ecommerce.Conversion.Rate
colnames(datos)[12]<-"CR"
datos$CR <- as.character(datos$CR)
datos$CR <-substring(datos$CR,1,nchar(datos$CR)-1)
datos$CR <- as.numeric(datos$CR)

# 13. Product.Adds.To.Cart 
colnames(datos)[13]<-"PAC"
datos$PAC <- as.character(datos$PAC)
datos$PAC <- as.numeric(datos$PAC)

# 14. Per.Session.Value
colnames(datos)[14]<-"valorSesion"
datos$valorSesion<-as.character(datos$valorSesion)
datos$valorSesion<-substring(datos$valorSesion,2,nchar(datos$valorSesion))
datos$valorSesion<-as.numeric(datos$valorSesion)

# 15. Product.Removes.From.Cart
colnames(datos)[15]<-"PRC"

# 16. Transactions
colnames(datos)[16]<-"transacciones"

# 17. Refunds
colnames(datos)[17]<-"devoluciones"
sum(datos$devoluciones==0)
nrow(datos)
datos$devoluciones=NULL

# 17. Product.List.CTR
colnames(datos)[17]<-"PLCTR"
datos$PLCTR <- as.character(datos$PLCTR)
datos$PLCTR <-substring(datos$PLCTR,1,nchar(datos$PLCTR)-1)
datos$PLCTR <- as.numeric(datos$PLCTR)

# 18. Product.List.Clicks
colnames(datos)[18]<-"PLC"

# 19. Product.Checkouts 
colnames(datos)[19]<-"checkouts"

# 20. Revenue
colnames(datos)[20]<-"ingresos"
datos$ingresos<-as.character(datos$ingresos)
datos$ingresos<-substring(datos$ingresos,2,nchar(datos$ingresos))
datos$ingresos<-gsub(",", "", datos$ingresos)
datos$ingresos<-as.numeric(datos$ingresos)

# 21. Clicks
colnames(datos)[21]<-"clicks"
datos$clicks<-gsub(",", "", datos$clicks)
datos$clicks<-as.numeric(datos$clicks)

# 22. Cost.per.Conversion
colnames(datos)[22]<-"CPA"
datos$CPA<-as.character(datos$CPA)
datos$CPA<-substring(datos$CPA,2,nchar(datos$CPA))
datos$CPA<-as.numeric(datos$CPA)

# 23. ROAS
datos$ROAS <- as.character(datos$ROAS)
datos$ROAS <-substring(datos$ROAS,1,nchar(datos$ROAS)-1)
datos$ROAS <- as.numeric(datos$ROAS)

# 24. CPC
datos$CPC<-as.character(datos$CPC)
datos$CPC<-substring(datos$CPC,2,nchar(datos$CPC))
datos$CPC<-as.numeric(datos$CPC)

# 25. CTR
datos$CTR <- as.character(datos$CTR)
datos$CTR <-substring(datos$CTR,1,nchar(datos$CTR)-1)
datos$CTR <- as.numeric(datos$CTR)

# A ver si tenemos ya todos los datos preparados para empezar a jugar.....
str(datos)


# Vamos a dibujar
boxplot(datos[,-1],las=2)
# ¿Qué observamos? 
# ¿Y si normalizamos primero? Tipos de normalización
# n0 - without normalization
# n1 - standardization ((x-mean)/sd)
# n2 - positional standardization ((x-median)/mad)
# n3 - unitization ((x-mean)/range)
# n3a - positional unitization ((x-median)/range)
# n4 - unitization with zero minimum ((x-min)/range)
# n5 - normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))
# n5a - positional normalization in range <-1,1> ((x-median)/max(abs(x-median)))
# n6 - quotient transformation (x/sd)
# n6a - positional quotient transformation (x/mad)
# n7 - quotient transformation (x/range)
# n8 - quotient transformation (x/max)
# n9 - quotient transformation (x/mean)
# n9a - positional quotient transformation (x/median)
# n10 - quotient transformation (x/sum)
# n11 - quotient transformation (x/sqrt(SSQ))
# n12 - normalization ((x-mean)/sqrt(sum((x-mean)^2)))
# n12a - positional normalization ((x-median)/sqrt(sum((x-median)^2)))
# n13 - normalization with zero being the central point ((x-midrange)/(range/2))
datos_norm = data.Normalization(datos[,-1],type="n4",normalization = "column")

# ¿Cómo vamos a ver ahora la escala?
boxplot(datos_norm[,-1],las=2)
# ¿Hay diferencia? :-)

# Vamos a cruzar todas las variables
pairs(datos[,-1])
hist(datos$rebotes,main="",xlab="Rebotes",prob=T)
hist(datos$carritoMedio,main="",xlab="Carrito medio",prob=T)

# Nos preguntamos si todavía arrastramos algún valor nulo
sum(is.na(datos))
# Buscamos donde pueden estar los nulos
summary(datos)

# Vamos a hacer una pequeña detección de outliers
datos$outlier=FALSE
for (i in 1:ncol(datos)-1){
  columna = datos[,i]
  if (is.numeric(columna)){
    media = mean(columna)
    desviacion = sd(columna)
    datos$outlier = (datos$outlier | columna>(media+3*desviacion) | columna<(media-3*desviacion))
  }
}
# Marcamos los TRUE y FALSE
table(datos$outlier)

# Separamos el dataframe donde tenemos los outliers... creo que merecen un buen estudio
datos.para.estudiar = datos[datos$outlier,]

# Marcamos los outliers en el gr?fico, los eliminamos y dibujamos
datos=datos[!datos$outlier,]

# Y ya no necesitamos que haya una columna "outlier"
datos$outlier=NULL

# Dibujamos sin outliers
pairs(datos)

# Hay correlaciones entre las columnas?
matrizCorrelacion<-cor(datos[,-1])
matrizCorrelacion
cor(datos[,-1])

# De una manera gráfica
library(corrplot)
M = cor(datos[,-1])

corrplot(M, method="circle")
corrplot(M, method="square")
corrplot(M, method="ellipse")
corrplot(M, method="number")
corrplot.mixed(M,lower="number", upper="ellipse",main="con outliers")

#######################################################################
# 1. Análisis de regresión
#######################################################################
# Vamos a ver con qué nos hemos quedado
str(datos)
# Construimos dos modelos de regresión: técnica estadística tradicional para explicar "por qué"
modelo1 = lm(PAC ~ sesiones, datos)
modelo2 = lm(transacciones ~ busquedasOrganicas+PAC, datos)
modelo3 = lm(transacciones ~ CPC, datos)
# Vemos algunos estadísticos para estudiar el ajuste del modelo
summary(modelo1)
summary(modelo2)
summary(modelo3)

# ¿Y aquí qué pasa?
hist(datos$CPC)

# Probemos nuevas
modelo4 = lm(transacciones ~ ROAS, datos)
summary(modelo4)

# Representamos el modelo. Le decimos que en el plot de salida solo pinte 1 gráfico
par(mfrow=c(1,1))
plot(datos$sesiones,datos$PAC)
plot(datos$busquedasOrganicas, datos$transacciones)

# Parece que no explicamos mucha varianza. ¿Hacemos algo más sofisticado? 
modelo5<-lm(transacciones~busquedasOrganicas+ROAS+sesiones,datos)
summary(modelo5)

# ¿Por qué un GLM?
# https://i1.wp.com/2.bp.blogspot.com/-nCUXu6hM4Fs/Vbu8N8zReJI/AAAAAAAACNI/_hm_blhnz94/s1600/AllModels.png

# Para hacer un GLM, separamos datos de entrenamiento de los datos de test. Como hemos dicho antes, 
#   hay dos fases en el modelado analítico
#       1. Entrenar/crear un modelo con los datos disponibles
#       2. Utilizar el modelo resultante para hacer predicciones sobre futuros datos
# Distinguimos así dos fases: (1) Entrenamiento; (2) Validación. 
datos <- datos[,-1]

# Partimos los datos: para entrenamiento, un 75% de todos ellos
index <- sample(1:nrow(datos),round(0.75*nrow(datos)))
# 'Parte' de los datos que serán para entrenamiento
train <- datos[index,]
# 'Parte' de los datos que serán para validar
test <- datos[-index,]
# 1. Construimos el modelo con los datos de entrenamiento
glm.fit <- glm(transacciones~., data=train)
# Vemos cómo es el modelo
summary(glm.fit)
# Y con los datos de test, 2. Hacemos predicciones de futuros casos
pr.glm <- predict(glm.fit,test)
# Y vemos como es la medida de "calidad" del modelo con el MSE
MSE.glm <- sum((pr.glm - test$transacciones)^2)/nrow(test)
# Luego volveremos a este punto

#######################################################################
# 2. Análisis clúster
#######################################################################
# En este caso que tengo tantas variables... ¿qué me puede venir bien antes de hacer una clusterización?
# PCA de dicho dataframe
pca=princomp(na.omit(datos),center=TRUE,scale=TRUE) 
# Información de los Componentes Principales
summary(pca)
# Componentes de cada vector de componentes principales
pca$loadings
# En este vector cada fila es un día y cada columna una coordenada con respecto al correspondiente componente principal
pca$scores 

# Podemos en adelante: 
# Agrupar por "datos": pero utilizando los Componentes Principales
kc <- kmeans(na.omit(datos), 5)
kc$centers

# Vamos a visualizar los grupos
pairs(datos,col=kc$cluster,main="clustering, sin normalizar, sin outliers")
# Cargamos la librería "fpc", fpc: Flexible Procedures for Clustering 
library(fpc)
clusterConOutliers=dist(na.omit(datos), method = "euclidean")
cluster.stats(clusterConOutliers,kc$cluster)

# Variamos algunos parámetros para visualizar mejor el gráfico
clusplot(na.omit(datos), kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Le asignamos a cada día su cluster correspondiente, como una característica más
datos$Cluster <- kc$cluster 

# Obtener el significado de los clústers
aggregate(datos, by=list(kc$cluster), FUN=mean)

##########################################################################
# 3. Reglas de asociación
##########################################################################
# Volvemos al punto original de datos
datosRules <- datos
str(datosRules)

datosRules$busquedasOrganicas = discretize(datosRules$busquedasOrganicas,categories=5,method = "frequency")
datosRules$duracionSesionMedia = discretize(datosRules$duracionSesionMedia,categories=5,method = "frequency")
datosRules$rebotes = discretize(datosRules$rebotes,categories=5,method = "frequency")
datosRules$salidas = discretize(datosRules$salidas,categories=5,method = "frequency")
datosRules$nuevosUsuarios = discretize(datosRules$nuevosUsuarios,categories=5,method = "frequency")
datosRules$sesiones = discretize(datosRules$sesiones,categories=5,method = "frequency")
datosRules$paginasVistas = discretize(datosRules$paginasVistas,categories=5,method = "frequency")
datosRules$carritoMedio = discretize(datosRules$carritoMedio,categories=5,method = "frequency")
datosRules$BDR = discretize(datosRules$BDR,categories=5,method = "frequency")
datosRules$CDR = discretize(datosRules$CDR,categories=5,method = "frequency")
datosRules$CR = discretize(datosRules$ CR,categories=5,method = "frequency")
datosRules$PAC = discretize(datosRules$PAC,categories=5,method = "frequency")
datosRules$valorSesion = discretize(datosRules$valorSesion,categories=5,method = "frequency")
datosRules$PRC = discretize(datosRules$PRC,categories=5,method = "frequency")
datosRules$transacciones = discretize(datosRules$transacciones,categories=5,method = "frequency")
datosRules$PLCTR = discretize(datosRules$PLCTR,categories=5,method = "frequency")
datosRules$PLC = discretize(datosRules$PLC,categories=5,method = "frequency")
datosRules$checkouts = discretize(datosRules$checkouts,categories=5,method = "frequency")
datosRules$ingresos = discretize(datosRules$ingresos,categories=5,method = "frequency")
datosRules$clicks = discretize(datosRules$clicks,categories=5,method = "frequency")
datosRules$CPA = discretize(datosRules$CPA,categories=5,method = "frequency")
datosRules$ROAS = discretize(datosRules$ROAS,categories=5,method = "frequency")
datosRules$CPC = discretize(datosRules$CPC,categories=5,method = "frequency")
datosRules$CTR = discretize(datosRules$CTR,categories=5,method = "frequency")

# No nos hace falta el campo en el que decíamos el clúster al que pertenecía el día
datosRules$Cluster=NULL

str(datosRules)

# Construimos las reglas "apriori"
rules = apriori(datosRules, parameter = list(supp=0.15, conf=0.8,minlen = 3, maxlen=6))
summary(rules)
inspect(rules[1:10])

inspect(sort(rules, by="support"   , decreasing=TRUE)[1:10],itemSep = " & ",ruleSep="-->",linebreak=FALSE)
inspect(sort(rules, by="confidence", decreasing=TRUE)[1:10],itemSep = " & ",ruleSep="-->",linebreak=FALSE)
inspect(sort(rules, by="lift"      , decreasing=TRUE)[1:10],itemSep = " & ",ruleSep="-->",linebreak=FALSE)

# Visualizamos las reglas de asociación
rules2 = subset(rules, subset = (rhs %in% "salidas=[   1, 933)"))
plot(rules2, method="graph")

# Podemos sacar las reglas que "apuntan a"....
rules_toRefundsZero <- subset(rules, subset = rhs %in% "Refunds=0")
rules_toRefundsZero<-sort(rules_toRefundsZero, by="lift", decreasing=TRUE)
inspect(rules_toRefundsZero)

# Podemos sacar las reglas que "salen" de....
rules_fromCTRzero <- subset(rules, subset = lhs %in% "PLC=[  0,131)")
rules_fromCTRzero<-sort(rules_fromCTRzero, by="lift", decreasing=TRUE)
inspect(rules_fromCTRzero)

# También podemos quedarnos en los itemsets, para evaluar apariciones
rules <- apriori(datosRules, parameter = list(supp = 0.001,maxlen=3,minlen=3,target = "frequent"))
rules<-sort(rules, by="support", decreasing=FALSE)
inspect(rules[1:15])
# ¿Qué información nos da todo esto para hacer campañas?
