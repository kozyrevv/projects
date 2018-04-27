# title: "BAaSegmentacion.R"
# course: Business Analytics
# author: Alex Rayón (alex.rayon@deusto.es)
# last modified: September 9, 2017
# output: -

# Antes de nada, limpiamos el workspace, por si hubiera algún dataset o información cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Leemos los datos
teens <- read.csv("redes.csv")

# veamos la distribucion
table(teens$gradyear)

table(teens$gender)

summary(teens$age)
summary(teens$friends)

# preparamos los datos
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# Creamos una variable para indicar que no hay género
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# Imputamos la edad con la media
mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens$age, 
               teens$gradyear, 
               FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# Segmentacion
# 1. Tenemos en cuenta las 36 variables que contienen intereses y las escalamos
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

# Utilizamos 5 clusters
set.seed(1234)
teen_clusters <- kmeans(interests_z, 5)

library(cluster)

clusplot(interests_z,
         teen_clusters$cluster, 
         color=TRUE,
         col.clus=c(1:5)[unique(teen_clusters$cluster)],
         shade=TRUE,
         labels=4, 
         lines=0, 
         main = "Bivariate Cluster Plot")

library(fpc)
plotcluster(interests_z, teen_clusters$cluster)

# Veamos cuantos hay en cada segmento
teen_clusters$size

# Observemos los centroides de los clusters
teen_clusters$centers

# Cual seria el segmento de los deportistas?
# Obtener el significado de los clústers
aggregate(interests_z, by=list(teen_clusters$cluster), FUN=mean)

# Analizando el performance del modelo: la calidad del clustering depende de la calidad
# de los grupos así como las acciones que se realizan 

# Veamos la informacion de los 10 primeros
teens$cluster <- teen_clusters$cluster
teens[1:10,c("cluster","gender","age","friends")]

# veamos la distribucion de edad de los clusters
aggregate(data = teens, age ~ cluster, mean)

# el porcentaje de mujeres en los segmentos
aggregate(data = teens, female ~ cluster, mean)

# el numero de amigos
aggregate(data = teens, friends ~ cluster, mean)
