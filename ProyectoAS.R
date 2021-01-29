#PROYECTo ANÁLISIS DE SUPERVIVENCIA
#Paqueterias-----
library(foreign)
library(survival)
library(KMsurv)
library(nlme)
library(muhaz)
library(TH.data)
library(ggplot2)
library(ggfortify)
library(proto)
library(GGally)
library(ggplot2)
library(randomForest)
#Base de datos----
data("pbc"); ?pbc
pbc <- pbc
str(pbc)
fc <- c(3,4,6,7,8,20)
for (i in fc) {
  pbc[,i] <- as.factor(pbc[,i])
}
str(pbc)
attach(pbc)
#----Análisis descriptivo----
summary(pbc)
#Imputación de datos faltantes con randomForest
pbc_sna <- rfImpute(time~.,pbc)
str(pbc_sna)
summary(pbc_sna)
#Tiempo de supervivencia----
hist(time)
ggplot(pbc,aes(x=time)) + geom_histogram()
#
