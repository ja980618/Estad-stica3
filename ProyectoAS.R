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
colores <- c("lightblue","lightgreen","yellow","pink")
#Base de datos----
data("pbc"); ?pbc
pbc <- pbc
str(pbc)
fc <- c(3,4,6,7,8,9,10,20)
for (i in fc) {
  pbc[,i] <- as.factor(pbc[,i])
}
str(pbc)
#----Análisis descriptivo----
summary(pbc)
#Imputación de datos faltantes con randomForest
pbc_sna <- rfImpute(time~.,pbc)
str(pbc_sna)
summary(pbc_sna)
attach(pbc_sna)
#Tiempo de supervivencia----
hist(time)
ggplot(pbc,aes(x=time)) + geom_histogram(bins = 10)
#Status----
table(status)
#Tratamiento
table(trt)
#Edad----
hist(age)
#Sexo----
table(sex)
#Ascites----
table(ascites)
#Graficas de barras----
library(gridExtra)
g1<-ggplot(pbc_sna,aes(x=status)) +geom_bar(fill=colores[1:3])+ggtitle("Status")+theme_classic()
g2<-ggplot(pbc_sna,aes(x=trt)) +geom_bar(fill=colores[1:2])+ggtitle("Tratamiento")+theme_classic()
g3<-ggplot(pbc_sna,aes(x=sex)) +geom_bar(fill=colores[1:2])+ggtitle("Sex")+theme_classic()
g4<-ggplot(pbc_sna,aes(x=ascites)) +geom_bar(fill=colores[1:2])+ggtitle("ascites")+theme_classic()
g5<-ggplot(pbc_sna,aes(x=hepato)) +geom_bar(fill=colores[1:2])+ggtitle("hepato")+theme_classic()
g6<-ggplot(pbc_sna,aes(x=spiders)) +geom_bar(fill=colores[1:2])+ggtitle("spiders")+theme_classic()
g7<-ggplot(pbc_sna,aes(x=edema)) +geom_bar(fill=colores[1:3])+ggtitle("edema")+theme_classic()
g8<-ggplot(pbc_sna,aes(x=stage)) +geom_bar(fill=colores)+ggtitle("Stage")+theme_classic()
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8, ncol=4)




