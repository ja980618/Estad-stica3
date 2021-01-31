#PROYECTO ANÁLISIS DE SUPERVIVENCIA

#                                                                Paqueterias  -----
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
library(gridExtra)
library(ggplot2)
library(randomForest)
library(survminer)

#                                                                 Base de datos
#Hacemos la base de datos manejable
    data("pbc")
    ? pbc
    pbc <- pbc
    str(pbc)
    fc <- c(3, 6, 7, 8, 9, 10, 20)
    for (i in fc) {
      pbc[, i] <- as.factor(pbc[, i])
    }
    str(pbc)
    summary(pbc)
#Imputación de datos faltantes con randomForest
    pbc$trt[is.na(pbc$trt)] <- 3
    pbc_sna <- rfImpute(time ~ ., pbc)
    pbc_sna$trt <- as.factor(pbc_sna$trt)
    str(pbc_sna)
    summary(pbc_sna)
    attach(pbc_sna)
#                                                       Análisis descriptivo  ----
#Dado que tenemos muchas variables, las dividimos en dos principales grupos
#numéricas y categóricas.

#Tiempo de supervivencia

ggplot(pbc, aes(x = time, fill = ..x..)) +
  geom_histogram(aes(y = ..density..) ,
                 bins = 10,
                 color = 'white',
                 alpha = 0.8) +
  scale_fill_gradient(low = '#390E7F', high = '#2171b5') +
  geom_density(
    color = '#deebf7',
    size = 0.7,
    fill = '#9ecae1',
    alpha = 0.2
  ) +
  geom_vline(
    aes(xintercept = mean(pbc$time)),
    linetype = 'dashed',
    color = '#2171b5',
    size = 0.8
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    y = 'Frecuencia',
    x = 'Tiempo de suervivencia',
    subtitle = 'Información sobre Cirrosis Biliar Primaria',
    title = 'Histograma'
  ) + theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10, color = 'grey20'),
    axis.title.y = element_text(size = 10, color = 'grey20'),
    legend.position = "none"
  )

colores <- c("lightblue", "lightgreen", "yellow", "pink")
# Variables categóricas ----
g1 <-
  ggplot(pbc_sna, aes(x = status)) + geom_bar(fill = colores[1:3]) + ggtitle("Status") +
  theme_classic()
g2 <-
  ggplot(pbc_sna, aes(x = trt)) + geom_bar(fill = colores[1:3]) + ggtitle("Tratamiento") +
  theme_classic()
g3 <-
  ggplot(pbc_sna, aes(x = sex)) + geom_bar(fill = colores[1:2]) + ggtitle("Sex") +
  theme_classic()
g4 <-
  ggplot(pbc_sna, aes(x = ascites)) + geom_bar(fill = colores[1:2]) + ggtitle("ascites") +
  theme_classic()
g5 <-
  ggplot(pbc_sna, aes(x = hepato)) + geom_bar(fill = colores[1:2]) + ggtitle("hepato") +
  theme_classic()
g6 <-
  ggplot(pbc_sna, aes(x = spiders)) + geom_bar(fill = colores[1:2]) + ggtitle("spiders") +
  theme_classic()
g7 <-
  ggplot(pbc_sna, aes(x = edema)) + geom_bar(fill = colores[1:3]) + ggtitle("edema") +
  theme_classic()
g8 <-
  ggplot(pbc_sna, aes(x = stage)) + geom_bar(fill = colores) + ggtitle("Stage") +
  theme_classic()
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 4)

# Variables numéricas   ----
#Dado que tenemos muchas variables numéricas y es iterativa la forma de hacer las gráficas
#hemos decidido automatixar el procesos de graficado para que no se extienda demasiado el 
#código:

variables_num <- c(5,11,12,13,14,15,16,17,18,19)
nombres <- colnames(pbc_sna)[variables_num]
low_color <- c('#deebf7','#deebf7','#98a222','#ffa36f', '#ffa36f', '#75287a',
               '#d08fa6', '#45e900', '#be4a2f','#58599b','#202143')
high_color <- c('#2171b5','#2171b5','#70771f','#d65500','#d65500','#2a152a',
                '#950d55','#234915','#622415','#202143')
h_c <- list(10)
for (i in 1:10) {
  h_c[[i]]<- ggplot(pbc, aes(x = pbc_sna[,variables_num[i]], fill = ..x..)) +
    geom_histogram(aes(y = ..density..) ,
                   bins = 10,
                   color = 'white',
                   alpha = 0.8) +
    scale_fill_gradient(low = low_color[i], high = high_color[i]) +
    geom_density(
      color = '#deebf7',
      size = 0.7,
      fill = '#9ecae1',
      alpha = 0.2
    ) +
    geom_vline(
      aes(xintercept = mean(pbc_sna[,variables_num[i]])),
      linetype = 'dashed',
      color = '#2171b5',
      size = 0.8
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      y = 'Frecuencia',
      x = nombres[i],
      title = nombres[i]
    ) + theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_text(size = 10, color = 'grey20'),
      axis.title.y = element_text(size = 10, color = 'grey20'),
      legend.position = "none"
    )  
}
grid.arrange(h_c[[1]],h_c[[2]],h_c[[3]],h_c[[4]],h_c[[5]],h_c[[6]],ncol=3)
grid.arrange(h_c[[7]],h_c[[8]],h_c[[9]],h_c[[10]],ncol=2)

# Comentario            ----
#Dado que, además de tener falla y censura en el estudio, se considera el transplante
#de hígado, vamos a agruparlo como censura. La razón es que cuando un paciente que 
#vive con la enfermedad recibe un hígado, el estudio para conocer la efectividad del 
#medicamento se ve afectado. No se agrupa como falla porque el paciente no muere.
status1 <- gsub(pattern = 2, replacement = 1,  gsub(pattern = 1, replacement = 0, pbc_sna$status))
table(status1)
status1 <- as.numeric(status1)

#                                                       Análisis Estadístico  ----
# 1. Obtenga el estimador K-M
fit_pbc <-survfit(Surv(pbc_sna$time ,as.numeric(status1))~1,type="kaplan-meier",conf.type='plain')
ggsurvplot(fit_pbc, data = pbc_sna, palette = c('#390E7F','#390E7F'),conf.int = T,censor = F)

# 2. Ver la significancia de las covariables

# Variables numéricas   ----

  # age: edad en años del paciente.

  # albumin: albúmina de suero (g/dl).

  # alk.phos: fosfotasa alcalina (U/liter).
  
  # ast: aspartate aminotransferase, once called SGOT (U/ml).
  
  # bili: serum bilirunbin (mg/dl).
  
  # chol: serum cholesterol (mg/dl).
  
  # copper: cobre en la orina (ug/day).
  
  # platelet: cuenta de plaquetas.
  
  # protime: (standardised blood clotting time ) tiempo estandarizado de coagulación de la sangre.
  
  # trig: triglicéridos (mg/dl).


# Variables Categóricas ----

  # ascites: presencia de ascitis valores 0 y 1.
fit_ascites <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$ascites, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_ascites <- ggsurvplot(fit_ascites, data = pbc_sna, palette = c('#F9F918',"#929203"),conf.int = T, censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1) ~ pbc_sna$ascites, rho=0)
survdiff(Surv(pbc_sna$time,status1) ~ pbc_sna$ascites, rho=1)


  # edema: 0 no edema, 0.5 untreated or successfully treated 1 edema despite diuretic therapy.
fit_edema <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$edema, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_edema <- ggsurvplot(fit_edema, data = pbc_sna, palette = c('#ED4E17','#EEA60C','#F5D317'),conf.int = T,censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$edema, rho=0)
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$edema, rho=1)


  # hepato: presence of hepatomegaly or enlarged liver.
fit_hepato <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$hepato, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_hepato <- ggsurvplot(fit_hepato, data = pbc_sna, palette = c('#98FC2D',"#4D9203"),conf.int = T,censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$hepato, rho=0)
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$hepato, rho=1)


  # spiders: (blood vessel malformations in the skin) Malformación en los vasos sanguíneos de la piel, con valores 0 y 1 .
fit_spiders <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$spiders, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_spiders <- ggsurvplot(fit_spiders, data = pbc_sna, palette = c('#9E0081',"#C75DB4"),conf.int = T,censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$spiders, rho=0)
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$spiders, rho=1)


# stage:	Estado de la enfermedad (después de la biopsia).
fit_stage <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$stage, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_stage <- ggsurvplot(fit_stage, data = pbc_sna, palette = c('#8B0542',"#F30672", "#F30635", "#B30326"),conf.int = T,censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$stage, rho=0)
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$stage, rho=1)


  # trt: Valores 1, 2 y NA for D-penicillmain, placebo, not randomised.
fit_trt <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$trt, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_trt <- ggsurvplot(fit_trt, data = pbc_sna, palette = c('#DF044A',"#F96C99", "#7D0229"),conf.int = T,censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$trt, rho=0)
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$trt, rho=1)


# sex: m/f (male/female) valores hombre  y mujer.
fit_sex <- survfit(Surv(pbc_sna$time,status1)~pbc_sna$sex, type='kaplan-meier', conf.type='plain')
      #Gráfico
g_sex <- ggsurvplot(fit_sex, data = pbc_sna, palette = c('#FC6C2D',"#9E3000"),conf.int = T,censor = F)
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$sex, rho=0)
survdiff(Surv(pbc_sna$time,status1)~pbc_sna$sex, rho=1)

# Las variables categóricas que aceptan H0, es decir que son iguales (sus funciones de supervivencia 
# comparando las subcategorías) no se considerarán en el modelo de riesgos proporcionales dado que 
# no hay diferencia entre una función de supervicencia y la otra en esa categoría y por tanto 
# considerarla en el modelo de riesgos proporcionales como una categoría, no haría diferencia en la
# estimación en el timepo de superivencia sentro del modelo. 


arrange_ggsurvplots(list(g_ascites, g_edema ,g_hepato, g_spiders, g_stage, g_trt, g_sex))








