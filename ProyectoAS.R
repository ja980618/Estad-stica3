#                       PROYECTO ANÁLISIS DE SUPERVIVENCIA
# Equipo:
# 6.  Avila Argüello Carlos
# 10. Bonilla Cruz José Armando
# 39. Gutierrez Luna Yanley
# 64. Reyes González Belén
# 67. Rivera Mata Dante Tristán
#                                                      Paqueterias -----
library(foreign)
library(survival)
library(nlme)
library(ggplot2)

library(survminer)
library(KMsurv)
library(muhaz)
library(TH.data)
library(ggfortify)
library(proto)
library(GGally)
library(gridExtra)
library(randomForest)
#                                                                 Base de datos
#Hacemos la base de datos manejable
data("pbc")
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
#                                             Análisis descriptivo ----
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
# Variables categóricas                 ----
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

# Variables numéricas                   ----
#Dado que tenemos muchas variables numéricas y es iterativa la forma de hacer las gráficas
#hemos decidido automatixar el procesos de graficado para que no se extienda demasiado el
#código:

variables_num <- c(5, 11, 12, 13, 14, 15, 16, 17, 18, 19)
nombres <- colnames(pbc_sna)[variables_num]
low_color <-
  c(
    '#deebf7',
    '#deebf7',
    '#98a222',
    '#ffa36f',
    '#ffa36f',
    '#75287a',
    '#d08fa6',
    '#45e900',
    '#be4a2f',
    '#58599b',
    '#202143'
  )
high_color <-
  c(
    '#2171b5',
    '#2171b5',
    '#70771f',
    '#d65500',
    '#d65500',
    '#2a152a',
    '#950d55',
    '#234915',
    '#622415',
    '#202143'
  )
h_c <- list(10)
for (i in 1:10) {
  h_c[[i]] <-
    ggplot(pbc, aes(x = pbc_sna[, variables_num[i]], fill = ..x..)) +
    geom_histogram(
      aes(y = ..density..) ,
      bins = 10,
      color = 'white',
      alpha = 0.8
    ) +
    scale_fill_gradient(low = low_color[i], high = high_color[i]) +
    geom_density(
      color = '#deebf7',
      size = 0.7,
      fill = '#9ecae1',
      alpha = 0.2
    ) +
    geom_vline(
      aes(xintercept = mean(pbc_sna[, variables_num[i]])),
      linetype = 'dashed',
      color = '#2171b5',
      size = 0.8
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::comma) +
    labs(y = 'Frecuencia',
         x = nombres[i],
         title = nombres[i]) + theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_text(size = 10, color = 'grey20'),
      axis.title.y = element_text(size = 10, color = 'grey20'),
      legend.position = "none"
    )
}
grid.arrange(h_c[[1]], h_c[[2]], h_c[[3]], h_c[[4]], h_c[[5]], h_c[[6]], ncol =
               3)
grid.arrange(h_c[[7]], h_c[[8]], h_c[[9]], h_c[[10]], ncol = 2)

# Comentario                            ----
#Dado que, además de tener falla y censura en el estudio, se considera el transplante
#de hígado, vamos a agruparlo como censura. La razón es que cuando un paciente que
#vive con la enfermedad recibe un hígado, el estudio para conocer la efectividad del
#medicamento se ve afectado. No se agrupa como falla porque el paciente no muere.
status1 <-
  gsub(
    pattern = 2,
    replacement = 1,
    gsub(pattern = 1, replacement = 0, pbc_sna$status)
    )
table(status1)
status1 <- as.numeric(status1)

#                                             Análisis Estadístico ----
# 1. Obtenga el estimador K-M
fit_pbc <-
  survfit(Surv(pbc_sna$time , as.numeric(status1)) ~ 1,
          type = "kaplan-meier",
          conf.type = 'plain')
ggsurvplot(
  fit_pbc,
  data = pbc_sna,
  palette = c('#390E7F', '#390E7F'),
  conf.int = T,
  censor = F, 
  surv.median.line = "hv",
  ggtheme = theme_bw()
)

# 2. Ver la significancia de las covariables

# Variables numéricas                   ----

#age: edad en años del paciente.
#Dado que tenemos diversas edades (49 diferentes en años) no es posible visualizar
#la función de supervivencia para cada una de tal forma que determinemos si existe o no
#diferencia por edad, por lo que agruparemos con el criterio siguiente:
#para los grupos se considerará el rango que va de la media de la edad menos la desviación
#estándar hasta la media de la edad más la desviación estándar y los otros dos grupos
#se considerarán el complemento de este.
#El grupo mayoritario será:-sqrt(var(pbc_sna$age)) + mean(pbc_sna$age) #de los 40
sqrt(var(pbc_sna$age)) + mean(pbc_sna$age)  #a los 60
grupos_age <-
  factor(
    ifelse(
      pbc_sna$age < 40,
      "age26-39",
      ifelse(age <= 60, "age40-60", "age>60")
    ),
    levels = c("age26-39", "age40-60", "age>60")
  )

fit_age <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_age,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_age <-
  ggsurvplot(
    fit_age,
    data = pbc_sna,
    palette = c('#D55757', "#D59457", "#E1C318"),
    conf.int = T,
    censor = F,
    title = "Edad",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('26-39', '40-60', '> 60'))
  )

# Tamaño leyenda
g_age$plot <-
  g_age$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_age, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_age, rho = 1)


# albumin: albúmina de suero (g/dl).
#Análogo a la explicación anterior vamos a proceder-sqrt(var(pbc_sna$albumin)) +
mean(pbc_sna$albumin) #de 3
sqrt(var(pbc_sna$albumin)) + mean(pbc_sna$albumin)  #a  4
grupos_albumin <-
  factor(ifelse(
    pbc_sna$albumin < 3,
    "albumin0-2.99",
    ifelse(pbc_sna$albumin <= 4, "albumin3-4", "albumin>4")
  ))

fit_albumin <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_albumin,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_albumin <-
  ggsurvplot(
    fit_albumin,
    data = pbc_sna,
    palette = c('#9c528b', "#2f0147", "#610f7f"),
    conf.int = T,
    censor = F,
    title = "Albumina de suero",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('> 4', '0-2.99', '3-4'))
  )

# Tamaño leyenda
g_albumin$plot <-
  g_albumin$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_albumin, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_albumin, rho = 1)


# alk.phos: fosfotasa alcalina (U/liter).
#Notemos que en este caso, los datos que tenemos generan una desviación estándar
#muy alta por lo cual es conveniente hacer otro criterio para seccionar esta variable

grupos_alk.phos <-
  factor(ifelse(pbc_sna$alk.phos < 3000, "alk.phos289-3000", "alk.phos>3000"))

fit_alk.phos <-
  survfit(
    Surv(pbc_sna$time, status1) ~ grupos_alk.phos,
    type = "kaplan-meier",
    conf.type = "plain"
  )
#Gráfico
g_alk.phos <-
  ggsurvplot(
    fit_alk.phos,
    data = pbc_sna,
    palette = c('#FE8423', "#FEB923"),
    conf.int = T,
    censor = F,
    title = "Fosfatasa alcalina",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('> 300', '289-300'))
  )

g_alk.phos$plot <-
  g_alk.phos$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_alk.phos, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_alk.phos, rho = 1)


# ast: aspartato aminotransferase, once called SGOT (U/ml).
#Análogo a la explicación anterior vamos a proceder-sqrt(var(pbc_sna$ast)) +
mean(pbc_sna$ast) #de 73
sqrt(var(pbc_sna$ast)) + mean(pbc_sna$ast)  #a  174

grupos_ast <-
  factor(ifelse(
    pbc_sna$ast < 73,
    "ast26-72.99",
    ifelse(pbc_sna$ast <= 174, "ast73-174", "ast>174")
  ))

fit_ast <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_ast,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_ast <-
  ggsurvplot(
    fit_ast,
    data = pbc_sna,
    palette = c('#51B85F', "#4798A3", "#47A387"),
    conf.int = T,
    censor = F,
    title = "Aspartato aminotransferasa",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('> 174', '26-72.99', '73-174'))
  )

g_ast$plot <-
  g_ast$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_ast, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_ast, rho = 1)


# bili: serum bilirunbin (mg/dl).
#Los criterios anteriores se vieron rebasados por el conocimiento de la medicina que nos dice que
#la partición conveniente es de 0 a 2, de 2 a 3 y mayor a 3.

grupos_bili <-
  factor(ifelse(
    pbc_sna$bili < 2,
    "bili.3-1.99",
    ifelse(pbc_sna$bili <= 3, "bili2-3", "bili>3")
  ))

fit_bili <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_bili,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_bili <-
  ggsurvplot(
    fit_bili,
    data = pbc_sna,
    palette = c('#729F2E', "#2E9F69", "#409F2E"),
    conf.int = T,
    censor = F,
    title = "Bilirrubina",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('0.3-1.99', '> 3', '2 - 3'))
  )

g_bili$plot <-
  g_bili$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_bili, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_bili, rho = 1)


# chol: serum cholesterol (mg/dl).
quantile(pbc_sna$chol)
grupos_chol <-
  factor(ifelse(pbc_sna$chol < 200, "chol 0-199.99", "chol>200"))

fit_chol <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_chol,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_chol <-
  ggsurvplot(
    fit_chol,
    data = pbc_sna,
    palette = c('#40916c', "#52b788"),
    conf.int = T,
    censor = F,
    title = "Colesterol",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('0-199.9', '> 200'))
  )

g_chol$plot <-
  g_chol$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_chol, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_chol, rho = 1)


# copper: cobre en la orina (ug/day).
#Análogo a la explicación anterior vamos a proceder
quantile(pbc_sna$copper)

grupos_copper <-
  factor(ifelse(
    pbc_sna$copper < 70,
    "copper4-69.99",
    ifelse(pbc_sna$copper <= 140, "copper70-140", "copper>140")
  ))

fit_copper <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_copper,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_copper <-
  ggsurvplot(
    fit_copper,
    data = pbc_sna,
    palette = c('#DB73E3', "#6E3971", "#A456AA"),
    conf.int = T,
    censor = F,
    title = "Cobre",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('> 140', '4-69.99', '70-140'))
  )

g_copper$plot <-
  g_copper$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_copper, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_copper, rho = 1)


# platelet: cuenta de plaquetas.
grupos_platelet <-
  factor(ifelse(pbc_sna$platelet < 200, "platelet0-199.99", "platelet>200"))

fit_platelet <-
  survfit(
    Surv(pbc_sna$time, status1) ~ grupos_platelet,
    type = "kaplan-meier",
    conf.type = "plain"
  )
#Gráfico
g_platelet <-
  ggsurvplot(
    fit_platelet,
    data = pbc_sna,
    palette = c('#6930c3', "#5e60ce"),
    conf.int = T,
    censor = F,
    title = "Plaquetas",
    xlab = '',
    ylab = '',
    legend.title = '',
    legend.labs = paste('', c('> 200', '0-199.99'))
  )

g_platelet$plot <-
  g_platelet$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_platelet, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_platelet, rho = 1)


# protime: (standardised blood clotting time ) tiempo estandarizado de coagulación de la sangre.
#Análogo a la explicación anterior vamos a proceder
quantile(pbc_sna$protime)

grupos_protime <-
  factor(ifelse(pbc_sna$protime < 11, "protime0-10.99", "protime>11"))
fit_protime <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_protime,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_protime <-
  ggsurvplot(
    fit_protime,
    data = pbc_sna,
    palette = c("#f28f3b", "#c8553d"),
    conf.int = T,
    censor = F,
    title = "Tiempo de coagulación",
    xlab = '',
    ylab = 'Probabilidad',
    legend.title = '',
    legend.labs = paste('', c('> 11', '0 - 10.99'))
  )

g_protime$plot <-
  g_protime$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_protime, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_protime, rho = 1)

# trig: triglicéridos (mg/dl).
#Análogo a la explicación anterior vamos a proceder
quantile(pbc_sna$trig)

grupos_trig <-
  factor(ifelse(pbc_sna$trig < 150, "trig0-150", "trig>150"))

fit_trig <-
  survfit(Surv(pbc_sna$time, status1) ~ grupos_trig,
          type = "kaplan-meier",
          conf.type = "plain")
#Gráfico
g_trig <-
  ggsurvplot(
    fit_trig,
    data = pbc_sna,
    palette = c('#da627d', "#a53860"),
    conf.int = T,
    censor = F,
    title = "Triglicéridos",
    xlab = '',
    ylab = 'Probabilidad',
    legend.title = '',
    legend.labs = paste('', c('> 150', '0-150'))
  )

g_trig$plot <-
  g_trig$plot + theme(
    legend.text = element_text(size = 9, color = "black", face = "plain"),
    axis.text.x = element_text(size = 8, color = "black", face = "plain"),
    axis.text.y = element_text(size = 8, color = "black", face = "plain")
  )

#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ grupos_trig, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ grupos_trig, rho = 1)


# Graficos juntos
arrange_ggsurvplots(
  list(
    g_protime,
    g_trig,
    g_age,
    g_albumin,
    g_alk.phos,
    g_copper,
    g_bili,
    g_platelet,
    g_chol
  ),
  ncol = 3,
  nrow = 3
)

# Variables Categóricas                 ----

# ascites: presencia de ascitis valores 0 y 1.
fit_ascites <-
  survfit(
    Surv(pbc_sna$time, status1) ~ pbc_sna$ascites,
    type = 'kaplan-meier',
    conf.type = 'plain'
  )
#Gráfico
g_ascites <-
  ggsurvplot(
    fit_ascites,
    data = pbc_sna,
    palette = c('#F9F918', "#929203"),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$ascites, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$ascites, rho = 1)


# edema: 0 no edema, 0.5 untreated or successfully treated 1 edema despite diuretic therapy.
fit_edema <-
  survfit(Surv(pbc_sna$time, status1) ~ pbc_sna$edema,
          type = 'kaplan-meier',
          conf.type = 'plain')
#Gráfico
g_edema <-
  ggsurvplot(
    fit_edema,
    data = pbc_sna,
    palette = c('#ED4E17', '#EEA60C', '#F5D317'),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$edema, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$edema, rho = 1)


# hepato: presence of hepatomegaly or enlarged liver.
fit_hepato <-
  survfit(Surv(pbc_sna$time, status1) ~ pbc_sna$hepato,
          type = 'kaplan-meier',
          conf.type = 'plain')
#Gráfico
g_hepato <-
  ggsurvplot(
    fit_hepato,
    data = pbc_sna,
    palette = c('#98FC2D', "#4D9203"),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$hepato, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$hepato, rho = 1)


# spiders: (blood vessel malformations in the skin) Malformación en los vasos sanguíneos de la piel, con valores 0 y 1 .
fit_spiders <-
  survfit(
    Surv(pbc_sna$time, status1) ~ pbc_sna$spiders,
    type = 'kaplan-meier',
    conf.type = 'plain'
  )
#Gráfico
g_spiders <-
  ggsurvplot(
    fit_spiders,
    data = pbc_sna,
    palette = c('#9E0081', "#C75DB4"),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$spiders, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$spiders, rho = 1)


# stage:	Estado de la enfermedad (después de la biopsia).
fit_stage <-
  survfit(Surv(pbc_sna$time, status1) ~ pbc_sna$stage,
          type = 'kaplan-meier',
          conf.type = 'plain')
#Gráfico
g_stage <-
  ggsurvplot(
    fit_stage,
    data = pbc_sna,
    palette = c('#8B0542', "#F30672", "#F30635", "#B30326"),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$stage, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$stage, rho = 1)


# trt: Valores 1, 2 y NA for D-penicillmain, placebo, not randomised.
fit_trt <-
  survfit(Surv(pbc_sna$time, status1) ~ pbc_sna$trt,
          type = 'kaplan-meier',
          conf.type = 'plain')
#Gráfico
g_trt <-
  ggsurvplot(
    fit_trt,
    data = pbc_sna,
    palette = c('#DF044A', "#F96C99", "#7D0229"),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$trt, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$trt, rho = 1)


# sex: m/f (male/female) valores hombre  y mujer.
fit_sex <-
  survfit(Surv(pbc_sna$time, status1) ~ pbc_sna$sex,
          type = 'kaplan-meier',
          conf.type = 'plain')
#Gráfico
g_sex <-
  ggsurvplot(
    fit_sex,
    data = pbc_sna,
    palette = c('#FC6C2D', "#9E3000"),
    conf.int = T,
    censor = F
  )
#Estadístico : H0:son iguales las funciones de supervivencia por subcategoría vs H1: son dsitintas
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$sex, rho = 0)
survdiff(Surv(pbc_sna$time, status1) ~ pbc_sna$sex, rho = 1)

# Las variables categóricas que aceptan H0, es decir que son iguales (sus funciones de supervivencia
# comparando las subcategorías) no se considerarán en el modelo de riesgos proporcionales dado que
# no hay diferencia entre una función de supervicencia y la otra en esa categoría y por tanto
# considerarla en el modelo de riesgos proporcionales como una categoría, no haría diferencia en la
# estimación en el timepo de superivencia sentro del modelo.


arrange_ggsurvplots(
  list(g_ascites, g_edema , g_hepato, g_spiders, g_stage, g_trt, g_sex),
  ncol = 4,
  nrow = 2
)


#                                                           Modelo ----
#Para poder realizar el modelo de las covariables que tengan importancia o 
#significancia en los riesgos proporcionales haremos las prubas de cox por cada variable.

#Si el p-value es menor que alpha, entonces rechazamos la hipótesis nula que dice que 
#"el coeficiente asociado al factor correspondiente es cero (lo cual implicaría que no es 
#segnificativo dicho factor para el modelo de riesgos proporcionales)" al nivel de significancia 
#del 20%. 

# Variables numéricas                   ----

#age
coxph(Surv(pbc_sna$time, status1)~pbc_sna$age)

#albumin
coxph(Surv(pbc_sna$time, status1)~pbc_sna$albumin)

#alk.phos
coxph(Surv(pbc_sna$time, status1)~pbc_sna$alk.phos)

#ast
coxph(Surv(pbc_sna$time, status1)~pbc_sna$ast)

#bili
coxph(Surv(pbc_sna$time, status1)~pbc_sna$bili)

#chol
coxph(Surv(pbc_sna$time, status1)~pbc_sna$chol)

#copper
coxph(Surv(pbc_sna$time, status1)~pbc_sna$copper)

#platelet
coxph(Surv(pbc_sna$time, status1)~pbc_sna$platelet)

#protime
coxph(Surv(pbc_sna$time, status1)~pbc_sna$protime)

#trig
coxph(Surv(pbc_sna$time, status1)~pbc_sna$trig)

# Variables categóricas                 ----

#ascites
coxph(Surv(pbc_sna$time, status1)~pbc_sna$ascites)

#edema
coxph(Surv(pbc_sna$time, status1)~pbc_sna$edema)

#hepato
coxph(Surv(pbc_sna$time, status1)~pbc_sna$hepato)

#sex
coxph(Surv(pbc_sna$time, status1)~pbc_sna$sex)

#spiders
coxph(Surv(pbc_sna$time, status1)~pbc_sna$spiders)

#stage
coxph(Surv(pbc_sna$time, status1)~pbc_sna$stage)

#trt
coxph(Surv(pbc_sna$time, status1)~pbc_sna$trt)

# El modelo.                            ----
#Para poder contrastar los resultados analicemos el modelo 0 que 
#considerarará todas las covariables (pesea a que no sean significativas)
modelo_0 <- coxph(Surv(pbc_sna$time, status1)~
                  pbc_sna$age
                + pbc_sna$albumin
                + pbc_sna$ast
                + pbc_sna$bili
                + pbc_sna$copper
                + pbc_sna$platelet
                + pbc_sna$protime
                + pbc_sna$trig
                + pbc_sna$ascites
                + pbc_sna$edema
                + pbc_sna$hepato
                + pbc_sna$spiders
                + pbc_sna$stage
                + pbc_sna$sex
                + pbc_sna$chol
                + pbc_sna$alk.phos
                + pbc_sna$trt)
# H0: los riesgos son proporcionales vs H1: los riesgos NO SON proporcionales
cox.zph(modelo_0)
modelo <- coxph(Surv(pbc_sna$time, status1)~
                  pbc_sna$age
                + pbc_sna$albumin
                + pbc_sna$ast
                + pbc_sna$copper
                + pbc_sna$platelet
                + pbc_sna$ascites
                + pbc_sna$edema
                + pbc_sna$hepato
                + pbc_sna$spiders
                + pbc_sna$stage)
# H0: los riesgos son proporcionales vs H1: los riesgos NO SON proporcionales
cox.zph(modelo)
df <- data.frame(modelo$coefficients, 
                 confint(modelo, level = .95),
                 exp(modelo$coefficients),
                 row.names = c('age', 'albumin', 'ast', 
                              'copper', 'platelet', 'ascites1',
                              'edema0.5','edema1', 'hepato1', 
                              'spiders1', 'stage2','stage3',
                              'stage4') )
colnames(df) <- c('Coeficientes', '2.5%', '97.5%', 'Exp(Coef)')
df

# ¿Las variables tienen efecto en el modelo?

# Residuos martingala: notar linealidad ----
residuos_mtg <- residuals(modelo,type='martingale')
par(mfrow = c(1,2))
#Age
plot(
  pbc_sna$age,
  residuos_mtg,
  xlab = "Edad",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_mtg ~ pbc_sna$age),
      col = "red",
      lwd = 2)
lines(pbc_sna$age, fitted(lm(residuos_mtg ~ pbc_sna$age)), col = "purple", lwd = 2)

#albumin
plot(
  pbc_sna$albumin,
  residuos_mtg,
    xlab = "Albúmina",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_mtg ~ pbc_sna$albumin),
      col = "red",
      lwd = 2)
lines(pbc_sna$albumin, fitted(lm(residuos_mtg ~ pbc_sna$albumin)), col = "purple", lwd = 2)

#ast
plot(
  pbc_sna$ast,
  residuos_mtg,
  xlab = "Ast",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_mtg ~ pbc_sna$ast),
      col = "red",
      lwd = 2)
lines(pbc_sna$ast, fitted(lm(residuos_mtg ~ pbc_sna$ast)), col = "purple", lwd = 2)

#copper
plot(
  pbc_sna$copper,
  residuos_mtg,
  xlab = "Cobre en orina",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_mtg ~ pbc_sna$copper),
      col = "red",
      lwd = 2)
lines(pbc_sna$copper, fitted(lm(residuos_mtg ~ pbc_sna$copper)), col = "purple", lwd = 2)

#platelet
plot(
  pbc_sna$platelet,
  residuos_mtg,
  xlab = "Plaquetas en la sangre",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_mtg ~ pbc_sna$platelet),
      col = "red",
      lwd = 2)
lines(pbc_sna$platelet, fitted(lm(residuos_mtg ~ pbc_sna$platelet)), col = "purple", lwd = 2)

# Valores outliers/influyentes: dfBetas ----
residuos_dfbetas <- resid(modelo, type = "dfbeta")

nombres <- row.names(df)
colores <-
  c(
    "#96DBD4",
    "#96BFDB",
    "#969DDB",
    "#B296DB",
    "#D496DB",
    "#DB96BF",
    '#DD866E',
    '#DDBE6E',
    '#C5DD6E',
    '#8EDD6E',
    '#6EDD86',
    '#6EDDBE',
    "orange"
    )

par(mfrow =c(2,3))
for (i in 1:6) {
  plot(
    residuos_dfbetas[,i],
    xlab = "Modelo Propuesta",
    ylab = "dfBeta",
    main = nombres[i],
    pch = 19,
    cex = 0.5,
    col = colores[i],
    type = "h"
  )
  identify(residuos_dfbetas[,i], labels = pbc_sna$id)
}
par(mfrow =c(2,3))
for (i in 7:12) {
  plot(
    residuos_dfbetas[,i],
    xlab = "Modelo Propuesta",
    ylab = "dfBeta",
    main = nombres[i],
    pch = 19,
    cex = 0.5,
    col = colores[i],
    type = "h"
  )
  identify(residuos_dfbetas[,i], labels = pbc_sna$id)
}
par(mfrow =c(1,1))
plot(
  residuos_dfbetas[,13],
  xlab = "Modelo Propuesta",
  ylab = "dfBeta",
  main = nombres[13],
  pch = 19,
  cex = 0.5,
  col = colores[13],
  type = "h"
)
identify(residuos_dfbetas[,13], labels = pbc_sna$id)

# Datos atípicos                        ----
#La función identify nos sirvió para identificar los datos atípicos.
atipicos <- sort(c(23, 253, 317, 166, 314, 293, 371, 52))

#Quitamos los datos atípicos 
pbc_sna_sda <- pbc_sna[-atipicos,]
status_bien <- status1[-atipicos]

#                                                     Modelo final ----
modelo_bien <- coxph(Surv(pbc_sna_sda$time, status_bien)~
                    pbc_sna_sda$age
                  + pbc_sna_sda$albumin
                  + pbc_sna_sda$ast
                  + pbc_sna_sda$copper
                  + pbc_sna_sda$platelet
                  + pbc_sna_sda$ascites
                  + pbc_sna_sda$edema
                  + pbc_sna_sda$hepato
                  + pbc_sna_sda$spiders
                  + pbc_sna_sda$stage)

# Verificación de supuestos             ----

# Comparando las gráficas anteriores sobre los supuestos de linealidad
# (residuos martinalas) y los datos atídicos (dfBetas) podremos notar facilmente
# que el modelo que no considera datos atípicos cumple mejor los supuestos para 
# un modelo de Cox (riesgos proporcionales).
residuos_bien_mtg <- residuals(modelo_bien, type='martingale')
residuos_bien_dfbetas <- resid(modelo_bien, type = "dfbeta")
#Age
plot(
  pbc_sna_sda$age,
  residuos_bien_mtg,
  xlab = "Edad",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta sda",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_bien_mtg ~ pbc_sna_sda$age),
      col = "red",
      lwd = 2)
lines(pbc_sna_sda$age, fitted(lm(residuos_bien_mtg ~ pbc_sna_sda$age)), col = "purple", lwd = 2)

#albumin
plot(
  pbc_sna_sda$albumin,
  residuos_bien_mtg,
  xlab = "Albúmina",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta sda",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_bien_mtg ~ pbc_sna_sda$albumin),
      col = "red",
      lwd = 2)
lines(pbc_sna_sda$albumin, fitted(lm(residuos_bien_mtg ~ pbc_sna_sda$albumin)), col = "purple", lwd = 2)

#ast
plot(
  pbc_sna_sda$ast,
  residuos_bien_mtg,
  xlab = "Ast",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta sda",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_bien_mtg ~ pbc_sna_sda$ast),
      col = "red",
      lwd = 2)
lines(pbc_sna_sda$ast, fitted(lm(residuos_bien_mtg ~ pbc_sna_sda$ast)), col = "purple", lwd = 2)

#copper
plot(
  pbc_sna_sda$copper,
  residuos_bien_mtg,
  xlab = "Cobre en orina",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta sda",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_bien_mtg ~ pbc_sna_sda$copper),
      col = "red",
      lwd = 2)
lines(pbc_sna_sda$copper, fitted(lm(residuos_bien_mtg ~ pbc_sna_sda$copper)), col = "purple", lwd = 2)

#platelet
plot(
  pbc_sna_sda$platelet,
  residuos_bien_mtg,
  xlab = "Plaquetas en la sangre",
  ylab = "Residuos Martingala",
  main = "Modelo propuesta sda",
  pch = 19,
  cex = 0.5,
  col = "orange"
)
lines(smooth.spline(residuos_bien_mtg ~ pbc_sna_sda$platelet),
      col = "red",
      lwd = 2)
lines(pbc_sna_sda$platelet, fitted(lm(residuos_bien_mtg ~ pbc_sna_sda$platelet)), col = "purple", lwd = 2)

# Reisgos proporcionales                ----
#coxph
par(mfrow = c(1,2))
aux <-
  survfit(coxph(Surv((status_bien - residuos_bien_mtg), status_bien
  ) ~ 1), type = 'kaplan-meier')
plot(
  aux$time,
  -log(aux$surv),
  type = 's',
  xlab = 'Residuos de Cox-Snell',
  ylab = 'Residuos acumulados',
  main = 'Riesgos proporcionales, modelo sda',
  col = '#AF130B',
  lwd = 1.8
)
abline(0, 1, col = '#ED4607', lwd = 2)


aux1 <-
  survfit(coxph(Surv((
    status1 - resid(modelo_0, type = 'martingale')
  ), status1) ~ 1), type = 'kaplan-meier')
plot(
  aux1$time,
  -log(aux1$surv),
  type = 's',
  xlab = 'Residuos de Cox-Snell',
  ylab = 'Residuos acumulados',
  main = 'Reisgos proporcionales, modelo base',
  col = 'blue',
  lwd = 1.8
)
abline(0, 1, col = 'purple', lwd = 2)

#Cox.zph
par(mfrow = c(2,2))

# Numéricas
plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$age",
  main = "Edad",
  col = c("orange", "green", "green"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "red", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$albumin",
  main = "Albumina",
  col = c("orange", "green", "green"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "red", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$ast",
  main = "Aspartato aminotransferasa",
  col = c("orange", "green", "green"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "red", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$copper",
  main = "Cobre en orina",
  col = c("orange", "green", "green"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "red", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$platelet",
  main = "Plaquetas en sangre",
  col = c("orange", "green", "green"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "red", lwd = 1.8)



# Categóricas

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$ascites",
  main = "Líquido en la cavidad abdominal",
  col = c("purple", "violet", "violet"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "blue", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$edema",
  main = "Edema",
  col = c("purple", "violet", "violet"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "blue", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$hepato",
  main = "Alargamiento del hígado",
  col = c("purple", "violet", "violet"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "blue", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$spiders",
  main = "Várices",
  col = c("purple", "violet", "violet"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "blue", lwd = 1.8)

plot(
  cox.zph(modelo_bien),
  var = "pbc_sna_sda$stage",
  main = "Estado de bienestar",
  col = c("purple", "violet", "violet"),
  lwd = 2,
  ylab = ""
)
abline(h = 0, col = "blue", lwd = 1.8)

par(mfrow = c(1,1))
