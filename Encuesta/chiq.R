
library(dplyr)
library(ggplot2)
setwd("/")

setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_transformada-D.csv",sep=",", header=T,na.strings = c("","NA"))

names(survey)
prop.table(table(survey$trabaja,survey$indice_academico),1)

ggplot(survey) +
  aes(x=indice_academico,fill=factor(trabaja))+
  geom_bar(position="fill")+
  theme(axis.text.x=element_text(angle=45))

ggplot(survey) +
  aes(x=indice_academico,fill=factor(trabaja))+
  geom_bar(position="stack")+
  theme(axis.text.x=element_text(angle=45))
chisq.test(table(survey$indice_academico))


#hipotesis nula
#H_0: las categorias de indice y trabaja son independientes
#H_A: Las categorias son dendientes
#termino correcto: rechazar o aceptar hipotesis nula
#aceptamos nuesta hipotesis nula cuando el p-value de nuestra prueba chisq.test esmenos 0.05
 #en este caso el p-value es menor asi que se acepta la hipotesis nula y se rechaza la dependencia

#conclusion: segun nuestro p-value aceptamos nuestra hipotesis nula por lo tanto las variables no son dependeientes
#normalidad
#H0:nuestra distribucion es normal
#H1: nuestra distribucion no es normal

#conclusion como el p-value es mayor a 0.05 no podemos rechazar la hipotesis nula video 11:22 por tanto es normal


table(survey$pensado_cambio_carrera)
table(survey$motivacion)
qqnorm(survey$motivacion)
qqline(survey$motivacion)

survey[survey$motivacion<10,"motivacion"]<-median(survey$motivacion)
summary(survey$motivacion)
boxplot(survey$motivacion)
shapiro.test(survey$motivacion)

#prueba de homocedasticidad : que tan normal es la varianza de la informacion
#var.test(df$variable,df$variable)
#interpretacion: #con un p-value=0.1177, mayor de 0.05 no podemos rechazar la hipotesisnula por tanto suponemos homogeneidad de las varianzas

#libreria corrplot
 
