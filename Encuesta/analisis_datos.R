##install.packages("dplyr")

library(dplyr)
setwd("/")
setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_limpia.csv",sep=",", header=T)


summary(survey)
names(survey)
df_perc<-as.data.frame(prop.table(table(survey$asesoria_universitaria)))
##ordena los dato de Freq con (-) lo ordena descenente
df_perc<-df_perc %>% arrange(-Freq) 

#identificar valores atipicos
boxplot(df_perc$Freq)

hist(df_perc$Freq)

qqnorm(df_perc$Freq)

df_perc[df_perc$Var1 %in% c("Ferias Vocacionales", "Ferias Vocacionales;Curso de induccion a la vida universitaria","Curso de induccion a la vida universitaria"),"asesoria_unah"]<-"Curso de induccion y ferias"
df_perc[df_perc$Var1 %in% c("Orientacion  VOAE", "Ferias Vocacionales;Orientacion  VOAE","Curso de induccion a la vida universitaria;Orientacion  VOAE","Ferias Vocacionales;Curso de induccion a la vida universitaria;Orientacion  VOAE"),"asesoria_unah"]<-"VOAE"
df_perc[df_perc$Var1 %in% c("Ferias Vocacionales;Ninguna","Ninguna"),"asesoria_unah"]<-"Ninguna"

df_perc[df_perc$Var1 %in% c("Mañana;Tarde;Noche", "Mañana;Noche"),"categoria"]<-"Jornada completa"
df_perc[df_perc$Var1 %in% c("Tarde", "Noche","Mañana"),"categoria"]<-"Unica Jornada"
df_perc[df_perc$Var1 %in% c("Tarde;Noche","Mañana;Tarde"),"categoria"]<-"Doble Jornada"

df_perc<-df_perc %>% select(Var1,asesoria_unah)
df_perc<-df_perc %>% select(Var1,categoria)

survey<-left_join(survey,df_perc,by=c("asesoria_universitaria"="Var1"))
survey<-left_join(survey,df_perc,by=c("jornada"="Var1"))


names(survey$asesoria_unah)
survey<-survey[,!(names(survey) %in% c("asesoria_universitaria"))]
survey<-survey[,!(names(survey) %in% c("jornada"))]

names(survey)[length(names(survey))]<-"asesoria_universitaria"
table(survey$asesoria_universitaria)
prop.table(table(survey$jornada))
df_perc2<-as.data.frame(prop.table(table(survey$asesoria_universitaria)))
boxplot(df_perc2$Freq)

hist(df_perc2$Freq)

qqnorm(df_perc2$Freq)


write.csv(survey,"encuesta_jornada.csv", row.names = FALSE)

summary(survey)
