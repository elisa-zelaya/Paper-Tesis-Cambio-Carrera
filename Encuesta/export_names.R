setwd("/")
setwd("Users/user/Documents/Encuesta/")

survey<-read.csv("Encuesta.csv",header=T,sep=",", encoding="UTF-8")

names(survey) %in% c("X..La.carrera.que.querias.estudiar.esta.dentro.de.la.oferta.académica.de.UNAH.")

names(survey)
my.names<-names(survey)

columnas_a_tratar<-my.names[!(my.names %in% c("X..La.carrera.que.querias.estudiar.esta.dentro.de.la.oferta.académica.de.UNAH."))]

#la forma correcta de hacer dble filtro
columnas_a_tratar<-my.names[!(my.names %in% c("X..La.carrera.que.querias.estudiar.esta.dentro.de.la.oferta.académica.de.UNAH.")),!(my.names %in% c("X"))]
#####

columnas_a_tratar
my.names2<-columnas_a_tratar
my.names2

columnas_a_tratar2<-my.names2[!(my.names2 %in% c("X"))]

columnas_a_tratar2

df<-data.frame(columna.name=columnas_a_tratar2)

write.csv(df,"column_names.csv", row.names = FALSE)
