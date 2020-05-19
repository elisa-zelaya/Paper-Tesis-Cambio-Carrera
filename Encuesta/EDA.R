
setwd("/")
setwd("Users/user/Documents/Encuesta/")

survey<-read.csv("Encuesta.csv",header=T,sep=",", encoding="UTF-8")

column_names<- read.csv("column_names_tratado.csv",header=T,sep=",")
names(survey) %in% c("X..La.carrera.que.querias.estudiar.esta.dentro.de.la.oferta.académica.de.UNAH.")

new.column<-survey[,!(names(survey) %in% c("X..La.carrera.que.querias.estudiar.esta.dentro.de.la.oferta.académica.de.UNAH."))]
names(new.column)
new.column2<-new.column[,!names(new.column) %in% c("X")]
names(new.column2)
summary(survey)
survey<-new.column2
##hacer transformaciones antes de
column_names$Translation<-as.character((column_names$Translation))
names(survey)<-column_names$Translation

head(survey)

survey<-survey[,!(names(survey) %in% c("X..La.carrera.que.querias.estudiar.esta.dentro.de.la.oferta.académica.de.UNAH."))]

names(survey)

write.csv(survey,"encuesta_limpia.csv",row.names=FALSE)

column_names
names(survey) %in% c("Trabajas")



survey[,names(survey)]

 

mtcars$hp
str(mtcars)
summary(mtcars)

 sort(mtcars$cyl)
mtcars$cyl<- as.factor(mtcars$cyl)

!names(column_names) %in% c("x")
column_names$translation==""
column_names<-column_names[!(column_names$translation==""),!(names(column_names) %in% c("x"))]

column_names$translation<-as.character(column_names$translation)
names(survey)<-column_names$translation

head(survey)

summary(survey)
names(survey)

write.csv(survey,"column_names_tratado2.csv",row.names=FALSE)

