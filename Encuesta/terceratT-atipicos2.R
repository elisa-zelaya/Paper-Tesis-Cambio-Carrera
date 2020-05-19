##install.packages("dplyr")

library(dplyr)
setwd("/")

setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_transformada-B.csv",sep=",", header=T,na.strings = c("","NA"))




names(survey) 
summary(survey)

#valores atipicos factores cambio carrera

summary(survey$factores_cambio_carrera)


df_perc6<-as.data.frame(prop.table(table(survey$factores_cambio_carrera)))

df_perc6<-df_perc6 %>% arrange(-Freq) 


boxplot(survey$factores_cambio_carrera)


hist(df_perc6$Freq)

qqnorm(df_perc6$Freq)

#eliminar los No aplica
#survey<-survey[!is.na(survey$factores_cambio_carrera),]

df_perc6[df_perc6$Var1 %in% c("Cambio de intereses personales",
                              "Cambio de intereses personales;No era lo que esperaba"),
         "razones_cambio"]<-"Cambio de intereses personales"

df_perc6[df_perc6$Var1 %in% c("Factor económico",
                              "Factor económico;No era lo que esperaba",
                              "Cambio de intereses personales;Factor económico"),
         "razones_cambio"]<-"Factor económico"

df_perc6[df_perc6$Var1 %in% c("Falta de puntos para la carrera a la cual aplico en la PAA",
                              "Cambio de intereses personales;Falta de puntos para la carrera a la cual aplico en la PAA",
                              "Cambio de intereses personales;No era lo que esperaba;Falta de puntos para la carrera a la cual aplico en la PAA",
                              "Factor económico;Falta de puntos para la carrera a la cual aplico en la PAA"),
         "razones_cambio"]<-"Falta de puntos en la PAA"


df_perc6[df_perc6$Var1 %in% c("Influencia familiar",
                              "Influencia familiar;Cambio de intereses personales;Factor económico;No era lo que esperaba",
                              "Influencia familiar;Cambio de intereses personales;No era lo que esperaba",
                              "Influencia familiar;Falta de puntos para la carrera a la cual aplico en la PAA",
                              "Influencia familiar;No era lo que esperaba",
                              "Influencia familiar;No era lo que esperaba;Falta de puntos para la carrera a la cual aplico en la PAA"),
         "razones_cambio"]<-"Influencia familiar"

df_perc6[df_perc6$Var1 %in% c("No era lo que esperaba",
                              "No era lo que esperaba;Falta de puntos para la carrera a la cual aplico en la PAA",
                              "No era lo que esperaba;Rendimiento académico",
                              "Rendimiento académico",
                              "Cambio de intereses personales;Factor económico;No era lo que esperaba;Rendimiento académico",
                              "Cambio de intereses personales;No era lo que esperaba;Rendimiento académico",
                              "Cambio de intereses personales;Factor económico;No era lo que esperaba"),
         "razones_cambio"]<-"No era lo que esperaba"
df_perc6[df_perc6$Var1 %in% c("No aplica"),
         "razones_cambio"]<-"No aplica"

df_perc6<-df_perc6 %>% select(Var1,razones_cambio)
survey<-left_join(survey,df_perc6,by=c("factores_cambio_carrera"="Var1"))
survey<-survey[,!(names(survey) %in% c("factores_cambio_carrera"))]
names(survey)[length(names(survey))]<-"factores_cambio_carrera"


prop.table(table(survey$factores_cambio_carrera))

survey$factores_cambio_carrera<-as.factor(survey$factores_cambio_carrera)
summary(survey$factores_cambio_carrera)

#valores atipicos factores multiple carrera

summary(survey$multiple_carrera)

survey <- survey %>%
  mutate(multiple_carrera= as.factor((ifelse(is.na(survey$multiple_carrera),  '', as.character(multiple_carrera)))))



df_perc7<-as.data.frame(prop.table(table(survey$multiple_carrera)))

df_perc7<-df_perc7 %>% arrange(-Freq) 




boxplot(df_perc7$Freq)


hist(df_perc7$Freq)

qqnorm(df_perc7$Freq)



df_perc7[df_perc7$Var1 %in% c("Diplomado de Inglés","Técnico en reded","Si","Sí, Letras","Diplomado de Inglés ", "Ingeniería de Sonido en línea ","Matemática","Matematicas"),"multiple_carrera2"]<-"Si"
df_perc7[df_perc7$Var1 %in% c("","No ","Ninguna","no","No 	","No.","NO","No","Solo una"),"multiple_carrera2"]<-"No"




df_perc7<-df_perc7 %>% select(Var1,multiple_carrera2)
survey<-left_join(survey,df_perc7,by=c("multiple_carrera"="Var1"))
survey<-survey[,!(names(survey) %in% c("multiple_carrera"))]
names(survey)[length(names(survey))]<-"multiple_carrera"


prop.table(table(survey$factores_cambio_carrera))

survey$multiple_carrera<-as.factor(survey$multiple_carrera)
summary(survey$multiple_carrera)
#--------------------------------------------------------------------------------------------
#valor atipico lugar de origen

df_perc6<-as.data.frame(prop.table(table(survey$lugar_origen)))

df_perc6<-df_perc6 %>% arrange(-Freq) 


boxplot(df_perc6$Freq)


hist(df_perc6$Freq)

qqnorm(df_perc6$Freq)

#eliminar los No aplica
#survey<-survey[!is.na(survey$factores_cambio_carrera),]

df_perc6[df_perc6$Var1 %in% c("Francisco Morazán"),
         "foraneo"]<-"Francisco Morazán"

df_perc6[df_perc6$Var1 %in% c("El Paraíso",
                              "Comayagua",
                              "Olancho",
                              "Valle",
                              "Atlántida",
                              "Choluteca",
                              "Cortes",
                              "Intibucá",
                              "Ocotepeque",
                              "Yoro",
                              "La Paz",
                              "Colón",
                              "Copán",
                              "Lempira",
                              "Santa Bárbara"),
         "foraneo"]<-"Fuera de Francisco Morazán"



df_perc6<-df_perc6 %>% select(Var1,foraneo)
survey<-left_join(survey,df_perc6,by=c("lugar_origen"="Var1"))
survey<-survey[,!(names(survey) %in% c("lugar_origen"))]
names(survey)[length(names(survey))]<-"lugar_origen"


prop.table(table(survey$lugar_origen))

survey$lugar_origen<-as.factor(survey$lugar_origen)

#--------------------------------------------------------------------------------------------
#valor atipico lugar test

summary(survey$lugar_test_vocacional)

#eliminar los No aplica
#survey<-survey[!is.na(survey$lugar_test_vocacional),]

df_perc9<-as.data.frame(prop.table(table(survey$lugar_test_vocacional)))

df_perc9<-df_perc9 %>% arrange(-Freq) 

boxplot(df_perc9$Freq)


hist(df_perc9$Freq)

qqnorm(df_perc9$Freq)

df_perc9[df_perc9$Var1 %in% c("Colegio",
                              "Colegio;Departamento privado de psicología;Internet"),
         "lugar_test"]<-"Colegio"
df_perc9[df_perc9$Var1 %in% c("Departamento privado de psicología",    
                              "Departamento privado de psicología;Otro",
                              "Otro"),
         "lugar_test"]<-"Otro"
df_perc9[df_perc9$Var1 %in% c("Internet",                                    
                              "Internet;Otro",
                              "UNAH;Internet",
                              "Colegio;Internet",                      
                              "Colegio;Internet;Otro"),
         "lugar_test"]<-"Internet"
df_perc9[df_perc9$Var1 %in% c(
  "UNAH;Colegio;Otro",
  "UNAH;Departamento privado de psicología",
  "UNAH;Departamento privado de psicología;Internet",
  "UNAH;Otro"
),"lugar_test"]<-"UNAH/Otro"
df_perc9[df_perc9$Var1 %in% c("UNAH",
                              "UNAH;Colegio",                            
                              "UNAH;Colegio;Internet"
),"lugar_test"]<-"UNAH"
df_perc9[df_perc9$Var1 %in% c("No aplica"
),"lugar_test"]<-"No aplica"
df_perc9[df_perc9$Var1 %in% c("No contestó"
),"lugar_test"]<-"No contestó"

df_perc9<-df_perc9 %>% select(Var1,lugar_test)
survey<-left_join(survey,df_perc9,by=c("lugar_test_vocacional"="Var1"))
survey<-survey[,!(names(survey) %in% c("lugar_test_vocacional"))]
names(survey)[length(names(survey))]<-"lugar_test_vocacional"


prop.table(table(survey$lugar_test_vocacional))

survey$lugar_test_vocacional<-as.factor(survey$lugar_test_vocacional)
summary(survey$lugar_test_vocacional)




#write
write.csv(survey,"encuesta_transformada-D.csv", row.names = FALSE)

summary(survey)