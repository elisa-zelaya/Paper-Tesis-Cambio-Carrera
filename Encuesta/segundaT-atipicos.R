##install.packages("dplyr")

library(dplyr)
setwd("/")

setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_transformada-A.csv",sep=",", header=T,na.strings = c("","NA"))



names(survey) 
summary(survey)



#----------------------------------ATIPICOS--------------------------------------------------
#identificar valores atipicos asesoria universitaria
summary(survey$asesoria_universitaria)
df_perc<-as.data.frame(prop.table(table(survey$asesoria_universitaria)))
##ordena los dato de Freq con (-) lo ordena descenente
df_perc<-df_perc %>% arrange(-Freq) 

boxplot(df_perc$Freq)

hist(df_perc$Freq)

qqnorm(df_perc$Freq)

df_perc[df_perc$Var1 %in% c("Ferias Vocacionales", "Ferias Vocacionales;Curso de induccion a la vida universitaria","Curso de induccion a la vida universitaria"),"asesoria_unah"]<-"Feria vocacional y Curso induccion"
df_perc[df_perc$Var1 %in% c("Orientacion  VOAE", "Ferias Vocacionales;Orientacion  VOAE","Curso de induccion a la vida universitaria;Orientacion  VOAE","Ferias Vocacionales;Curso de induccion a la vida universitaria;Orientacion  VOAE"),"asesoria_unah"]<-"Orientacion VOAE"
df_perc[df_perc$Var1 %in% c("Ferias Vocacionales;Ninguna","Ninguna"),"asesoria_unah"]<-"Ninguna"

df_perc<-df_perc %>% select(Var1,asesoria_unah)
survey<-left_join(survey,df_perc,by=c("asesoria_universitaria"="Var1"))

names(survey$asesoria_unah)
survey<-survey[,!(names(survey) %in% c("asesoria_universitaria"))]
names(survey)[length(names(survey))]<-"asesoria_universitaria"
table(survey$asesoria_universitaria)

survey$asesoria_universitaria<-as.factor(survey$asesoria_universitaria)
summary(survey$asesoria_universitaria)
#--------------------------------------------------------------------------------------
#valores atipicos Jornada
df_perc2<-as.data.frame(prop.table(table(survey$jornada)))

boxplot(df_perc2$Freq)

hist(df_perc2$Freq)

qqnorm(df_perc2$Freq)

df_perc2[df_perc2$Var1 %in% c("Mañana;Tarde;Noche", "Mañana;Noche"),"categoria"]<-"Jornada completa"
df_perc2[df_perc2$Var1 %in% c("Tarde", "Noche","Mañana"),"categoria"]<-"Unica Jornada"
df_perc2[df_perc2$Var1 %in% c("Tarde;Noche","Mañana;Tarde"),"categoria"]<-"Doble Jornada"


df_perc2<-df_perc2 %>% select(Var1,categoria)
survey<-left_join(survey,df_perc2,by=c("jornada"="Var1"))
survey<-survey[,!(names(survey) %in% c("jornada"))]
names(survey)[length(names(survey))]<-"jornada"


prop.table(table(survey$jornada))

survey$jornada<-as.factor(survey$jornada)
summary(survey)
#------------------------------------------------------------------------------------------
#valores atipicos razones eleccion carrera


survey %>% filter(is.na(razones_eleccion_carrera))
summary(survey$razones_eleccion_carrera)

df_perc3<-as.data.frame(prop.table(table(survey$razones_eleccion_carrera)))
##ordena los dato de Freq con (-) lo ordena descenente
df_perc3<-df_perc3 %>% arrange(-Freq) 

boxplot(df_perc3$Freq)


hist(df_perc3$Freq)

qqnorm(df_perc3$Freq)

df_perc3[df_perc3$Var1 %in% c("Interés general en el area de conocimiento",
                              "Interés general en el area de conocimiento;Por conocer profesionales que ya trabajan en el area",
                              "Interés general en el area de conocimiento;Demanda laboral;Beneficios económicos;Por conocer profesionales que ya trabajan en el area",
                              "Interés general en el area de conocimiento;Beneficios económicos;Por conocer profesionales que ya trabajan en el area",
                              "Interés general en el area de conocimiento;Demanda laboral;Por conocer profesionales que ya trabajan en el area"),
         "razones_estudiar_carrera"]<-"Vocación"
df_perc3[df_perc3$Var1 %in% c("Beneficios económicos", 
                              "Demanda laboral",
                              "Demanda laboral;Beneficios económicos",
                              "Interés general en el area de conocimiento;Beneficios económicos",
                              "Interés general en el area de conocimiento;Demanda laboral;Beneficios económicos",
                              "Interés general en el area de conocimiento;Demanda laboral"),
         "razones_estudiar_carrera"]<-"Demanda laboral y Beneficio economico"

df_perc3[df_perc3$Var1 %in% c("Influencia familiar",
                              "Influencia familiar;Por conocer profesionales que ya trabajan en el area",
                              "Beneficios económicos;Influencia familiar",
                              "Demanda laboral;Beneficios económicos;Influencia familiar",
                              "Demanda laboral;Influencia familiar;Por conocer profesionales que ya trabajan en el area",
                              "Interés general en el area de conocimiento;Beneficios económicos;Influencia familiar",                                                                                                                                                                                                                                                                                                                                                                                               
                              "Interés general en el area de conocimiento;Demanda laboral;Influencia familiar;Por conocer profesionales que ya trabajan en el area",                                                                                                                                                      
                              "Interés general en el area de conocimiento;Influencia familiar",                                                                                                                                                                                     
                              "Interés general en el area de conocimiento;Demanda laboral;Influencia familiar",                                                                                      
                              "Interés general en el area de conocimiento;Demanda laboral;Beneficios económicos;Influencia familiar",                                                                                                                                                                                                                                                  
                              "Interés general en el area de conocimiento;Demanda laboral;Beneficios económicos;Influencia familiar;Por conocer profesionales que ya trabajan en el area",
                              "Demanda laboral;Beneficios económicos;Por conocer profesionales que ya trabajan en el area"),
         "razones_estudiar_carrera"]<-"Influencia Familiar/Conocidos"


df_perc3<-df_perc3 %>% select(Var1,razones_estudiar_carrera)
survey<-left_join(survey,df_perc3,by=c("razones_eleccion_carrera"="Var1"))
survey<-survey[,!(names(survey) %in% c("razones_eleccion_carrera"))]
names(survey)[length(names(survey))]<-"razones_eleccion_carrera"


prop.table(table(survey$razones_eleccion_carrera))

survey$razones_eleccion_carrera<-as.factor(survey$razones_eleccion_carrera)
summary(survey$razones_eleccion_carrera)

#----------------------------------------------------------------------------------------
#valores atipicos factores no cambio
#primero tratar los NA

summary(survey$factores_no_cambio)


survey %>% filter(is.na(factores_no_cambio) & survey$cambio_carrera == 'Sí')
survey %>% filter(is.na(factores_no_cambio) &survey$pensado_cambio_carrera == 'No')
survey %>% filter((is.na(factores_no_cambio) &(survey$pensado_cambio_carrera == 'Sí'| survey$pensado_cambio_carrera =='Tal vez')))

summary(survey$factores_no_cambio)

#survey$factores_no_cambio<-survey[!is.na(survey$factores_no_cambio),]

survey %>% filter(is.na(survey$factores_no_cambio))

df_perc5<-as.data.frame(prop.table(table(survey$factores_no_cambio)))

df_perc5<-df_perc5 %>% arrange(-Freq) 


boxplot(df_perc5$Freq)


hist(df_perc5$Freq)

qqnorm(df_perc5$Freq)

df_perc5[df_perc5$Var1 %in% c("Recursos y tiempo invertidos en la carrera",
                              "Recursos y tiempo invertidos en la carrera;Factor economico",
                              "Tu edad;Recursos y tiempo invertidos en la carrera",
                              "Tu edad;Recursos y tiempo invertidos en la carrera;Factor economico",
                              "Tu edad;Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos;Factor economico",
                              "Recursos y tiempo invertidos en la carrera;Factor economico;No cuentas con todos los requisitos para el proceso.",
                              "Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos;Factor economico",
                              "Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos;Factor economico;El sistema de cambio de carrera no te lo permite",
                              "Tu edad",
                              "Tu edad;Recursos y tiempo invertidos en la carrera;No cuentas con todos los requisitos para el proceso."),
         "razones_no_cambio"]<-"Recursos y tiempo invertidos"

df_perc5[df_perc5$Var1 %in% c("El sistema de cambio de carrera no te lo permite",
                              "El sistema de cambio de carrera no te lo permite;No cuentas con todos los requisitos para el proceso.",
                              "Opinion familiar/amigos;El sistema de cambio de carrera no te lo permite;No cuentas con todos los requisitos para el proceso.",
                              "Recursos y tiempo invertidos en la carrera;El sistema de cambio de carrera no te lo permite;No cuentas con todos los requisitos para el proceso.",
                              "No cuentas con todos los requisitos para el proceso.",
                              "Tu edad;El sistema de cambio de carrera no te lo permite;No cuentas con todos los requisitos para el proceso.",
                              "Tu edad;Recursos y tiempo invertidos en la carrera;El sistema de cambio de carrera no te lo permite;No cuentas con todos los requisitos para el proceso."),
         "razones_no_cambio"]<-"No cumple con requisitos requeridos"

df_perc5[df_perc5$Var1 %in% c("Estás realizando o vas a relizar un cambio de carrera",
                              "Estás realizando o vas a relizar un cambio de carrera;No cuentas con todos los requisitos para el proceso.",
                              "Tu edad;Recursos y tiempo invertidos en la carrera;Estás realizando o vas a relizar un cambio de carrera",
                              "Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos;Estás realizando o vas a relizar un cambio de carrera"),
         "razones_no_cambio"]<-"Actualmente en proceso de cambio"


df_perc5[df_perc5$Var1 %in% c("Opinion familiar/amigos",
                              "Opinion familiar/amigos;Factor economico",
                              "Opinion familiar/amigos;No cuentas con todos los requisitos para el proceso.",
                              "Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos",
                              "Tu edad;Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos",
                              "Recursos y tiempo invertidos en la carrera;Opinion familiar/amigos;No cuentas con todos los requisitos para el proceso."),
         "razones_no_cambio"]<-"Opinion familiar/amigos"

df_perc5[df_perc5$Var1 %in% c("No aplica"),
     "razones_no_cambio"]<-"No aplica"


df_perc5[df_perc5$Var1 %in% c("No contestó"),
    "razones_no_cambio"]<-"No contestó"

df_perc5<-df_perc5 %>% select(Var1,razones_no_cambio)
survey<-left_join(survey,df_perc5,by=c("factores_no_cambio"="Var1"))
survey<-survey[,!(names(survey) %in% c("factores_no_cambio"))]
names(survey)[length(names(survey))]<-"factores_no_cambio"


prop.table(table(survey$factores_no_cambio))

survey$factores_no_cambio<-as.factor(survey$factores_no_cambio)
summary(survey$factores_no_cambio)


#write
write.csv(survey,"encuesta_transformada-B.csv", row.names = FALSE)

summary(survey)