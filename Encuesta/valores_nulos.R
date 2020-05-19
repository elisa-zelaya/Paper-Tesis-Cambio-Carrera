setwd("/")
setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_jornada.csv",sep=",", header=T,na.strings = c("","NA")) 
#NA treatment
summary(survey)
na.summary<- c()


for (myname in names(survey)){
 

  s<-as.data.frame( prop.table(table(is.na(survey[,myname]))))#verifica que valores son nulos

  operacion <-s %>% filter(Var1==TRUE) %>% select(Freq)
  df_temp<-data.frame(column.name=c(myname), na.percentage=ifelse(length(operacion$Freq)==0,0,operacion$Freq[1]))
  na.summary<-rbind(na.summary,df_temp)
}


na.summary %>% arrange(-na.percentage) %>% filter(na.percentage >0)


#survey$veces._cambio_carrera reemplazar por ceros, por que son estudiantes que no han hecho un cambio de carrera

#1-survey[is.na(survey$veces._cambio_carrera),"veces._cambio_carrera "] <- 0 #nofunca

survey$veces._cambio_carrera[is.na(survey$veces._cambio_carrera)]<-0
survey$factores_no_cambio

names(survey)
summary(survey)

#ya validado para estas columnas que sean estudiantes que no han hecho un test
#reemplazar los NA de momento test vocacional por "No aplica" por que son preguntas con dependecia de respuestas
survey$momento_test_vocacional<- as.character(survey$momento_test_vocacional)

survey$momento_test_vocacional[is.na(survey$momento_test_vocacional)] <- "No aplica"

survey$momento_test_vocacional<- as.factor(survey$momento_test_vocacional)
#reemplazar los NA de resultados test vocacional por "No aplica" por que son preguntas con dependecia de respuestas
survey$resultados_congruentes_test<- as.character(survey$resultados_congruentes_test)

survey$resultados_congruentes_test[is.na(survey$resultados_congruentes_test)] <- "Si"

survey$resultados_congruentes_test<- as.factor(survey$resultados_congruentes_test)

# reemplazar por no contesto al campo del lugar de test por "no contesto al unico registro que no contesto esa pregunta pese a que si hizo el test previo"
survey <- survey %>%
  mutate(lugar_test_vocacional = (ifelse((is.na(survey$lugar_test_vocacional) 
                                                & survey$previo_test_vocacional == 'Si'), 'No Contestó', lugar_test_vocacional)))

survey$lugar_test_vocacional<- as.factor(survey$lugar_test_vocacional)
summary(survey)
x<-survey %>% filter(is.na(lugar_test_vocacional), survey$previo_test_vocacional=='Si')
survey$lugar_test_vocacional<-as.character(survey$lugar_test_vocacional)
survey$lugar_test_vocacional[survey$Timestamp==x$Timestamp]<-"No contestó"
survey$lugar_test_vocacional<-as.factor(survey$lugar_test_vocacional)

survey %>% filter(lugar_test_vocacional=='No contesto')
summary(survey)

survey$lugar_test_vocacional

#reemplazar por "no aplica" el campo de lugar de test por que son preguntas con dependencia de respuestas
survey$lugar_test_vocacional<- as.character(survey$lugar_test_vocacional)

survey$lugar_test_vocacional[is.na(survey$lugar_test_vocacional)] <- "No aplica"

survey$lugar_test_vocacional<- as.factor(survey$lugar_test_vocacional)

#cambiar factor no cambio por no aplica por que son personas que no han hecho un cambio pero que si han pesado cambiarse
survey %>% filter(is.na(survey$factores_no_cambio)  & (survey$pensado_cambio_carrera == 'No'))

#cambiar los NA de pensado en cambio de carrerra por no aplica por que son personas que si han hecho un cambio de Carrera
survey$pensado_cambio_carrera<- as.character(survey$pensado_cambio_carrera)
survey$pensado_cambio_carrera[is.na(survey$pensado_cambio_carrera)] <- "No aplica"
survey$pensado_cambio_carrera<-as.factor((survey$pensado_cambio_carrera))
summary(survey)

#cambiar a no contesto a personas que si han pensado en hacer cambio de carrera y no contesto los factores
survey <- survey %>%
  mutate(factores_no_cambio = as.factor(ifelse((is.na(survey$factores_no_cambio) 
                                                & (survey$pensado_cambio_carrera == 'Sí' | survey$pensado_cambio_carrera =='Tal vez')), 'No Contestó', as.character(factores_no_cambio))))
#cambio a no Aplica a ersonas que contestaron que no han realizado cambio de carrera y no han pensado en realizar cambio de carrera 
survey <- survey %>%
  mutate(factores_no_cambio = as.factor(ifelse((is.na(survey$factores_no_cambio)  & (survey$pensado_cambio_carrera == 'No')), 'No aplica', as.character(factores_no_cambio))))


      #survey %>% filter(is.na(factores_no_cambio),(survey$pensado_cambio_carrera=='Sí' | survey$pensado_cambio_carrera=='Tal vez'))
#cambiar a no aplica en factor cambio a personas que no han realizado cambio de carrera
survey$factores_cambio_carrera<- as.character(survey$factores_cambio_carrera)
survey$factores_cambio_carrera[is.na(survey$factores_cambio_carrera)] <- "No aplica"
survey$factores_cambio_carrera<-as.factor(survey$factores_cambio_carrera)
summary(survey)


#cambiar a no contesto multiple carrera
survey$multiple_carrera<- as.character(survey$multiple_carrera)
survey$multiple_carrera[is.na(survey$multiple_carrera)] <- "No"
survey$multiple_carrera<-as.factor(survey$multiple_carrera)

survey %>% filter(is.na(multiple_carrera))

#cambiar a no contesto anio reprobacion
survey$año_reprobacion<- as.character(survey$año_reprobacion)
survey$año_reprobacion[is.na(survey$año_reprobacion)] <- "No aplica"
survey$año_reprobacion<-as.factor(survey$año_reprobacion)

#cambiar a no contesto lugar de residencia
survey$lugar_residencia<- as.character(survey$lugar_residencia)
survey$lugar_residencia[is.na(survey$lugar_residencia)] <- "No contestó"
survey$lugar_residencia<-as.factor(survey$año_reprobacion)

#cambiar a no contesto indice
survey$indice_academico<- as.character(survey$indice_academico)
survey$indice_academico[is.na(survey$indice_academico)] <- "No contestó"
survey$indice_academico<-as.factor(survey$indice_academico)

#cambiar a no contesto Reprobado mas de 2
survey <- survey %>%
  mutate(reprobado_mas_de_dos = as.factor(ifelse((is.na(survey$reprobado_mas_de_dos) 
                                                & (survey$reprobado == 'Si' )), 'No Contestó', as.character(reprobado_mas_de_dos))))
summary(survey)
survey$nombre_opcion_carrera


#cambiar a no aplica Reprobao mas de 2 por que son personas que no han reprobado una clase

survey$reprobado_mas_de_dos<- as.character(survey$reprobado_mas_de_dos)
survey$reprobado_mas_de_dos[is.na(survey$reprobado_mas_de_dos)] <- "Si"
survey$reprobado_mas_de_dos<-as.factor(survey$reprobado_mas_de_dos)

#cambiar a no contesto juegos de estrategias por que no respondio

survey$juegos_estrategia<- as.character(survey$juegos_estrategia)
survey$juegos_estrategia[is.na(survey$juegos_estrategia)] <- "No contestó"
survey$juegos_estrategia<-as.factor(survey$juegos_estrategia)

#cambiar a no contesto operaciones matematicas por que no respondio

survey$operaciones_matematicas<- as.character(survey$operaciones_matematicas)
survey$operaciones_matematicas[is.na(survey$operaciones_matematicas)] <- "No contestó"
survey$operaciones_matematicas<-as.factor(survey$operaciones_matematicas)

#cambiar a no contesto resolucion_puzzles por que no respondio

survey$resolucion_puzzles<- as.character(survey$resolucion_puzzles)
survey$resolucion_puzzles[is.na(survey$resolucion_puzzles)] <- "No contestó"
survey$resolucion_puzzles<-as.factor(survey$resolucion_puzzles)

#cambiar a no contesto calculo mental por que no respondio

survey$calculo_mental<- as.character(survey$calculo_mental)
survey$calculo_mental[is.na(survey$calculo_mental)] <- "No contestó"
survey$calculo_mental<-as.factor(survey$calculo_mental)

#cambiar a no contesto creatividad por que no respondio

survey$creatividad<- as.character(survey$creatividad)
survey$creatividad[is.na(survey$creatividad)] <- "No contestó"
survey$creatividad<-as.factor(survey$creatividad)


#camabiar a no contesto nombre opcion carrera
survey$nombre_opcion_carrera<- as.character(survey$nombre_opcion_carrera)
survey$nombre_opcion_carrera[is.na(survey$nombre_opcion_carrera)] <- "No contestó"
survey$nombre_opcion_carrera<-as.factor(survey$nombre_opcion_carrera)


survey$multiple_carrera
#######################################################

#----------------------------NO-------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#valores atipicos factores cambio carrera

summary(survey$factores_cambio_carrera)


df_perc6<-as.data.frame(prop.table(table(survey$factores_cambio_carrera)))

df_perc6<-df_perc6 %>% arrange(-Freq) 


boxplot(df_perc6$Freq)


hist(df_perc6$Freq)

qqnorm(df_perc6$Freq)

#eliminar los No aplica
survey<-survey[!is.na(survey$factores_cambio_carrera),]

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

summary(survey$lugar_origen)


df_perc8<-as.data.frame(prop.table(table(survey$lugar_origen)))

df_perc8<-df_perc8 %>% arrange(-Freq) 

median((df_perc8$Freq))




boxplot(df_perc8$Freq)


hist(df_perc8$Freq)

qqnorm(df_perc8$Freq)

df_perc8[df_perc8$Var1 %in% c("Ocotepeque","Copán","Lempira"),"regiones"]<-"Occidental"
df_perc8[df_perc8$Var1 %in% c("Cortes","Yoro","Santa Bárbara"),"regiones"]<-"Noroccidental"
df_perc8[df_perc8$Var1 %in% c("Atlántida","Gracias a Dios","Colón","Islas de la Bahía"),"regiones"]<-"Nororiental"
df_perc8[df_perc8$Var1 %in% c("Intibucá","Comayagua","La Paz"),"regiones"]<-"Centro Occidental"
df_perc8[df_perc8$Var1 %in% c("Francisco Morazán","El Paraíso","Olancho"),"regiones"]<-"Centro Oriental"
df_perc8[df_perc8$Var1 %in% c("Choluteca","Valle"),"regiones"]<-"Sur"

df_perc8<-df_perc8 %>% select(Var1,regiones)
survey<-left_join(survey,df_perc8,by=c("lugar_origen"="Var1"))
survey<-survey[,!(names(survey) %in% c("lugar_origen"))]
names(survey)[length(names(survey))]<-"lugar_origen"


prop.table(table(survey$lugar_origen))

survey$lugar_origen<-as.factor(survey$lugar_origen)
summary(survey$lugar_origen)


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


df_perc9<-df_perc9 %>% select(Var1,lugar_test)
survey<-left_join(survey,df_perc9,by=c("lugar_test_vocacional"="Var1"))
survey<-survey[,!(names(survey) %in% c("lugar_test_vocacional"))]
names(survey)[length(names(survey))]<-"lugar_test_vocacional"


prop.table(table(survey$lugar_test_vocacional))

survey$lugar_test_vocacional<-as.factor(survey$lugar_test_vocacional)
summary(survey$lugar_test_vocacional)





