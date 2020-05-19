##install.packages("dplyr")

library(dplyr)
setwd("/")

setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_limpia.csv",sep=",", header=T)

na.summary<- c()


for (myname in names(survey)){
  
  
  s<-as.data.frame( prop.table(table(is.na(survey[,myname]))))#verifica que valores son nulos
  
  operacion <-s %>% filter(Var1==TRUE) %>% select(Freq)
  df_temp<-data.frame(column.name=c(myname), na.percentage=ifelse(length(operacion$Freq)==0,0,operacion$Freq[1]))
  na.summary<-rbind(na.summary,df_temp)
}


na.summary %>% arrange(-na.percentage) %>% filter(na.percentage >0)

names(survey) 
summary(survey)
#######3

summary(survey$factores_cambio_carrera)



summary(survey$lugar_origen)
#-------------------------------NULOS---------------------------------------
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
                                          & survey$previo_test_vocacional == 'Si'), 'No contestó', as.character(lugar_test_vocacional))))

survey$lugar_test_vocacional<- as.factor(survey$lugar_test_vocacional)
summary(survey)
x<-survey %>% filter(is.na(lugar_test_vocacional), survey$previo_test_vocacional=='Si')
survey$lugar_test_vocacional<-as.character(survey$lugar_test_vocacional)
survey$lugar_test_vocacional[survey$Timestamp==x$Timestamp]<-"No contestó"
survey$lugar_test_vocacional<-as.factor(survey$lugar_test_vocacional)

survey %>% filter(lugar_test_vocacional=='No contestó')
summary(survey)

summary(survey$lugar_test_vocacional)

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


#survey %>% filter(is.na(factores_no_cambio),(survey$pensado_cambio_carrera=='Sí' | survey$pensado_cambio_carrera=='Tal vez'))
#cambiar a no aplica en factor cambio a personas que no han realizado cambio de carrera
survey$factores_cambio_carrera<- as.character(survey$factores_cambio_carrera)
survey$factores_cambio_carrera[is.na(survey$factores_cambio_carrera)] <- "No aplica"
survey$factores_cambio_carrera<-as.factor(survey$factores_cambio_carrera)



#survey$veces._cambio_carrera reemplazar por ceros, por que son estudiantes que no han hecho un cambio de carrera

survey$veces._cambio_carrera[is.na(survey$veces._cambio_carrera)]<-0

#cambiar a no contesto anio reprobacion
survey$año_reprobacion<- as.character(survey$año_reprobacion)
survey$año_reprobacion[is.na(survey$año_reprobacion)] <- "No aplica"
survey$año_reprobacion<-as.factor(survey$año_reprobacion)

#cambiar los NA de pensado en cambio de carrerra por no aplica por que son personas que si han hecho un cambio de Carrera
survey$pensado_cambio_carrera<- as.character(survey$pensado_cambio_carrera)
survey$pensado_cambio_carrera[is.na(survey$pensado_cambio_carrera)] <- "No aplica"
survey$pensado_cambio_carrera<-as.factor((survey$pensado_cambio_carrera))

#cambiar a si Reprobao mas de 2 por que son NA

survey$reprobado_mas_de_dos<- as.character(survey$reprobado_mas_de_dos)
survey$reprobado_mas_de_dos[is.na(survey$reprobado_mas_de_dos)] <- "Si"
survey$reprobado_mas_de_dos<-as.factor(survey$reprobado_mas_de_dos)

#reemplazar los NA de resultados test vocacional por "Si" por que no YES?No
survey$resultados_congruentes_test<- as.character(survey$resultados_congruentes_test)

survey$resultados_congruentes_test[is.na(survey$resultados_congruentes_test)] <- "Si"


survey$resultados_congruentes_test<- as.factor(survey$resultados_congruentes_test)

#cambiar a no contesto lugar de residencia
survey$lugar_residencia<- as.character(survey$lugar_residencia)
survey$lugar_residencia[is.na(survey$lugar_residencia)] <- "No contestó"
survey$lugar_residencia<-as.factor(survey$lugar_residencia)

#cambiar a no contesto indice
survey$indice_academico<- as.character(survey$indice_academico)
survey$indice_academico[is.na(survey$indice_academico)] <- "No contestó"
survey$indice_academico<-as.factor(survey$indice_academico)

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

summary(survey)
#camabiar a no contesto nombre opcion carrera
survey$nombre_opcion_carrera<- as.character(survey$nombre_opcion_carrera)
survey$nombre_opcion_carrera[is.na(survey$nombre_opcion_carrera)] <- "No contestó"
survey$nombre_opcion_carrera<-as.factor(survey$nombre_opcion_carrera)

#cambiar a no contesto a personas que si han pensado en hacer cambio de carrera y no contesto los factores
survey <- survey %>%
  mutate(factores_no_cambio = as.factor(ifelse((is.na(survey$factores_no_cambio) 
                                                & (survey$pensado_cambio_carrera == 'Sí' | survey$pensado_cambio_carrera =='Tal vez')), 'No contestó', as.character(factores_no_cambio))))
#cambio a no Aplica a ersonas que contestaron que no han realizado cambio de carrera y no han pensado en realizar cambio de carrera 
survey <- survey %>%
  mutate(factores_no_cambio = as.factor(ifelse((is.na(survey$factores_no_cambio)  & (survey$pensado_cambio_carrera == 'No')), 'No aplica', as.character(factores_no_cambio))))

#cambio a no Aplica a ersonas que contestaron que si han realizado cambio de carrera
survey <- survey %>%
  mutate(factores_no_cambio = as.factor(ifelse((is.na(survey$factores_no_cambio)  & (survey$cambio_carrera == 'Sí')), 'No aplica', as.character(factores_no_cambio))))










#write
write.csv(survey,"encuesta_transformada-A.csv", row.names = FALSE)

summary(survey)
