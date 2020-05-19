library(dplyr)
library(ggplot2)
  
setwd("/")
  
setwd("Users/user/Documents/Encuesta")
  
# Carga de datos #########
survey <- read.csv('encuesta_transformada-D.csv', sep=",")


# Resumen estadístico #########
summary(survey)

# Tipos de datos #########
sapply(survey, class)


# Variable dependiente ######

prop.table(table(survey$cambio_carrera))


# Preprocesamiento adicional #####
summary(survey)

#survey[survey$pensado_cambio_carrera %in% 'No aplica','pensado_cambio_carrera'] <- 'S�'
#survey[survey$pensado_cambio_carrera %in% 'Tal vez','pensado_cambio_carrera'] <- 'S�'
#survey$pensado_cambio_carrera <- droplevels(survey$pensado_cambio_carrera)


#survey[as.character(survey$momento_test_vocacional) %in% 'No aplica','momento_test_vocacional'] <- 'Sí'

survey$momento_test_vocacional <- as.character(survey$momento_test_vocacional)
survey$momento_test_vocacional[survey$momento_test_vocacional %in% 'No aplica'] <- 'No realizo test'
survey$momento_test_vocacional <- factor(survey$momento_test_vocacional)

survey$momento_test_vocacional <- droplevels(survey$momento_test_vocacional)

#boxplot(prop.table(table(survey$momento_test_vocacional)))

# Eliminando las variables innecesarias
survey <- survey[, !(names(survey) %in% c(
    'Timestamp',
    'lugar_test_vocacional',
    'nombre_opcion_carrera',
    'nombre_carrera',
    'año_reprobacion',
    'comunicación_oral_escrita.', 
    'relaciones_interpersonales.', 
    'creatividad', 
    'liderazgo', 
    'autodidacta', 
    'calculo_mental', 
    'resolucion_puzzles', 
    'juegos_estrategia', 
    'operaciones_matematicas'  
    )
  )
]

"
survey %>% 
  select(momento_test_vocacional) %>% 
  group_by(momento_test_vocacional) %>% 
  summarise(cantidad = n()) %>% 
  ggplot(data = .) +
  geom_boxplot(mapping = aes(x = cantidad)) +
  coord_flip()

survey %>% 
  mutate(momento_test_vocacional = ifelse(
    momento_test_vocacional == 'No aplica',
    'No realizo test',
    as.character(momento_test_vocacional)
  )
  ) %>% 
  select(momento_test_vocacional) %>% 
  group_by(momento_test_vocacional) %>% 
  summarise(cantidad = n()) %>% 
  ggplot(data = .) +
  geom_histogram(mapping = aes(x = cantidad), binwidth = 30)


survey %>% 
  mutate(momento_test_vocacional = ifelse(
    momento_test_vocacional == 'No aplica',
    'No realizo test',
    as.character(momento_test_vocacional)
  )
  ) %>% 
  select(momento_test_vocacional) %>% 
  group_by(momento_test_vocacional) %>% 
  summarise(cantidad = n()) %>% 
  ggplot(data = .) +
  geom_qq(mapping = aes(sample = cantidad)) +
  geom_qq_line(mapping = aes(sample = cantidad))


survey %>% 
  group_by(factores_cambio_carrera) %>% 
  summarise(cantidad = n()) %>% 
  ggplot(data = .) +
  geom_boxplot(mapping = aes(x = cantidad)) +
  coord_flip()

"

# Hipótesis 1 #####
'¿Por qué los estudiantes de las carreras de ingeniería de 
la Universidad Autónoma de Honduras realizan cambios de carrera 
durante la primera mitad de vida universitaria?'
# Variables necesarias
# Grado de satisfacción con la carrera escogida
#	Índice de reprobación de clases
#	Edad
#	Ingresos económicos
#	motivación
#	satisfacción_docentes

#	satisfaccion_carrera #####

# satisfaccion_carrera vs reprobado
table(survey$satisfaccion_carrera, survey$reprobado)

prop.table(table(survey$satisfaccion_carrera, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de satisfaccion_carrera y reprobado son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_carrera, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# satisfaccion_carrera vs edad
table(survey$satisfaccion_carrera, survey$edad)

prop.table(table(survey$satisfaccion_carrera, survey$edad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(edad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(edad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_carrera y edad son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_carrera, survey$edad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# satisfaccion_carrera vs ingreso_mensual
table(survey$satisfaccion_carrera, survey$ingreso_mensual)

prop.table(table(survey$satisfaccion_carrera, survey$ingreso_mensual),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(ingreso_mensual)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(ingreso_mensual)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_carrera y ingreso_mensual son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_carrera, survey$ingreso_mensual), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

# satisfaccion_carrera vs satisfaccion_docentes
table(survey$satisfaccion_carrera, survey$satisfaccion_docentes)

prop.table(table(survey$satisfaccion_carrera, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_carrera, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_carrera y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_carrera, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# reprobado #####

# reprobado vs satisfaccion_carrera
table(survey$reprobado, survey$satisfaccion_carrera)

prop.table(table(survey$reprobado, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de reprobado y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# reprobado vs edad FALTOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
table(survey$reprobado, survey$edad)

prop.table(table(survey$reprobado, survey$edad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado, fill = factor(edad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado, fill = factor(edad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado y edad son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado, survey$edad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# reprobado vs ingreso_mensual faltooooooooooooooooo
table(survey$reprobado, survey$ingreso_mensual)

prop.table(table(survey$reprobado, survey$ingreso_mensual),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado, fill = factor(ingreso_mensual)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado, fill = factor(ingreso_mensual)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado y ingreso_mensual son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado, survey$ingreso_mensual), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# reprobado vs satisfaccion_docentes
table(survey$reprobado, survey$satisfaccion_docentes)

prop.table(table(survey$reprobado, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

# edad #####

# edad vs satisfaccion_carrera faltoooooooooooooooooooooooo
table(survey$edad, survey$satisfaccion_carrera)

prop.table(table(survey$edad, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = edad, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = edad, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de edad y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$edad, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# edad vs reprobado faltoooooooooooooooo
table(survey$edad, survey$reprobado)

prop.table(table(survey$edad, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = edad, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = edad, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de edad y reprobado son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$edad, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# edad vs ingreso_mensual faltoooooooooooooo
table(survey$edad, survey$ingreso_mensual)

prop.table(table(survey$edad, survey$ingreso_mensual),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = edad, fill = factor(ingreso_mensual)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = edad, fill = factor(ingreso_mensual)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de edad y ingreso_mensual son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$edad, survey$ingreso_mensual), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# edad vs satisfaccion_docentes
table(survey$edad, survey$satisfaccion_docentes)

prop.table(table(survey$edad, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = edad, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = edad, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de edad y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$edad, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# ingreso_mensual #####

# ingreso_mensual vs satisfaccion_carrera faltooooooooooooo
table(survey$ingreso_mensual, survey$satisfaccion_carrera)

prop.table(table(survey$ingreso_mensual, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de ingreso_mensual y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$ingreso_mensual, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# ingreso_mensual vs reprobado fatoooooo
table(survey$ingreso_mensual, survey$reprobado)

prop.table(table(survey$ingreso_mensual, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de ingreso_mensual y ingreso_mensual son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$ingreso_mensual, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# ingreso_mensual vs edad faltooo
table(survey$ingreso_mensual, survey$edad)

prop.table(table(survey$ingreso_mensual, survey$edad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(edad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(edad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de ingreso_mensual y edad son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$ingreso_mensual, survey$edad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# ingreso_mensual vs motivacion faltooo
table(survey$ingreso_mensual, survey$motivacion)

prop.table(table(survey$ingreso_mensual, survey$motivacion),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(motivacion)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(motivacion)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de ingreso_mensual y motivacion son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$ingreso_mensual, survey$motivacion), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# ingreso_mensual vs satisfaccion_docentes faltoo
table(survey$ingreso_mensual, survey$satisfaccion_docentes)

prop.table(table(survey$ingreso_mensual, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = ingreso_mensual, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de ingreso_mensual y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$ingreso_mensual, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

# motivacion #####

# motivacion vs satisfaccion_carrera
table(survey$motivacion, survey$satisfaccion_carrera)

prop.table(table(survey$motivacion, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = motivacion, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = motivacion, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de motivacion y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$motivacion, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# motivacion vs reprobado
table(survey$motivacion, survey$reprobado)

prop.table(table(survey$motivacion, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = motivacion, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = motivacion, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de motivacion y motivacion son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$motivacion, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# motivacion vs edad faltoooo
table(survey$motivacion, survey$edad)

prop.table(table(survey$motivacion, survey$edad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = motivacion, fill = factor(edad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = motivacion, fill = factor(edad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de motivacion y edad son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$motivacion, survey$edad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# motivacion vs ingreso_mensual fattoooo
table(survey$motivacion, survey$ingreso_mensual)

prop.table(table(survey$motivacion, survey$ingreso_mensual),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = motivacion, fill = factor(ingreso_mensual)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = motivacion, fill = factor(ingreso_mensual)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de motivacion y motivacion son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$motivacion, survey$ingreso_mensual), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# motivacion vs satisfaccion_docentes
table(survey$motivacion, survey$satisfaccion_docentes)

prop.table(table(survey$motivacion, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = motivacion, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = motivacion, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de motivacion y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$motivacion, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

# satisfaccion_docentes #####

# satisfaccion_docentes vs satisfaccion_carrera NOO
table(survey$satisfaccion_docentes, survey$satisfaccion_carrera)

prop.table(table(survey$satisfaccion_docentes, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de satisfaccion_docentes y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_docentes, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# satisfaccion_docentes vs reprobado
table(survey$satisfaccion_docentes, survey$reprobado)

prop.table(table(survey$satisfaccion_docentes, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_docentes y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_docentes, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# satisfaccion_docentes vs edad
table(survey$satisfaccion_docentes, survey$edad)

prop.table(table(survey$satisfaccion_docentes, survey$edad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(edad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(edad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_docentes y edad son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_docentes, survey$edad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# satisfaccion_docentes vs ingreso_mensual
table(survey$satisfaccion_docentes, survey$ingreso_mensual)

prop.table(table(survey$satisfaccion_docentes, survey$ingreso_mensual),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(ingreso_mensual)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(ingreso_mensual)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_docentes y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_docentes, survey$ingreso_mensual), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# satisfaccion_docentes vs motivacion
table(survey$satisfaccion_docentes, survey$motivacion)

prop.table(table(survey$satisfaccion_docentes, survey$motivacion),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(motivacion)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = satisfaccion_docentes, fill = factor(motivacion)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de satisfaccion_docentes y satisfaccion_docentes son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$satisfaccion_docentes, survey$motivacion), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# Hipótesis 2 #####
'¿Cómo puede cada uno de los departamentos de ingeniería 
ayudar a los estudiantes a saber si sus habilidades personales 
son afines con las de un profesional de dicha área?'
# Variables necesarias
#	reprobado
#	reprobado_mas_de_dos
#	satisfacción_carrera 
#	satisfacción_docente

# reprobado_mas_de_dos #####

# reprobado_mas_de_dos vs satisfaccion_carrera
table(survey$reprobado_mas_de_dos, survey$satisfaccion_carrera)

prop.table(table(survey$reprobado_mas_de_dos, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de reprobado_mas_de_dos y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado_mas_de_dos, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# reprobado_mas_de_dos vs reprobado
table(survey$reprobado_mas_de_dos, survey$reprobado)

prop.table(table(survey$reprobado_mas_de_dos, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado_mas_de_dos y reprobado_mas_de_dos son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado_mas_de_dos, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# reprobado_mas_de_dos vs satisfaccion_docentes
table(survey$reprobado_mas_de_dos, survey$satisfaccion_docentes)

prop.table(table(survey$reprobado_mas_de_dos, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado_mas_de_dos y reprobado_mas_de_dos son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado_mas_de_dos, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.



#qqnorm(survey$cambio_carrera)# reprobado_mas_de_dos #####

# reprobado_mas_de_dos vs satisfaccion_carrera
table(survey$reprobado_mas_de_dos, survey$satisfaccion_carrera)

prop.table(table(survey$reprobado_mas_de_dos, survey$satisfaccion_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))


# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de reprobado_mas_de_dos y satisfaccion_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado_mas_de_dos, survey$satisfaccion_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# reprobado_mas_de_dos vs reprobado
table(survey$reprobado_mas_de_dos, survey$reprobado)

prop.table(table(survey$reprobado_mas_de_dos, survey$reprobado),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(reprobado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(reprobado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado_mas_de_dos y reprobado_mas_de_dos son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado_mas_de_dos, survey$reprobado), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# reprobado_mas_de_dos vs satisfaccion_docentes
table(survey$reprobado_mas_de_dos, survey$satisfaccion_docentes)

prop.table(table(survey$reprobado_mas_de_dos, survey$satisfaccion_docentes),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = reprobado_mas_de_dos, fill = factor(satisfaccion_docentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de reprobado_mas_de_dos y reprobado_mas_de_dos son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$reprobado_mas_de_dos, survey$satisfaccion_docentes), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

# Hipótesis 3 #####
'¿Saben los estudiantes de la existencia de pruebas de aptitud que realiza el 
departamento de psicología de la universidad?'
# asesoria_universitaria
# previo_test_vocacional
# momento_test_vocacional
# conocimiento_test_unah

# asesoria_universitaria #####

# asesoria_universitaria vs previo_test_vocacional
table(survey$asesoria_universitaria, survey$previo_test_vocacional)

prop.table(table(survey$asesoria_universitaria, survey$previo_test_vocacional),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = asesoria_universitaria, fill = factor(previo_test_vocacional)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = asesoria_universitaria, fill = factor(previo_test_vocacional)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de asesoria_universitaria y previo_test_vocacional son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$asesoria_universitaria, survey$previo_test_vocacional), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# asesoria_universitaria vs momento_test_vocacional
table(survey$asesoria_universitaria, survey$momento_test_vocacional)

prop.table(table(survey$asesoria_universitaria, survey$momento_test_vocacional),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = asesoria_universitaria, fill = factor(momento_test_vocacional)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = asesoria_universitaria, fill = factor(momento_test_vocacional)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de asesoria_universitaria y asesoria_universitaria son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$asesoria_universitaria, survey$momento_test_vocacional), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# asesoria_universitaria vs conocimiento_test_unah
table(survey$asesoria_universitaria, survey$conocimiento_test_unah)

prop.table(table(survey$asesoria_universitaria, survey$conocimiento_test_unah),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = asesoria_universitaria, fill = factor(conocimiento_test_unah)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = asesoria_universitaria, fill = factor(conocimiento_test_unah)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de asesoria_universitaria y asesoria_universitaria son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$asesoria_universitaria, survey$conocimiento_test_unah), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# previo_test_vocacional #####

# previo_test_vocacional vs asesoria_universitaria
table(survey$previo_test_vocacional, survey$asesoria_universitaria)

prop.table(table(survey$previo_test_vocacional, survey$asesoria_universitaria),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = previo_test_vocacional, fill = factor(asesoria_universitaria)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = previo_test_vocacional, fill = factor(asesoria_universitaria)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de previo_test_vocacional y asesoria_universitaria son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$previo_test_vocacional, survey$asesoria_universitaria), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# previo_test_vocacional vs momento_test_vocacional
table(survey$previo_test_vocacional, survey$momento_test_vocacional)

prop.table(table(survey$previo_test_vocacional, survey$momento_test_vocacional),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = previo_test_vocacional, fill = factor(momento_test_vocacional)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = previo_test_vocacional, fill = factor(momento_test_vocacional)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de previo_test_vocacional y previo_test_vocacional son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$previo_test_vocacional, survey$momento_test_vocacional), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# previo_test_vocacional vs conocimiento_test_unah
table(survey$previo_test_vocacional, survey$conocimiento_test_unah)

prop.table(table(survey$previo_test_vocacional, survey$conocimiento_test_unah),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = previo_test_vocacional, fill = factor(conocimiento_test_unah)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = previo_test_vocacional, fill = factor(conocimiento_test_unah)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de previo_test_vocacional y previo_test_vocacional son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$previo_test_vocacional, survey$conocimiento_test_unah), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# momento_test_vocacional #####

# momento_test_vocacional vs asesoria_universitaria
table(survey$momento_test_vocacional, survey$asesoria_universitaria)

prop.table(table(survey$momento_test_vocacional, survey$asesoria_universitaria),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = momento_test_vocacional, fill = factor(asesoria_universitaria)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = momento_test_vocacional, fill = factor(asesoria_universitaria)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de momento_test_vocacional y asesoria_universitaria son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$momento_test_vocacional, survey$asesoria_universitaria), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# momento_test_vocacional vs previo_test_vocacional
table(survey$momento_test_vocacional, survey$previo_test_vocacional)

prop.table(table(survey$momento_test_vocacional, survey$previo_test_vocacional),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = momento_test_vocacional, fill = factor(previo_test_vocacional)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = momento_test_vocacional, fill = factor(previo_test_vocacional)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de momento_test_vocacional y momento_test_vocacional son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$momento_test_vocacional, survey$previo_test_vocacional), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# momento_test_vocacional vs conocimiento_test_unah
table(survey$momento_test_vocacional, survey$conocimiento_test_unah)

prop.table(table(survey$momento_test_vocacional, survey$conocimiento_test_unah),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = momento_test_vocacional, fill = factor(conocimiento_test_unah)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = momento_test_vocacional, fill = factor(conocimiento_test_unah)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de momento_test_vocacional y momento_test_vocacional son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$momento_test_vocacional, survey$conocimiento_test_unah), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.

# conocimiento_test_unah #####

# conocimiento_test_unah vs asesoria_universitaria
table(survey$conocimiento_test_unah, survey$asesoria_universitaria)

prop.table(table(survey$conocimiento_test_unah, survey$asesoria_universitaria),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = conocimiento_test_unah, fill = factor(asesoria_universitaria)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = conocimiento_test_unah, fill = factor(asesoria_universitaria)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de conocimiento_test_unah y asesoria_universitaria son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$conocimiento_test_unah, survey$asesoria_universitaria), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# conocimiento_test_unah vs previo_test_vocacional
table(survey$conocimiento_test_unah, survey$previo_test_vocacional)

prop.table(table(survey$conocimiento_test_unah, survey$previo_test_vocacional),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = conocimiento_test_unah, fill = factor(previo_test_vocacional)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = conocimiento_test_unah, fill = factor(previo_test_vocacional)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de conocimiento_test_unah y conocimiento_test_unah son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$conocimiento_test_unah, survey$previo_test_vocacional), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# conocimiento_test_unah vs momento_test_vocacional
table(survey$conocimiento_test_unah, survey$momento_test_vocacional)

prop.table(table(survey$conocimiento_test_unah, survey$momento_test_vocacional),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = conocimiento_test_unah, fill = factor(momento_test_vocacional)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = conocimiento_test_unah, fill = factor(momento_test_vocacional)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de conocimiento_test_unah y momento_test_vocacional son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$conocimiento_test_unah, survey$momento_test_vocacional), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.


# Hipótesis 4 #####
'¿Cuáles son los factores más comunes que impulsan
a un alumno a realizar un cambio de carrera?'
# factores_cambio 
# indice_academico
# porcentaje_clases_facultad

# factores_cambio #####

# factores_cambio vs indice_academico
table(survey$factores_cambio, survey$indice_academico)

prop.table(table(survey$factores_cambio, survey$indice_academico),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = survey$factores_cambio, fill = factor(indice_academico)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = survey$factores_cambio, fill = factor(indice_academico)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de factores_cambio y indice_academico son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$factores_cambio, survey$indice_academico), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# factores_cambio vs porcentaje_clases_facultad
table(survey$factores_cambio, survey$porcentaje_clases_facultad)

prop.table(table(survey$factores_cambio, survey$porcentaje_clases_facultad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = survey$factores_cambio, fill = factor(porcentaje_clases_facultad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = survey$factores_cambio, fill = factor(porcentaje_clases_facultad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de factores_cambio y factores_cambio son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$factores_cambio, survey$porcentaje_clases_facultad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# indice_academico #####

# indice_academico vs factores_cambio 
table(survey$indice_academico, survey$factores_cambio)

prop.table(table(survey$indice_academico, survey$factores_cambio),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = indice_academico, fill = factor(survey$factores_cambio)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = indice_academico, fill = factor(survey$factores_cambio)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de indice_academico y factores_cambio son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$indice_academico, survey$factores_cambio), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# indice_academico vs porcentaje_clases_facultad
table(survey$indice_academico, survey$porcentaje_clases_facultad)

prop.table(table(survey$indice_academico, survey$porcentaje_clases_facultad),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = indice_academico, fill = factor(porcentaje_clases_facultad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = indice_academico, fill = factor(porcentaje_clases_facultad)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de indice_academico y indice_academico son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$indice_academico, survey$porcentaje_clases_facultad), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.

# porcentaje_clases_facultad #####

# porcentaje_clases_facultad vs factores_cambio 
table(survey$porcentaje_clases_facultad, survey$factores_cambio)

prop.table(table(survey$porcentaje_clases_facultad, survey$factores_cambio),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = porcentaje_clases_facultad, fill = factor(survey$factores_cambio)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = porcentaje_clases_facultad, fill = factor(survey$factores_cambio)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chis.test es menos a 0.05


# H_0: Las categorias de porcentaje_clases_facultad y factores_cambio son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$porcentaje_clases_facultad, survey$factores_cambio), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.


# porcentaje_clases_facultad vs indice_academico
table(survey$porcentaje_clases_facultad, survey$indice_academico)

prop.table(table(survey$porcentaje_clases_facultad, survey$indice_academico),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = porcentaje_clases_facultad, fill = factor(indice_academico)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = porcentaje_clases_facultad, fill = factor(indice_academico)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de porcentaje_clases_facultad y indice_academico son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$porcentaje_clases_facultad, survey$indice_academico), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.

# Hipótesis 5 #####
'¿Cuáles serán las probabilidades o razones que una persona 
continúe estudiando una carrera que no es de su agrado
y afectar el tiempo invertido en sus estudios universitarios?'
#	porcentaje_clases_facultad
#	procentaje_clases_cursada
#	porcentaje_clases_matematica

# factores_no_cambio #####

# factores_no_cambio vs pensado_cambio_carrera
table(survey$factores_no_cambio, survey$pensado_cambio_carrera)

prop.table(table(survey$factores_no_cambio, survey$pensado_cambio_carrera),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = factores_no_cambio, fill = factor(pensado_cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = factores_no_cambio, fill = factor(pensado_cambio_carrera)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de factores_no_cambio y pensado_cambio_carrera son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$factores_no_cambio, survey$pensado_cambio_carrera), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value no rechazamos nuestra hipotesis nula,
# por la tanto las variables son independientes.

# factores_no_cambio vs procentaje_clases_cursadas
table(survey$factores_no_cambio, survey$procentaje_clases_cursadas)

prop.table(table(survey$factores_no_cambio, survey$procentaje_clases_cursadas),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = factores_no_cambio, fill = factor(procentaje_clases_cursadas)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = factores_no_cambio, fill = factor(procentaje_clases_cursadas)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de factores_no_cambio y procentaje_clases_cursadas son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$factores_no_cambio, survey$procentaje_clases_cursadas), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value  rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

# factores_no_cambio vs ingreso_mensual
table(survey$factores_no_cambio, survey$ingreso_mensual)

prop.table(table(survey$factores_no_cambio, survey$ingreso_mensual),2)

# Visualización de las frecuencias 

ggplot(survey) +
  aes(x = factores_no_cambio, fill = factor(ingreso_mensual)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = factores_no_cambio, fill = factor(ingreso_mensual)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

# Prueba de Hipótesis

# Regla: no rechazamos nuestras hipotesis nula cuando el p-value 
# de nuestra prueba chisq.test es menos a 0.05


# H_0: Las categorias de factores_no_cambio y ingreso_mensual son independientes.
# H_A: Las categorias son dependientes.

chisq.test(table(survey$factores_no_cambio, survey$ingreso_mensual), simulate.p.value = TRUE)


# Conclusion: Según nuestro p-value rechazamos nuestra hipotesis nula,
# por la tanto las variables son dependientes.

#write
write.csv(survey,"encuesta_transformada_regresion1.csv", row.names = FALSE)