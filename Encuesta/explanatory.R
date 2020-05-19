library(dplyr)
library(caret)
library(ggplot2)

setwd("/")


setwd("Users/user/Documents/Encuesta")


# Carga de datos #########
survey <- read.csv('encuesta_transformada_regresion.csv', sep=",")

survey$cambio_carrera <- as.character(survey$cambio_carrera)

survey[survey$cambio_carrera %in% 'No', 'cambio_carrera'] <- "0"
survey[survey$cambio_carrera %in% 'Sí', 'cambio_carrera'] <- "1"

survey$cambio_carrera <- as.factor(survey$cambio_carrera)

set <- survey[,(names(survey) %in% c(
    'lugar_residencia',
    'horas_estudio_diario',
    'porcentaje_clases_matematica',
    'satisfaccion_carrera',
    'estudia_grupo',
    'motivacion',
    'trabaja',
    'edad',
    'asesoria_universitaria',
    'pensado_cambio_carrera'
  )
)]

cambio_carrera <- survey[,'cambio_carrera']


model <- glm(cambio_carrera ~ ., data = set, family = "binomial")

importances <- varImp(model)
importances$col <- row.names(importances)
importances <- importances %>% arrange(-Overall)
importances


# lugar_residencia ####
ggplot(set) +
  aes(x = lugar_residencia, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# horas_estudio_diario ####
ggplot(set) +
  aes(x = horas_estudio_diario, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# porcentaje_clases_matematica ####
ggplot(set) +
  aes(x = porcentaje_clases_matematica, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# satisfaccion_carrera####
ggplot(set) +
  aes(x = satisfaccion_carrera, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# estudia_grupo####
ggplot(set) +
  aes(x = estudia_grupo, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# motivacion####
ggplot(set) +
  aes(x = motivacion, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# trabaja####
ggplot(set) +
  aes(x = trabaja, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# edad####
ggplot(set) +
  aes(x = edad, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# asesoria_universitaria####
ggplot(set) +
  aes(x = asesoria_universitaria, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))


# pensado_cambio_carrera####
ggplot(set) +
  aes(x = pensado_cambio_carrera, fill = factor(cambio_carrera)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999","#E69F00"))
