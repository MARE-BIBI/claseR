#library 

library(tidyverse)
library(ggplot2)
library(DT)
library(readxl)


#Carga de datos
Tabla_Final= read_xlsx("Data/Tabla_Final.xlsx")

dim(Tabla_Final)

#Modificar nombre de atributos:
names(Tabla_Final)[1]="Identificador"
names(Tabla_Final)[2]= "Tipo_Titulo"
names(Tabla_Final)[3]= "Nombre_Pelicula"
names(Tabla_Final)[4]= "Adulto"
names(Tabla_Final)[5]= "Año_Inicio"
names(Tabla_Final)[6]= "Duración_Pelicula"
names(Tabla_Final)[7]= "Genero"
names(Tabla_Final)[8]= "Rating"
names(Tabla_Final)[9]= "Numero_Votos"


#Validación datos nulos:

Nulos= 2.865
Registros_totales= 171.743

Porcentaje_nulos = (Nulos/Registros_totales*100)
Porcentaje_nulos


#Grafico de barras Año/número de votos
ggplot(data=Tabla_Final, aes(x=Año_Inicio,y=Numero_Votos)) + geom_bar(stat="identity")



# Filtrar Base de datos eliminando el año 2020 y 2021

Data_Final= Tabla_Final %>%  filter(Año_Inicio>=2000&Año_Inicio<2019)


# Selección de variables importantes según hipotesis:

Data_nueva= Data_Final %>% select(Genero,Rating,Numero_Votos,Año_Inicio)


# En total tengo un total de 999 generos de pelicula:
unique(Data_nueva$Genero)


# Generar top 5 generos con más votos

Top5_generos = Data_nueva %>% group_by(Genero) %>% summarize(Promedio_votantes = sum(Numero_Votos)) %>%
arrange(desc(Promedio_votantes))

generos_seleccionados= c("Action,Adventure,Sci-Fi","Adventure,Animation,Comedy","Comedy,Drama,Romance","Action,Crime,Drama","Drama")

Data_Top5_generos= Data_nueva%>% filter(Genero %in% generos_seleccionados)

# Hipotesis correcta:
Votos_Genero_ = Data_Top5_generos%>% group_by(Genero) %>% 
  summarize(Suma_votacion= sum(Numero_Votos)) %>% 
  mutate(Promedio_votacion= round(Suma_votacion/119481869*100,2))%>% 
  arrange(desc(Suma_votacion))

sum(Votos_Genero_$Suma_votacion)
# GRAFICOS
ggplot(Votos_Genero_,mapping = aes(x="",y=Promedio_votacion, fill=Genero))+
  geom_bar(stat = "identity",color="white")+
  geom_text(aes(label=Promedio_votacion),
            position=position_stack(vjust=0.5))

#Barras con número de votos:

ggplot(data=Votos_Genero_, aes(x=Genero,y=Suma_votacion)) + geom_bar(stat="identity", position="stack")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


# se filtra por los generos que quedaron en 2,3,4,5 lugar:
generos_seleccionado= c("Adventure,Animation,Comedy","Comedy,Drama,Romance","Action,Crime,Drama","Drama")

Data_Top5_gener= Data_nueva%>% filter(Genero %in% generos_seleccionado)

# Promedio de los generos que quedaron en 2,3,4,5, lugar
mean(Votos_Genero_$Suma_votacion)



         



















