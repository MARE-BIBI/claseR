#library 

library(tidyverse)
library(ggplot2)


#Carga de datos
dim(Tabla_Final)

#Grafico de barras Año/número de votos
ggplot(data=Tabla_Final, aes(x=startYear,y=numVotes)) + geom_bar(stat="identity", position="stack")

#Necesito saber el promedio de votantes por año:

Votante_año = Tabla_Final %>% group_by(startYear) %>% summarize(promedio_votantes = mean(numVotes))

#grafico de barras promedio de votantes año:

ggplot(data=Votante_año, aes(x=startYear,y=promedio_votantes)) + geom_bar(stat="identity", position="stack")

#Necesito saber el número de titulos sacados por año:

Peliculas_año = data.frame(table(Tabla_Final$startYear))

#Grafico de titulos sacados por año:

plot(x = Peliculas_año)

#Filtro Tabla_Final del año 2009 al año 2019 (por la pandemia)

Data_Final= Tabla_Final %>%  filter(startYear>2008&startYear<2020)










ggplot(Peliculas_año, aes(Freq,Var1 )) +
  geom_area(fill = rgb(0, 0.5,1, alpha = 0.5))+
  ggtitle("Titulos año")








str(Tabla_Final %>% group_by(startYear))



count(Tabla_Final,"startYear")



str(Tabla_Final$numVotes)

#Base de datos resumida para cada una de las variables:
  
summary(Tabla_Final)
Tabla_Final$averageRating


#Base de datos filtrada por genero y número de votos:

Genero_Votos= Tabla_Final %>% group_by(genres) %>% summarise(suma_votos= sum(numVotes)) %>%
              arrange(desc(suma_votos))


#Base de datos filtrada por número de votos:

Genero_Rating= Tabla_Final %>% group_by(genres) %>% summarise(Rating_promedio= mean(averageRating)) %>%
  arrange(desc(Rating_promedio))



#Base de datos filtrada por mejores generos:

generos_seleccionados= c("Action,Crime,Drama","Action,Adventure,Sci-Fi","Action,Adventure,Drama")

#Base de datos filtrada porlos 3 generos más votados:

generos_mas_votados= Tabla_Final %>% filter(genres %in% generos_seleccionados)






