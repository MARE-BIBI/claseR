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

# Generar top 5 generos con más votos

Top5_generos = Data_Final %>% group_by(genres) %>% summarize(promedio_votantes = mean(numVotes)) %>%
  arrange(desc(promedio_votantes))

generos_seleccionados= c("Adventure,Drama,War","Action,Adventure,Sci-Fi","Adventure,Fantasy,Mystery","Adventure,Mystery,Sci-Fi","	
Adventure,Drama,Sci-Fi")

Filtro_Top5= Data_Final%>% filter(genres %in% generos_seleccionados)

#grafico de genero más votados:

ggplot(data=Filtro_Top5, aes(x=genres,y=numVotes)) + geom_bar(stat="identity", position="stack")

#Número de titulos por cada genero:

Titulos_Genero = data.frame(table(Filtro_Top5$genres))

#grafico de titulos por cada genero:

ggplot(Titulos_Genero,aes(x="",y=Freq, fill=Var1))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=Freq),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")

#Pelicula con mejor Rating:

Top5_Rating = Data_Final %>% group_by(genres) %>% summarize(promedio_Rating = mean(averageRating)) %>%
  arrange(desc(promedio_Rating))

Rating_seleccionado= c("Drama,History,News","Documentary,News,Reality-TV","Action,History,Musical","Action,Fantasy,Musical","Comedy,Talk-Show")

Resumen_Rating= Data_Final %>% filter(genres %in% Rating_seleccionado)

#grafico de Rating:
  
ggplot(data=Resumen_Rating, aes(x=Rating_seleccionado,y=averageRating)) + geom_bar(stat="identity", position="stack")

# graficnúmero de votos por genero mejor Rating.

ggplot(data=Resumen_Rating, aes(x=Rating_seleccionado,y=numVotes)) + geom_bar(stat="identity", position="stack")

# ahora se seleccionara las peliculas con mejor Rating:

Data_ordenada_Votos= Data_Final %>% arrange(desc(numVotes))


# peliculas top 5: Falta hacer grafico() voy acá

Top5_Peliculas= c("Inception","Interstellar","The Dark Knight Rises","Django Unchained","Inglourious Basterds")

Resumen_peliculas_Top= Data_Final %>% filter(numVotes %in% Top5_Peliculas)


















#Filtrar por top 5 más votados:

Top5_generos_peliculas=c("Action,Adventure,Sci-Fi","Adventure,Drama,Sci-Fi","Action,Crime,Drama","Drama,Western","Adventure,Drama,War")

Resumen_Top_genero_peli= Data_Final %>% filter(genres %in% Top5_generos_peliculas)

Peliculas_año_generosTop5 = data.frame(table(Resumen_Top_genero_peli$numVotes))

Peliculas_año_ = data.frame(table(Peliculas_año_generosTop5$numVotes))


# Generos que más votación tienen
contar_genero= Tabla_Final%>% count(genres)


















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






