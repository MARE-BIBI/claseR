#library 

library(tidyverse)


dim(Tabla_Final)

Columnas= names(Tabla_Final)

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


