################# PARAMETROS ################# 
set.seed(77686486) # Establecemos una semilla para realizar las comparaciones de manera justa.
imageNumber <- 2
distanceMeasure <- "euclidean" # "manhattan" ó "euclidean"
popSize <- 300  # Tamaño de la poblacion. imagen0 = 150 | imagen1 = 200 | imagen2 = ? 
mutProb <- 0.10 # Probabilidad de mutación
cxProb  <- 0.8  # Probabilidad de cruce
maxIter <- 100000 # Maximo numero de iteraciones. imagen0 = 15000 | imagen1 = 30000 | imagen2 ???= +100K CURRENT = 30K

############### END PARAMETROS ############### 


# Primeramente, cargamos las librerias que son necesarias para la ejecución de nuestro algoritmo
library(GA)
library(doParallel)
library(ggplot2)

# Cargamos el directorio de trabajo así como las imagen a reconstruir.
setwd("C:/Users/ps3aj/Desktop/TrabajoFinal_2")
image <- read.table(paste("images\\",imageNumber,"\\shuffled.txt", sep = ""))

# Comprobamos si cargamos la imagen bien y es válida
cat(paste("Imagen válida:", !(min(image)<0 && max(image)>255)))

## Calculamos los datos necesarios para nuestro algoritmo. En este caso la matriz de distancias asi como la función de fitness que usara a esta. ##

# Calculamos la matriz de distancias entre columnas (ciudades) para que nos sirva posteriormente para la función de fitness
distanceMatrix <- as.matrix(dist(image, method = distanceMeasure))

# Desarrollamos nuestra función de fitness. En este caso, evaluamos la solución como un orden de ciudades a recorrer,
# y calculamos el coste según la matriz de distancias del problema TSP calculada anteriormente.
fitness_check <- function(city_tour, TSP_matrix){
  cost <- 0
  for(i in 1:(length(city_tour) - 1)) {
    cost <- cost - TSP_matrix[city_tour[i], city_tour[i+1]]
  }
  return(cost)
}

TSP_resolver <- ga(type = "permutation", popSize = popSize, pmutation = mutProb, pcrossover = cxProb, elitism = round(popSize*0.05), 
                   crossover = gaperm_pmxCrossover, mutation = gaperm_simMutation, selection = gaperm_tourSelection, population = gaperm_Population, 
                   lower = 1, upper = nrow(image), fitness = fitness_check, TSP_matrix = distanceMatrix,
                   maxiter = maxIter, parallel = TRUE, monitor = FALSE,  keepBest = TRUE)


# Grafica de comparativa de crecimiento del Fitness
best_fitness <- TSP_resolver@summary[, "max"]
no_iter <- 0:(nrow(TSP_resolver@summary)-1)

ggplot(data=data.frame(no_iter, best_fitness), aes(x=no_iter, y=best_fitness)) +
  geom_line(color="darkblue", size=1) +
  labs(title = "Mejor fitness por iteración aplicando la distancia de Manhattan",x="Número de iteraciones", y="Fitness de la mejor solución en dicha iteración")+
  theme(plot.title = element_text(hjust = 0.5))

# ggplot() +  geom_line(color="darkblue", size=1, aes(x=no_iter, y=best_fitness)) + theme(plot.title = element_text(hjust = 0.5)) +
#   labs(title = "Mejor fitness por iteración aplicando la distancia de Manhattan",x="Número de iteraciones", y="Fitness de la mejor solución en dicha iteración")
  

# Guardamos checkpoints de las imagenes a cada tercio de ejecución
write.table(image[TSP_resolver@bestSol[[round(maxIter/3)]][1, ], ], row.names = FALSE, col.names = FALSE,
            file = paste("images\\",imageNumber,"\\",distanceMeasure,"\\nice_",round(maxIter/3),"_",distanceMeasure,".txt", sep = ""))

write.table(image[TSP_resolver@bestSol[[round(maxIter*2/3)]][1, ], ], row.names = FALSE, col.names = FALSE,
            file = paste("images\\",imageNumber,"\\",distanceMeasure,"\\nice_",round(maxIter*2/3),"_",distanceMeasure,".txt", sep = ""))

write.table(image[TSP_resolver@bestSol[[maxIter]][1, ], ], row.names = FALSE, col.names = FALSE,
            file = paste("images\\",imageNumber,"\\",distanceMeasure,"\\nice_",maxIter,"_",distanceMeasure,".txt", sep = ""))



