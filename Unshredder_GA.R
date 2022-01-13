################# PARAMETROS ################# 
set.seed(77686486) # Establecemos una semilla para realizar las comparaciones de manera justa.
imageNumber <- 0
distanceMeasure <- "euclidean" # "manhattan" ó "euclidean"
popSize <- 150  # Tamaño de la poblacion. imagen0 = 150 | imagen1 = 200 | imagen2 = 200
mutProb <- 0.10 # Probabilidad de mutación
cxProb  <- 0.8  # Probabilidad de cruce
maxIter <- 15000 # Maximo numero de iteraciones. imagen0 = 15000 | imagen1 = 30000 | imagen2 = 100000

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
ggplot() +  geom_line(color="darkblue", size=1, aes(x=0:(maxIter-1), y=TSP_resolver@summary[, "max"])) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Mejor fitness por iteración aplicando la distancia de Manhattan",x="Número de iteraciones", y="Fitness de la mejor solución en dicha iteración")

get_best_sol <- function(solution, iteration){
  return(solution@bestSol[[round(iteration)]][1, ])
}

# Guardamos las mejores ordenaciones segun el criterio definido en el informe.
correct_image_row_order <- list()
correct_image_row_order[[as.character(round(maxIter/3))]] <- get_best_sol(TSP_resolver, maxIter/3)-1
correct_image_row_order[[as.character(round(maxIter*2/3))]] <- get_best_sol(TSP_resolver, maxIter*2/3)-1
correct_image_row_order[[as.character(round(maxIter))]] <- get_best_sol(TSP_resolver, maxIter)-1

write.table(correct_image_row_order, file=paste("images\\",imageNumber,"\\",distanceMeasure,"\\row_order.csv", sep=""), sep=",")

