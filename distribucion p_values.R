# Número de repeticiones en la simulación
repeticiones <- 10000
# Tamaño de las dos muestras
n_muestra_1 <- 100
n_muestra_2 <- 100
# Valor de mu (media) de la población de cada una de las muestras
media_muestra_1 <- 0
media_muestra_2 <- 0
# Desviación estandar de la población de cada una de las muestras
ds_muestra_1 <- 1
ds_muestra_2 <- 1

# Valores que vamos a salvar, estadístico t, p valores y diferencia de las medias
t_values <- rep(0, repeticiones)
p_values <- rep(0, repeticiones)
diferencia_de_medias <- rep(0, repeticiones)

# Repetimos la simulación tantas veces como repeticiones hemos solicitado
for (simulacion in 1:repeticiones){
  # Generamos dos muestras normales de media y desviación estandar conocidas
  muestra_1 <- rnorm(n_muestra_1, media_muestra_1, ds_muestra_1)
  muestra_2 <- rnorm(n_muestra_2, media_muestra_2, ds_muestra_2)
  
  # Calculamos una prueba t para muestras independientes y varianzas distintas
  result <- t.test(muestra_1, muestra_2)
  # Salvamos los tres valores que nos interesan de cada prueba t
  p_values[simulacion] <- as.double(result$p.value)
  t_values[simulacion] <- as.double(result$statistic)
  diferencia_de_medias[simulacion] <- result$estimate[1] - result$estimate[2]
  
}

# Función para hacer un histograma de los distintos valores que queremos generar
plot_relative_frequency <- function(cur_values, name_variable){
  hist_vector <- hist(cur_values, breaks = 100, plot = FALSE)
  hist_vector$counts <- hist_vector$counts / sum(hist_vector$counts)
  if (name_variable == "p valores"){
    xlims_plot <- c(0,1)
    threshold <- sum(cur_values < 0.05)/length(cur_values)
    name_title <- paste0("Distribución de p valores - p<0.05: ", threshold)
    line_placer = 0.05
  }
  else if (name_variable == "t valores") {
    xlims_plot <- c(-6,6)
    name_title <- "Distribución de estadístico t"
    line_placer = 0
  }
  else if (name_variable == "medias") {
    xlims_plot <- c(min(cur_values), max(cur_values))
    name_title <- "Distribución de diferencias de medias"
    line_placer = 0
  }
  plot(hist_vector, main = name_title, xlim = xlims_plot)
  abline(v=line_placer, col = 'red')
  
  if (name_variable == "p valores"){
    abline(h=0.01, col = 'blue')
    }
}

# Ploteamos los histogramas de los tres valores que hemos generado
par(mfrow=c(1,3))
plot_relative_frequency(p_values, "p valores")
plot_relative_frequency(t_values, "t valores")
plot_relative_frequency(diferencia_de_medias, "medias")

# Ploteamos el histograma de las diferencias de medias de las simulaciones significativas
par(mfrow=c(1,1))
hist(diferencia_de_medias[p_values<0.05],breaks = 100)

# Distribución tamaños del efecto
# https://rpsychologist.com/d3/cohend/