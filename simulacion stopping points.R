# Número de repeticiones en la simulación
repeticiones <- 1000
# Tamaño inicial de las dos muestras
n_muestra_1 <- 10
n_muestra_2 <- 10

# Aumento del tamaño de la muestra en cada paso
paso <- 5

# Tamaño máximo de las muestras que vamos a emplear
max_pasos <- 32

# Valor de mu (media) de la población de cada una de las muestras
media_muestra_1 <- 0
media_muestra_2 <- 0
# Desviación estandar de la población de cada una de las muestras
ds_muestra_1 <- 1
ds_muestra_2 <- 1

p_values <- rep(0, repeticiones)
punto_parada <- rep(0, repeticiones)
final <- rep(0, repeticiones)

for (simulacion in 1:repeticiones){
  muestra_1 <- rnorm(n_muestra_1, media_muestra_1, ds_muestra_1)
  muestra_2 <- rnorm(n_muestra_2, media_muestra_2, ds_muestra_2)
  result <- t.test(muestra_1, muestra_2)
  
  for (paso_actual in 1:max_pasos){
    if (result$p.value < 0.05) {break}
    else{
    muestra_1 <- c(muestra_1, rnorm(paso, media_muestra_1, ds_muestra_1))
    muestra_2 <- c(muestra_2, rnorm(paso, media_muestra_2, ds_muestra_2))
    result <- t.test(muestra_1, muestra_2)
    }
  }

  p_values[simulacion] <- result$p.value
  punto_parada[simulacion] <- length(muestra_1)
  if (result$p.value < 0.05) {final[simulacion] <- 1}
}

hist(punto_parada[p_values<0.05], breaks = 100)
hist(p_values)
hist(final)
