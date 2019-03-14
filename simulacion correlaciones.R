library(MASS)

# Número de repeticiones en la simulación
repeticiones <- 1000
# Tamaño de la muestra
n_muestra_1 <- 180
# Valor de mu (media) de cada una de las variables
media_v1 <- 0
media_v2 <- 0
# Desviación estandar de cada una de las variables
ds_v1 <- 1
ds_v2 <- 1
# Correlación que existe entre las variables
correlacion <- 0.2

p_values <- rep(0, repeticiones)
cor_values <- rep(0, repeticiones)

for (simulacion in 1:repeticiones){
  cor_data <- mvrnorm(n = n_muestra_1, mu=c(media_v1,media_v2),
                      Sigma = matrix(c(ds_v1,correlacion * sqrt(ds_v1*ds_v2),
                                       correlacion * sqrt(ds_v1*ds_v2),ds_v2),2,2))
  result <- cor.test(cor_data[,1], cor_data[,2])
  p_values[simulacion] <- as.double(result$p.value)
  cor_values[simulacion] <- as.double(result$estimate)
}

hist(cor_values, breaks = 100)
hist(p_values, breaks = 100)
hist(cor_values[p_values<0.05],breaks = 100, xlim = c(-1,1))


library(pwr)
pwr::plot.power.htest(pwr.r.test(n = muestra_1, r = correlacion))
# https://www.statmethods.net/stats/power.html