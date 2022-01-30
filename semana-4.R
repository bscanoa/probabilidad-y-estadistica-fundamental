library(readr)
personas <- read_delim("/mnt/s/stiven/2021-2/UNAL/PEF/probabilidad-y-estadistica-fundamental/datasets/personas-semana-4.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Tabla de las variables 
tabla <- data.frame(personas$p6426, personas$ingtotob)
head(tabla)
tail(tabla)

# Filtro de elementos faltantes
filas <- which( is.na(tabla[, 1]) | tabla[, 2] == 0)
tabla <- tabla[-filas, ]

na.omit(tabla[, 1])
tabla <- na.omit(tabla)

head(tabla)
tail(tabla)


# Histograma de tiempo
hist(tabla[,1], breaks = 100, main = "Histograma de la tiempo en el trabajo", xlab = "Tiempo",
     ylab = "Frecuencia")

# Filtro con los datos menores a 600
filas1 <- which(tabla[, 1] > 601)
tabla <- tabla[-filas1, ]

# Histograma final
hist(tabla[,1], breaks = 100, main = "Histograma de tiempo en el trabajo", xlab = "Tiempo",
     ylab = "Frecuencia")


# Histograma con el ingreso total
hist(tabla[,2], breaks = 100, main = "Histograma del ingreso total", xlab = "Ingreso",
     ylab = "Frecuencia")

# Filtro de los datos
filas2 <- which(tabla[, 2] < 500000 | tabla[ ,2] > 5000000)
tabla <- tabla[-filas2, ]

# Histograma final
hist(tabla[,2], breaks = 100, main = "Histograma del ingreso total", xlab = "Ingreso",
     ylab = "Frecuencia")
plot(tabla[, 1], tabla[, 2], pch = 16, col = "blue", xlab = "Edad", ylab =
       "Ingreso total")

# Gráfico de dispersión
plot(tabla[, 1], tabla[, 2], pch = 16, col = "yellow", xlab = "Tiempo de antiguedad", ylab =
       "Ingreso total")

# Recta de tendencia
abline(lm(tabla[,2] ~ tabla[, 1]), lwd = 2, col = "purple") 

# Coeficientes de correlación
cor.test(tabla[, 1], tabla[, 2], method = "pearson")
cor.test(tabla[, 1], tabla[, 2], method = "spearman")
cor.test(tabla[, 1], tabla[, 2], method = "kendall")

# Pruebas de dsitribución normal
library(nortest)
ad.test(tabla[, 1])
ad.test(tabla[, 2])


library(testforDEP)
library(Rcpp)

# Muestra de 2000 datos
filas3 <- sample(1:length(tabla[, 1]), size = 1000, replace = TRUE, prob = NULL)
muestra <- tabla[filas3, ]
head(muestra)
tail(muestra)

testforDEP(x = muestra[, 1], y = muestra[, 2], test = "MIC", p.opt = "MC",
           num.MC = 100)
# resultado de test MIC =  Slot "TS": [1] 0.1385755 Slot "p_value":  [1] 0.2277228

testforDEP(x = muestra[, 1], y = muestra[, 2], test = "VEXLER", p.opt = "MC",
           num.MC = 100)
# resultado de test VEXLER = Slot "TS":[1] 526.0407  Slot "p_value": [1] 0.950495
        