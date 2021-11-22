library(readr)
Personas <- read_delim("C:/Users/User/Downloads/Personas.csv",
                          ";", escape_double = FALSE, trim_ws = TRUE)


tabla <- data.frame(personas$p6800, personas$ingtotob)
head(tabla)
tail(tabla)

filas <- which( tabla[, 1] == NA | tabla[, 2] == 0)
tabla <- tabla[-filas,]
head(tabla)
na.omit(tabla[, 1])
tabla <- na.omit(tabla)
head(tabla)
tail(tabla)
hist(tabla[, 1], breaks = 100, main = "Histograma de la edad", xlab = "Edad",
     ylab = "Frecuencia")
filas1 <- which(tabla[, 1] < 18 | tabla[, 1] > 80)
tabla <- tabla[-filas1, ]
head(tabla)
hist(tabla[, 1], breaks = 100, main = "Histograma de la edad", xlab = "Edad",
     ylab = "Frecuencia")
hist(tabla[,2], breaks = 100, main = "Histograma del ingreso total", xlab = "Ingreso",
     ylab = "Frecuencia")
filas2 <- which(tabla[, 2] < 500000 | tabla[ ,2] > 5000000)
tabla <- tabla[-filas2, ]
hist(tabla[,2], breaks = 100, main = "Histograma del ingreso total", xlab = "Ingreso",
     ylab = "Frecuencia")
plot(tabla[, 1], tabla[, 2], pch = 16, col = "blue", xlab = "Edad", ylab =
       "Ingreso total")
abline(lm(tabla[,2] ~ tabla[, 1]), lwd = 2, col = "red") # Recta de tendencia
?cor.test
cor.test(tabla[, 1], tabla[, 2], method = "pearson")
cor.test(tabla[, 1], tabla[, 2], method = "spearman")
cor.test(tabla[, 1], tabla[, 2], method = "kendall")
install.packages("nortest")
library(nortest)

ad.test(tabla[, 1])
ad.test(tabla[, 2])
install.packages("testforDEP", dependencies=TRUE)
library(testforDEP)
length(tabla[,1])
testforDEP(x = tabla[, 1], y = tabla[, 2], test = "MIC", p.opt = "MC", num.MC = 1000)
?sample
filas3 <- sample(1:length(tabla[, 1]), size = 1000, replace = TRUE, prob = NULL)
muestra <- tabla[filas3, ]
head(muestra)
tail(muestra)
?testforDEP
library(Rcpp)
testforDEP(x = muestra[, 1], y = muestra[, 2], test = "MIC", p.opt = "MC",
           num.MC = 100)
testforDEP(x = muestra[, 1], y = muestra[, 2], test = "VEXLER", p.opt = "MC",
           num.MC = 100)
