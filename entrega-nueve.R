library(readxl)
tempMedia2019 <- read_excel("/mnt/s/stiven/2021-2/UNAL/PEF/tempMedia2019.xlsx")

filas_nt <- which(tempMedia2019$ESTACIÓN == "NEVADO DEL TOLIMA")
filas_nt

temp_nt <- tempMedia2019[filas_nt, ]
temp_nt

temp_nt <- temp_nt[ , 8:(365 + 7)]
temp_nt

temp_nt <- as.matrix(temp_nt)
temp_nt
dim(temp_nt)

sin_datos <- which(temp_nt == 65535)
sin_datos

temp_nt <- as.vector(temp_nt)

sin_datos <- which(temp_nt == 65535)
sin_datos

temp_nt <- replace(temp_nt, sin_datos, NA)

#PUNTO 1
install.packages("imputeTS")
library(imputeTS)
temp_nt <- na.interpolation(temp_nt, option = "spline")
min(temp_nt)
max(temp_nt)
plot(temp_nt, type = "l", lwd = 2, col = "blue2") #Grafica con atipicos

filas_atipicas <- which(temp_nt > 6)
temp_nt <- replace(temp_nt, filas_atipicas, NA)
temp_nt
temp_nt <- na.interpolation(temp_nt, option = "linear")
plot(temp_nt, type = "l", lwd = 2, col = "blue2") #GRAFICA FINAL no presenta normalidad


#PUNTO 2
hist(temp_nt, breaks = c(-1, 0, 1, 2, 3, 4 ,5, 6), col = "blue")
hist(temp_nt, breaks = seq(-1, 6, by = 0.5), col = "blue", freq = F)
lines(density(temp_nt), lwd = 2, col = "red4")
lines(seq(-1, 6, by = 0.5), dnorm(seq(-1, 6, by = 0.5), mean = mean(temp_nt), sd = sd(temp_nt)), col = "red", lwd = 2)


#PUNTO 3
qqnorm(temp_nt, pch = 20)
qqline(temp_nt, lwd = 2, col = "green3") #No presenta normalidad


#PUNTO 4
shapiro.test(temp_nt) #p-value = 0.0003812 < 0.05 ---> Se rechaza Ho
ks.test(temp_nt, "pnorm") #p-value < 2.2e-16 < 0.05 ---> Se rechaza Ho
install.packages("nortest")
library(nortest)
ad.test(temp_nt) #p-value = 0.003613 < 0.05 ---> Se rechaza Ho


#PUNTO 5 
install.packages("boot", dep= TRUE)
library(boot)
media_aritmetica <- function(data, i){
  return(mean(data[i]))
}
media_aritmetica(temp_nt, i = c(1, 2, 365)) #Funcion que da la media
bootstrap_media <- boot(data = temp_nt, media_aritmetica, R= 365)
bootstrap_media

hist(bootstrap_media$t, breaks = 20, col = "blue")

hist(bootstrap_media$t, breaks = 20, col = "blue", freq = F)
lines(density(bootstrap_media$t), lwd = 2, col = "red4")
lines(seq(min(bootstrap_media$t), max(bootstrap_media$t), by = 0.01), 
      dnorm(seq(min(bootstrap_media$t), max(bootstrap_media$t), by = 0.01), 
            mean = mean(bootstrap_media$t), sd = sd(bootstrap_media$t)), col = "red", lwd = 2) #Grafica final si presenta normalidad


#PUNTO 6
qqnorm(bootstrap_media$t, pch =20)
qqline(bootstrap_media$t, lwd = 3, col = "green3") #presenta normalidad


#PUNTO 7
shapiro.test(bootstrap_media$t) #p-value = 0.4059 > 0.05 ---> NO se rechaza Ho
ks.test(bootstrap_media$t, "pnorm") #p-value < 2.2e-16 < 0.05 ---> Se rechaza Ho
install.packages("nortest")
library(nortest)
ad.test(bootstrap_media$t) #p-value = 0.3716 > 0.05 ---> NO se rechaza Ho 