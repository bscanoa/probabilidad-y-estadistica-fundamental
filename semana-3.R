library(readr)

Nacidos_2019 <- read_csv("/mnt/s/stiven/2021-2/UNAL/PEF/Semana 3/Nacidos_2019.csv", + col_types = cols(TIPO_PARTO = col_character(), + IDFACTORRH = col_character()))

# 3.1 Tabla de contingencia
View(Nacidos_2019)
barplot(table(Nacidos_2019$IDFACTORRH)) # Diagrama de barras Factor RH
barplot(table(Nacidos_2019$TIPO_PARTO)) # Diagrama de barras Tipo de Parto

filas_sin_info <- which(Nacidos_2019$IDFACTORRH == "9"
                        | Nacidos_2019$TIPO_PARTO == "9")  

tabla_inicial <- data.frame(Nacidos_2019$IDFACTORRH, Nacidos_2019$TIPO_PARTO)
tabla_filtrada <- tabla_inicial[-filas_sin_info, ]
tabla_contingencia <- table(tabla_filtrada)
tabla_contingencia

# 3.2 Gráficos de la tabla de contingencia
plot(tabla_contingencia)
library(vcd)
assoc(tabla_contingencia,
      shade=T,
      main="Asociación entre Tipo de Parto y factor RH",
      sub="Residuales de Pearson")


# 3.3 Probabilidad de que un recién nacido nazca por cesárea
total_datos <- sum(tabla_contingencia)
total_datos
pp_cesarea <- sum(tabla_contingencia[ , 2])
pp_cesarea
prob_cesarea <- pp_cesarea/total_datos
prob_cesarea
length(Nacidos_2019$TIPO_PARTO)

# 3.4 Probabilidad de que un recién nacido nazca con factor RH Negativo

rh_negativo <- sum(tabla_contingencia[2, ])
prob_rh_neg <- rh_negativo/total_datos
prob_rh_neg

# 3.5 Probabilidad de si un bebé tiene RH negativo entonces este
# nazca por parto espontáneo.
(15101/rh_negativo)


# 3.6
chisq.test(tabla_contingencia)

chisq.test()?
ji = chisq.test(tabla_contingencia)
ji_c = ji$statistic

total_datos <- sum(tabla_contingencia)
filas <- nrow(tabla_contingencia)
colum <- ncol(tabla_contingencia)

# 3.7 índice V de Kramer:
v_c = sqrt(ji_c/(total_datos*min((filas-1),(colum-1))))
v_c

# 3.8 índice de Karl-Pearson
k_p = sqrt(ji_c/(ji_c + total_datos))
k_p
sqrt(((min(nrow(tabla_contingencia), ncol(tabla_contingencia))) -1)/min(nrow(tabla_contingencia), ncol(tabla_contingencia)))
(0.7071068/5)*5

