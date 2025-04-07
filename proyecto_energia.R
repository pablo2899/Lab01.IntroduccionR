energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(120, 110, 115, NA, 105, 130, NA, 125, 135, 140,
             200, 195, NA, 210, 205, 220, 215, NA, 225, 230)
costo_kwh <- ifelse(energia == "Renovable", 0.12, 0.15)


mediana_renovable <- median(consumo[energia == "Renovable"], na.rm = TRUE)
mediana_norenovable <- median(consumo[energia == "No Renovable"], na.rm = TRUE)

consumo_limpio <- consumo
consumo_limpio[is.na(consumo) & energia == "Renovable"] <- mediana_renovable
consumo_limpio[is.na(consumo) & energia == "No Renovable"] <- mediana_norenovable


df_consumo <- data.frame(
  energia = energia,
  consumo = consumo_limpio,
  costo_kwh = costo_kwh
)


df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
costo_total_por_energia <- tapply(df_consumo$costo_total, df_consumo$energia, sum)
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

df_consumo$ganancia <- df_consumo$costo_total * 1.1


df_ordenado <- df_consumo[order(-df_consumo$costo_total), ]
top_3_costos <- head(df_ordenado, 3)

resumen_energia <- list(
  dataframe_ordenado = df_ordenado,
  consumo_total_por_tipo = total_consumo,
  costo_total_por_tipo = costo_total_por_energia,
  top_3_costos = top_3_costos
)


print(resumen_energia)
