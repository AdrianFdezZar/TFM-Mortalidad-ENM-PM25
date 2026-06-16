library(dplyr)
library(ggplot2)


########################################################
# 1. Reducir el nº de grupos de edad
########################################################
tabla_frecuencias_edad <- df_final %>%
  # Agrupamos por los 18 códigos de edad actuales
  group_by(edadgr) %>%
  summarise(
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    poblacion_total = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculamos el peso relativo de cada grupo
  mutate(
    porcentaje_muertes = (defunciones_totales / sum(defunciones_totales)) * 100,
    porcentaje_acumulado = cumsum(porcentaje_muertes)
  ) %>%
  arrange(edadgr)

# Mostrar la tabla en la consola para analizarla
print(tabla_frecuencias_edad, n = 18)
#Generar gráfico para ver la tendencia visualmente
grafico_mortalidad <- ggplot(data = tabla_frecuencias_edad, aes(x = factor(edadgr), y = porcentaje_muertes)) +
                        geom_col(fill = "#2c3e50", width = 0.7) +  # Un color azul oscuro/grisáceo muy académico
                        theme_minimal(base_size = 12) +            # Tema limpio y tamaño de letra adecuado
                        labs(
                        title = "Distribución porcentual de la mortalidad por ENM según tramos de edad",
                        subtitle = "Periodo 2000-2019 (18 grupos de edad originales)",
                        x = "Grupos de edad originales (1 al 18)",
                        y = "Porcentaje sobre el total de defunciones (%)"
                        ) +
                        theme(
                        plot.title = element_text(face = "bold", hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
                        panel.grid.minor = element_blank()
                        )

ggsave("output/grafico_mortalidad.png", 
       plot = grafico_mortalidad, 
       width = 20, 
       height = 12, 
       units = "cm", 
       dpi = 300)

#Creamos los nuevos grupos
df_final_v2 <- df_final %>%
  mutate(
    # Creamos los 5 supergrupos basados en los percentiles de tu tabla real
    edad_supergrupo = case_when(
      edadgr %in% 1:8 ~ "1_Menores_40",
      edadgr %in% 9:11 ~ "2_De_40_a_54",
      edadgr %in% 12:13 ~ "3_De_55_a_64",
      edadgr %in% 14:15 ~ "4_De_65_a_74",
      edadgr %in% 16:18 ~ "5_Mayores_75",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Re-agrupamos sumando para colapsar las 18 filas en solo 5 por municipio/año
  group_by(codigo_ine, ID_numerico, municipio_nombre, año, pm25, sexo, edad_supergrupo) %>%
  summarise(
    poblacion = sum(poblacion, na.rm = TRUE),
    mortalidad = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Limpiamos remanentes
  filter(poblacion > 0)


tabla_frecuencias_edad2 <- df_final_v2 %>%
  # Agrupamos por los 18 códigos de edad actuales
  group_by(edad_supergrupo) %>%
  summarise(
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    poblacion_total = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculamos el peso relativo de cada grupo
  mutate(
    porcentaje_muertes = (defunciones_totales / sum(defunciones_totales)) * 100,
    porcentaje_acumulado = cumsum(porcentaje_muertes)
  ) %>%
  arrange(edad_supergrupo)

grafico_mortalidad_v2 <- ggplot(data = tabla_frecuencias_edad2, aes(x = factor(edad_supergrupo), y = porcentaje_muertes)) +
  geom_col(fill = "#2c3e50", width = 0.7) +  # Un color azul oscuro/grisáceo muy académico
  theme_minimal(base_size = 12) +            # Tema limpio y tamaño de letra adecuado
  labs(
    title = "Distribución porcentual de la mortalidad por ENM según tramos de edad",
    subtitle = "Periodo 2000-2019 (5 grupos de edad nuevos)",
    x = "Grupos de edad nuevos",
    y = "Porcentaje sobre el total de defunciones (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank()
  )

ggsave("output/grafico_mortalidad_grupos_nuevos.png", 
       plot = grafico_mortalidad_v2, 
       width = 20, 
       height = 12, 
       units = "cm", 
       dpi = 300)
#################################################################
# 2. Eliminar el sexo "Ambos" (redundania)
################################################################
df_final_v2 <- df_final_v2 %>%
  # Nos quedamos EXCLUSIVAMENTE con estas dos categorías
  filter(sexo %in% c("Hombres", "Mujeres"))

# Comprobación de seguridad para confirmar que "Ambos" ha desaparecido
unique(df_final_v2$sexo)

##################################################################
# 3. Convertir a tipo factor la columna de sexo, edad y año
##################################################################
df_final_v2 <- df_final_v2 %>%
  mutate(
    # 1. FACTOR SEXO (Ponemos Mujeres como nivel base, y Hombres como segundo)
    sexo = factor(sexo, levels = c("Mujeres", "Hombres")),
    
    # 2. FACTOR AÑO (Los ordenamos cronológicamente. 2003 será el periodo base)
    año = factor(año, levels = c("2003", "2007", "2012", "2017")),
    
    # 3. FACTOR EDAD (Respetamos los 5 supergrupos que creamos, en orden vital)
    edad_supergrupo = factor(edad_supergrupo, levels = c(
      "1_Menores_40", 
      "2_De_40_a_54", 
      "3_De_55_a_64", 
      "4_De_65_a_74", 
      "5_Mayores_75"
    ))
  )

# Comprobación de seguridad: Preguntarle a R cómo entiende ahora el sexo
str(df_final_v2$sexo)

saveRDS(df_final_v2, "output/df_final_v2.rds")
