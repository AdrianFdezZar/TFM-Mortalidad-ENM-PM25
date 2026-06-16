####################################################
#GRÁFICOS DISTRIBUCIÓN MORTALIDAD QUINQUENIOS Y SEXO
####################################################
library(dplyr)
library(ggplot2)
library(patchwork) 

# 1. PREPARACIÓN DE DATOS Y GRÁFICO: SEXO

tabla_sexo <- df_final_v2 %>%
  group_by(sexo) %>%
  summarise(
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_muertes = (defunciones_totales / sum(defunciones_totales)) * 100
  )

grafico_sexo <- ggplot(data = tabla_sexo, aes(x = sexo, y = porcentaje_muertes)) +
  geom_col(fill = "#2c3e50", width = 0.5) + # Misma estética académica
  geom_text(aes(label = sprintf("%.1f%%", porcentaje_muertes)), vjust = -0.5, size = 4) + # Añade el % exacto encima
  theme_minimal(base_size = 12) +
  labs(
    subtitle = "Distribución por Sexo",
    x = "Sexo",
    y = "Porcentaje (%)"
  ) +
  # Fijamos el límite Y un poco más alto para que quepa el texto
  scale_y_continuous(limits = c(0, max(tabla_sexo$porcentaje_muertes) + 5)) +
  theme(
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )



# 2. PREPARACIÓN DE DATOS Y GRÁFICO: QUINQUENIOS (AÑO)

tabla_quinquenios <- df_final_v2 %>%
  group_by(año) %>%
  summarise(
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_muertes = (defunciones_totales / sum(defunciones_totales)) * 100
  )

grafico_quinquenios <- ggplot(data = tabla_quinquenios, aes(x = año, y = porcentaje_muertes)) +
  geom_col(fill = "#2c3e50", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje_muertes)), vjust = -0.5, size = 4) +
  theme_minimal(base_size = 12) +
  labs(
    subtitle = "Evolución por Periodos (Quinquenios)",
    x = "Periodo de estudio",
    y = "Porcentaje (%)"
  ) +
  scale_x_discrete(labels = c(
    "2003" = "2000-2004",
    "2007" = "2005-2009",
    "2012" = "2010-2014",
    "2017" = "2015-2019"
  )) +
  scale_y_continuous(limits = c(0, max(tabla_quinquenios$porcentaje_muertes) + 5)) +
  theme(
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )


# 3. UNIÓN DE GRÁFICOS Y EXPORTACIÓN (Con patchwork)

# Unimos los gráficos uno al lado del otro con el operador "+"
composicion_final <- grafico_sexo + grafico_quinquenios + 
  # Añadimos un título global que abarque ambos gráficos
  plot_annotation(
    title = "Caracterización de la mortalidad por EMN",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))
  )

# Guardamos la composición. Aumentamos el 'width' para que quepan bien los dos.
ggsave("output/composicion_mortalidad_sexo_tiempo.png", 
       plot = composicion_final, 
       width = 25,  # Más ancho para evitar que se achaten
       height = 12, 
       units = "cm", 
       dpi = 300)

# Para verlo en la consola de RStudio/Colab antes de exportar
print(composicion_final)

#================================================================
#Modelo estadístico Poisson básico
#================================================================
# Ejecutamos un Modelo Lineal Generalizado (GLM) con familia Poisson
modelo_poisson_base <- glm(
  mortalidad ~ pm25 + sexo + edad_supergrupo + año + offset(log(poblacion)),
  family = poisson(link = "log"),
  data = df_final_v2
)

# Imprimimos los resultados
summary(modelo_poisson_base)

(exp(0.151933)-1)*100 #(exp(Estimate)-1)*100 es el % de cuanto está más afectado una categoría respecto a la de referencia

#===============================================================
#Modelo regresión con estructura espacial:
#===============================================================
library(INLA)

df_final_inla <- df_final_v2 %>%
  rename(anio = año) %>%
  as.data.frame()

modelo_bym_1a <- inla(
  mortalidad ~
    pm25 +
    edad_supergrupo +
    sexo +
    anio +
    f(
      ID_numerico,
      model = "bym2",
      graph = "output/matriz_espacial.graph",
      scale.model = TRUE
    ),
  
  family = "poisson",
  E = poblacion,
  data = df_final_inla,
  
  verbose = TRUE
)

summary(modelo_bym_1a)

(exp( 0.025 )-1)*100 #(exp(Estimate)-1)*100 es el % de cuanto está más afectado una categoría respecto a la de referencia


saveRDS(modelo_bym_1a, "output/modelo_bym_1a.rds")


#Obtención del riesgo

riesgo_1a <- modelo_bym_1a$summary.random$ID_numerico

# Selecciono  las 8106 filas
# y transformo de escala logarítmica a RR
riesgo_municipal <- riesgo_1a[1:n_mun, ] %>%
  mutate(
    RR = exp(mean)
  )

#Calculo la Probabilidad posterior aproximada de que el RR municipal sea superior o inferior a 1.
riesgo_municipal <- riesgo_municipal %>%
  mutate(
    PP = pnorm(
      0,
      mean = mean,
      sd = sd,
      lower.tail = FALSE
    )
  )


riesgo_final <- mapa_limpio %>%
  left_join(
    riesgo_municipal,
    by = c("ID_numerico" = "ID")
  )


