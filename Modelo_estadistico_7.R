####################################################
#GRÁFICOS DISTRIBUCIÓN MORTALIDAD QUINQUENIOS Y SEXO
####################################################
library(dplyr)
library(ggplot2)
library(patchwork) 

# 1. PREPARACIÓN DE DATOS Y GRÁFICO: SEXO (Ahora con 'N' absoluto)

tabla_sexo <- df_final_v2 %>%
  group_by(sexo) %>%
  summarise(
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  )

grafico_sexo <- ggplot(data = tabla_sexo, aes(x = sexo, y = defunciones_totales)) +
  geom_col(fill = "#2c3e50", width = 0.5) +
  # Imprimimos la 'N' absoluta encima de la barra (usamos format para separar los miles con punto)
  geom_text(aes(label = format(defunciones_totales, big.mark = ".", scientific = FALSE)), 
            vjust = -0.5, size = 4) + 
  theme_minimal(base_size = 12) +
  labs(
    subtitle = "Distribución por Sexo",
    x = "Sexo",
    y = "Nº de Defunciones (N)"
  ) +
  # Damos un 10% de margen arriba para que el texto no se corte
  scale_y_continuous(limits = c(0, max(tabla_sexo$defunciones_totales) * 1.1)) +
  theme(
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )


# 2. PREPARACIÓN DE DATOS Y GRÁFICO: QUINQUENIOS (Con 'N' y Línea de Tasa)

tabla_quinquenios <- df_final_v2 %>%
  group_by(año) %>%
  summarise(
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    poblacion_total = sum(poblacion, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    # Calculamos la Tasa de Mortalidad por cada 100.000 habitantes
    tasa_mortalidad = (defunciones_totales / poblacion_total) * 100000 
  )

# El nuevo coeff relaciona las Defunciones Totales máximas con la Tasa máxima
coeff <- max(tabla_quinquenios$defunciones_totales) / max(tabla_quinquenios$tasa_mortalidad)

grafico_quinquenios <- ggplot(data = tabla_quinquenios, aes(x = año)) +
  # Barras con la 'N' absoluta
  geom_col(aes(y = defunciones_totales), fill = "#2c3e50", width = 0.6) +
  
  # Línea de tendencia de la Tasa (La dibujamos ANTES de la etiqueta para que quede al fondo)
  geom_line(aes(y = tasa_mortalidad * coeff, group = 1), color = "#c0392b", linewidth = 1.2) +
  geom_point(aes(y = tasa_mortalidad * coeff), color = "#c0392b", size = 3) +
      geom_label(aes(y = defunciones_totales, 
                 label = format(defunciones_totales, big.mark = ".", scientific = FALSE)), 
             vjust = -0.8, 
             size = 4, 
             fill = alpha("white", 0.85), 
             linewidth = 0, # <--- La corrección está aquí
             label.padding = unit(0.2, "lines")) + # Margen interno del cajetín
  
  theme_minimal(base_size = 12) +
  labs(
    subtitle = "Evolución por Periodos (Quinquenios)",
    x = "Periodo de estudio",
    y = "Nº de Defunciones (N)"
  ) +
  scale_x_discrete(labels = c(
    "2003" = "2000-2004",
    "2007" = "2005-2009",
    "2012" = "2010-2014",
    "2017" = "2015-2019"
  )) +
  scale_y_continuous(
    # NUEVO: Damos un 15% de margen arriba para que quepa bien la etiqueta subida
    limits = c(0, max(tabla_quinquenios$defunciones_totales) * 1.15),
    sec.axis = sec_axis(~ . / coeff, name = "Tasa (por 100.000 hab.)") 
  ) +
  theme(
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.y.right = element_text(color = "#c0392b", face = "bold"),
    axis.text.y.right = element_text(color = "#c0392b")
  )


# 3. UNIÓN DE GRÁFICOS Y EXPORTACIÓN

composicion_final <- grafico_sexo + grafico_quinquenios + 
  plot_annotation(
    title = "Caracterización de la mortalidad por ENM", 
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))
  )

ggsave("output/composicion_mortalidad_sexo_tiempo.png", 
       plot = composicion_final, 
       width = 25, 
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

(exp(0.205124)-1)*100 #(exp(Estimate)-1)*100 es el % de cuanto está más afectado una categoría respecto a la de referencia

#===============================================================
#Modelo regresión con estructura espacial:
#===============================================================
library(INLA)

# 1. Limpiamos la RAM antes de empezar
gc()

# 2. CORRECCIÓN POSIBLES FUENTES DE ERROR
df_final_inla <- df_final_v2 %>%
  rename(anio = año) %>%
  as.data.frame()

# 3. MODELO ADAPTADO PARA POCA RAM
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
  
  control.inla = list(
    strategy = "simplified.laplace", 
    int.strategy = "eb"              
  ),
  num.threads = "1:1",               
  
  verbose = TRUE
)

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

riesgo_final <- riesgo_final %>%
  mutate(
    categoria = case_when(
      PP > 0.90 ~ "Muy alto",
      PP > 0.80 ~ "Alto",
      PP >= 0.20 ~ "Normal",
      PP >= 0.10 ~ "Bajo",
      TRUE ~ "Muy bajo"
    )
  )

