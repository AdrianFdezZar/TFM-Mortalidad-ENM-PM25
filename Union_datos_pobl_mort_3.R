# ==========================================
# UNIÓN DE POBLACIÓN Y MORTALIDAD
# ==========================================
library(dplyr)
df_unido <- df_poblacion_final %>%
  # 1. Crear la correspondencia entre 'año' y 'quinquenio' para el Join
  mutate(
    quinquenio = case_when(
      año == "2003" ~ 1,
      año == "2007" ~ 2,
      año == "2012" ~ 3,
      año == "2017" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 2. Hacer el cruce (Left Join) con la tabla de mortalidad
  # Mantenemos todas las filas de población y le pegamos las defunciones que coincidan
  left_join(
    df_mort_limpio, 
    by = c(
      "codigo_ine" = "CODIGO_NUM", 
      "edadgr" = "edadgr", 
      "quinquenio" = "quinquenio"
    )
  ) %>%
  
  # 3. Asignar el dato de mortalidad en función de la columna 'sexo'
  mutate(
    mortalidad = case_when(
      sexo == "Ambos" ~ Ambos_sexos,
      sexo == "Hombres" ~ hombres_ER,
      sexo == "Mujeres" ~ mujeres_ER,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 4. Los NA en mortalidad significan que hubo 0 muertes en ese cruce
  mutate(mortalidad = replace_na(mortalidad, 0)) %>%
  
  # 5. Limpieza: Eliminar las columnas de mortalidad originales y el quinquenio auxiliar
  select(
    codigo_ine, 
    municipio_nombre, 
    año, 
    sexo, 
    edadgr, 
    poblacion, 
    mortalidad
  )

# Revisar cómo ha quedado
head(df_unido)

print(colSums(is.na(df_unido))) #Para comprobar que los na están solo en "poblacion"


