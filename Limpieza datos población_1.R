library(tidyverse)
library(readxl)
library(writexl)

# 1. Leer el archivo (sin skip, asumiendo que la primera fila tiene los encabezados reales)
df_raw <- read_excel("data/Poblacion_2003_2022.xlsx") 

# ==========================================
# FASE 1: LIMPIEZA DE IDENTIFICADORES Y FILAS
# ==========================================
df_clean <- df_raw %>%
  rename(col_original = Municipio) %>% 
  mutate(
    # Extraer identificadores básicos
    codigo_ine = str_extract(col_original, "^\\d{5}"),
    municipio_nombre = if_else(!is.na(codigo_ine), str_trim(str_remove(col_original, "^\\d{5}")), NA_character_),
    
    # Lógica de Sexo
    sexo_label = case_when(
      str_detect(col_original, "Hombres") ~ "Hombres",
      str_detect(col_original, "Mujeres") ~ "Mujeres",
      TRUE ~ NA_character_
    )
  ) %>%
  # Rellenar hacia abajo
  fill(codigo_ine, municipio_nombre, .direction = "down") %>%
  fill(sexo_label, .direction = "down") %>%
  mutate(sexo_label = replace_na(sexo_label, "Ambos")) %>%
  
  # Quedarnos solo con las filas de fechas reales
  filter(str_detect(col_original, "1 de enero")) %>%
  mutate(año = str_extract(col_original, "\\d{4}")) %>%
  
  # Seleccionar y reordenar (EXCLUIMOS la columna "Todas las edades" para que no moleste luego)
  select(codigo_ine, municipio_nombre, año, sexo = sexo_label, everything(), -col_original, -`Todas las edades`)

# ==========================================
# FASE 2: AGRUPAR EDADES AVANZADAS (85+)
# ==========================================
df_poblacion_agrupada <- df_clean %>%
  mutate(
    # Creamos la nueva columna sumando las 4 categorías finales
    `85 años y más` = rowSums(across(c(
      `De 85 a 89 años`, 
      `De 90 a 94 años`, 
      `De 95 a 99 años`, 
      `100 y más años`
    )), na.rm = TRUE)
  ) %>%
  # Borramos las originales de >85
  select(
    -`De 85 a 89 años`, 
    -`De 90 a 94 años`, 
    -`De 95 a 99 años`, 
    -`100 y más años`
  )

# ==========================================
# FASE 3: PIVOTAR Y CREAR CÓDIGO DE EDAD
# ==========================================
df_poblacion_larga <- df_poblacion_agrupada %>%
  
  # Pivotar de ancho a largo (dejando intactas nuestras 4 columnas clave)
  pivot_longer(
    cols = -c(codigo_ine, municipio_nombre, año, sexo), 
    names_to = "grupo_edad_texto", 
    values_to = "poblacion"        
  ) %>%
  
  # Traducir el texto al código numérico (del 1 al 18)
  mutate(
    edadgr = case_when(
      str_detect(grupo_edad_texto, "De 0 a 4") ~ 1,
      str_detect(grupo_edad_texto, "De 5 a 9") ~ 2,
      str_detect(grupo_edad_texto, "De 10 a 14") ~ 3,
      str_detect(grupo_edad_texto, "De 15 a 19") ~ 4,
      str_detect(grupo_edad_texto, "De 20 a 24") ~ 5,
      str_detect(grupo_edad_texto, "De 25 a 29") ~ 6,
      str_detect(grupo_edad_texto, "De 30 a 34") ~ 7,
      str_detect(grupo_edad_texto, "De 35 a 39") ~ 8,
      str_detect(grupo_edad_texto, "De 40 a 44") ~ 9,
      str_detect(grupo_edad_texto, "De 45 a 49") ~ 10,
      str_detect(grupo_edad_texto, "De 50 a 54") ~ 11,
      str_detect(grupo_edad_texto, "De 55 a 59") ~ 12,
      str_detect(grupo_edad_texto, "De 60 a 64") ~ 13,
      str_detect(grupo_edad_texto, "De 65 a 69") ~ 14,
      str_detect(grupo_edad_texto, "De 70 a 74") ~ 15,
      str_detect(grupo_edad_texto, "De 75 a 79") ~ 16,
      str_detect(grupo_edad_texto, "De 80 a 84") ~ 17,
      str_detect(grupo_edad_texto, "85") ~ 18,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Limpiar, organizar y ordenar
  select(codigo_ine, municipio_nombre, año, sexo, edadgr, poblacion) %>%
  arrange(codigo_ine, año, sexo, edadgr)

# ==========================================
# FASE 4: FILTRADOS FINALES
# ==========================================
# 1. Limpiar NA's en las columnas clave
df_poblacion_larga <- df_poblacion_larga %>% 
  drop_na(codigo_ine, edadgr)

# 2. Quedarnos solo con los años centrales de los quinquenios
df_poblacion_final <- df_poblacion_larga %>% 
  filter(año %in% c("2003", "2007", "2012", "2017"))

# Comprobación final
print(colSums(is.na(df_poblacion_final)))
print(unique(df_poblacion_final$año))

# ==========================================
# FASE 5: comprobar NA's en la columna de población
# ==========================================
df_poblacion_na <- df_poblacion_final %>% 
  filter(is.na(poblacion))
print(unique(df_poblacion_na$municipio_nombre))

