library(tidyverse)

df_mort_limpio <- motoneurona %>%
  # 1. Rellenar el código de municipio con ceros a la izquierda 
  mutate(CODIGO_NUM = str_pad(CODIGO_NUM, width = 5, side = "left", pad = "0")) %>%
  
  # 2. Renombrar la columna ER
  rename(Ambos_sexos = ER) %>%
  
  # 3. Crear la columna quinquenio usando los rangos de años
  mutate(quinquenio = case_when(
    anodef %in% 2000:2004 ~ 1,
    anodef %in% 2005:2009 ~ 2,
    anodef %in% 2010:2014 ~ 3,
    anodef %in% 2015:2019 ~ 4,
    TRUE ~ NA_real_ # Si hay años fuera de estos rangos (ej. 1999), les pone NA
  )) %>%
  
  # (Opcional pero recomendado) Quitar las filas que no entran en ningún quinquenio (ej. 1999)
  filter(!is.na(quinquenio)) %>%
  
  # 4. Eliminar la columna original del año de defunción
  select(-anodef) %>%
  
  # 5. Agrupar por las variables clave: el nuevo quinquenio, municipio y edad
  group_by(quinquenio, CODIGO_NUM, edadgr) %>%
  
  # 6. Sumar (agregar) los datos de esos 5 años en una única fila por grupo
  summarise(
    hombres_ER = sum(hombres_ER, na.rm = TRUE),
    mujeres_ER = sum(mujeres_ER, na.rm = TRUE),
    Ambos_sexos = sum(Ambos_sexos, na.rm = TRUE),
    .groups = "drop" # Quita la agrupación interna para evitar errores futuros
  )



