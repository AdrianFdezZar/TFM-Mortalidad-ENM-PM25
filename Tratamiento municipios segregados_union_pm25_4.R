library(writexl)
library(dplyr)
library(sf)
# 1. Identificar los códigos INE de los municipios que tienen NA en población
codigos_problematicos <- df_unido %>%
  filter(is.na(poblacion)) %>%
  pull(codigo_ine) %>%
  unique()

# 2. Crear la tabla resumen
tabla_decision <- df_unido %>%
  # Nos quedamos solo con los problemáticos
  filter(codigo_ine %in% codigos_problematicos) %>%
  
  # Agrupamos por municipio y año para sumar totales
  group_by(codigo_ine, municipio_nombre, año) %>%
  summarise(
    # Si todo es NA (no existía el municipio), dejamos NA. Si hay datos, sumamos.
    poblacion_total = if(all(is.na(poblacion))) NA_real_ else sum(poblacion, na.rm = TRUE),
    
    # Sumamos las defunciones de todos los grupos de edad y sexos en ese año/quinquenio
    defunciones_totales = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # 3. Pivotar a lo ancho para poder comparar fácilmente los años de un vistazo
  pivot_wider(
    names_from = año,
    values_from = c(poblacion_total, defunciones_totales),
    names_glue = "{.value}_{año}" # Esto creará nombres como poblacion_total_2003
  ) %>%
  
  # 4. Ordenar las columnas para tener Año a Año (Población y Mortalidad juntas)
  select(
    codigo_ine, municipio_nombre,
    poblacion_total_2003, defunciones_totales_2003,
    poblacion_total_2007, defunciones_totales_2007,
    poblacion_total_2012, defunciones_totales_2012,
    poblacion_total_2017, defunciones_totales_2017
  )

# 5. Abrir la tabla en el visor de RStudio para analizarla cómodamente
View(tabla_decision)

#write_xlsx(tabla_decision,"output/municipios_modificados.xlsx")

#6. Eliminar segregados y quedarnos con el municipio fusionado

# 6.1. Tratamiento de las FUSIONES (Suma de municipios en todos los periodos)
df_arreglado <- df_unido %>%
  mutate(
    # Reasignamos el código INE de los antiguos al nuevo municipio fusionado
    codigo_ine_nuevo = case_when(
      codigo_ine %in% c("15026", "15063") ~ "15902", # Cesuras + Oza dos Ríos -> Oza-Cesuras
      codigo_ine %in% c("36011", "36012") ~ "36902", # Cerdedo + Cotobade -> Cerdedo-Cotobade
      TRUE ~ codigo_ine
    ),
    # Reasignamos el nombre para que quede limpio
    municipio_nombre_nuevo = case_when(
      codigo_ine_nuevo == "15902" ~ "Oza-Cesuras",
      codigo_ine_nuevo == "36902" ~ "Cerdedo-Cotobade",
      TRUE ~ municipio_nombre
    )
  ) %>%
  # Agrupamos por los nuevos identificadores, para que R sume los datos de 2003 y 2007
  group_by(codigo_ine = codigo_ine_nuevo, municipio_nombre = municipio_nombre_nuevo, año, sexo, edadgr) %>%
  summarise(
    poblacion = sum(poblacion, na.rm = TRUE),
    mortalidad = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  )


# 6.2. Tratamiento de las SEGREGACIONES (Excluir solo los municipios nuevos)
# Aquí metemos ÚNICAMENTE los códigos de los pueblos que se han independizado, 
# dejando a los municipios "padre" (Zaragoza, Tarragona, etc.) intactos en la tabla.
codigos_hijos_a_eliminar <- c(
  "04904", # Balanegra
  "06903", # Guadiana
  "10902", # Vegaviana
  "10903", # Alagón del Río
  "10904", # Tiétar
  "10905", # Pueblonuevo de Miramontes
  "11903", # San Martín del Tesorillo
  "14901", # Fuente Carreteros
  "14902", # La Guijarrosa
  "18065", # Dehesas Viejas
  "18077", # Fornes
  "18106", # Játar
  "18914", # Valderrubio 
  "18915", # Domingo Pérez de Granada 
  "18916", # Torrenueva Costa 
  "21902", # La Zarza-Perrunal 
  "29902", # Villanueva de la Concepción
  "29903", # Montecorto
  "29904", # Serrato
  "38901", # El Pinar de El Hierro
  "41904", # El Palmar de Troya
  "43907", # La Canonja 
  "46904", # Benicull de Xúquer
  "48915", # Ziortza-Bolibar
  "50903"  # Villamayor de Gállego 
)
# "48916" )# Usansolo Independizado en 2022 no aparece en datos INE, sí en shp IGN(Lo quito)


# Filtramos para que se queden todos MENOS los de esa lista
df_final_modelado <- df_arreglado %>%
  filter(!codigo_ine %in% codigos_hijos_a_eliminar)

# 6.3. Ordenación final exigida para la matriz de adyacencias
df_final_modelado <- df_final_modelado %>%
  arrange(codigo_ine, año, sexo, edadgr)

# Verificar el resultado
head(df_final_modelado)

#COMPROBACIÓN FINAL
df_comprobacion <-df_final_modelado|>
                    filter(codigo_ine %in% codigos_hijos_a_eliminar)
view(df_comprobacion)

df_comprobacion<-df_final_modelado|>
  select(codigo_ine, municipio_nombre)|>
  filter(codigo_ine %in% c("15026", "15063", "15902", "36011", "36012", "36902"))|>
  distinct()
         
view(df_comprobacion)

#######################################################
#AQUÍ YA DEJAMOS UNIDOS LOS DATOS DE PM25 AL DF FINAL##
#######################################################
# 1. Leer el shapefile 
mapa <- st_read("C:/Users/adrian.fernandezz/Desktop/TFM/Datos/Limites_+_contaminación/Limites_+_contaminación/Limites_+_contaminación.shp")


# 2. Extraemos solo las columnas de contaminación que nos interesan de 'mapa'
df_contaminacion <- mapa %>%
  st_drop_geometry() %>% # Quita los polígonos para que sea una tabla normal y ligera
  select(cod_mun,pm25_00_04, pm25_05_09, pm25_10_14, pm25_15_19)

# 3. Unimos y asignamos el valor de pm25 según el año
df_final <- df_final_modelado %>%
  
  # Hacemos el cruce: a cada municipio le pegamos sus 4 columnas de PM2.5
  left_join(df_contaminacion, by = c("codigo_ine" = "cod_mun")) %>%
  
  # Creamos la columna definitiva "pm25" evaluando el año de cada fila
  mutate(
    pm25 = case_when(
      año == "2003" ~ pm25_00_04,
      año == "2007" ~ pm25_05_09,
      año == "2012" ~ pm25_10_14,
      año == "2017" ~ pm25_15_19,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Limpieza: Borramos las 4 columnas de periodos porque ya hemos rescatado el dato útil
  select(-pm25_00_04, -pm25_05_09, -pm25_10_14, -pm25_15_19)

#Exportamos
saveRDS(df_final, "output/df_final.rds")
