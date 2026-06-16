library(dplyr)
library(sf)
library(writexl)

# Buscamos los polígonos del mapa que se han quedado huérfanos de datos 
municipios_huerfanos <- mapa %>%
  # 1. Quitamos la geometría para que la comprobación sea instantánea
  st_drop_geometry() %>% 
  
  # 2. Seleccionamos el código y el nombre (OJO: cambia "nombre_mun" por 
  # el nombre real de la columna de texto que tenga tu shapefile 'mapa')
  select(cod_mun, NAMEUNIT) %>% 
  
  # 3. Nos quedamos SOLO con los que están en el mapa pero NO en df_final
  anti_join(df_final, by = c("cod_mun" = "codigo_ine")) %>%
  
  # 4. Filtramos para quitar de esta lista a los 25 segregados conocidos
  filter(!cod_mun %in% codigos_hijos_a_eliminar)

# Ver a los sospechosos
View(municipios_huerfanos)

saveRDS(municipios_huerfanos, "output/municipios_faltan.rds")
write_xlsx(municipios_huerfanos, "output/municipios_faltan.xlsx")

#COMPROBACIÓN 
nrow(mapa)
n_distinct(df_final$codigo_ine)+length(codigos_hijos_a_eliminar) + 4 #LOS QUE HEMOS DESCARGADO DATOS DEL INE
nrow(municipios_faltan)+n_distinct(df_final$codigo_ine)+length(codigos_hijos_a_eliminar)#Los que hay en "mapa"
