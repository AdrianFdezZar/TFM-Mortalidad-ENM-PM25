library(sf)
library(dplyr)
library(spdep)
library(stringr)

#1. Borrar de "mapa" las entidades que el INE no da datos, Usansolo y todos los condominios y los municipios excluidos de estudio (segregados)
mapa_limpio <- mapa %>%
  filter(
    # 1. Fuera los municipios segregados de tu lista oficial
    !cod_mun %in% codigos_hijos_a_eliminar, 
    
    # 2. Fuera Usansolo (independizado en 2022)
    cod_mun != "48916", 
    
    # 3. Fuera todos los condominios y parzonerías (códigos del IGN)
    !str_starts(cod_mun, "53") 
  )
nrow(mapa_limpio)
n_distinct(df_final$codigo_ine)

# 2. Ordenar por Código INE y crear el ID numérico
mapa_limpio <- mapa_limpio %>%
  arrange(cod_mun) %>%
  mutate(ID_numerico = row_number()) # Crea la secuencia 1, 2, 3... hasta 8000+

#2.1 Darle a df_final el campo de ID_numerico
##2.1.1. Aislamos el diccionario espacial de la geometría
diccionario_espacial <- mapa_limpio %>%
  st_drop_geometry() %>%                   # Fundamental: quitamos los polígonos
  select(cod_mun, ID_numerico)             # Nos quedamos solo con las llaves de cruce

## 2.1.2. Inyectamos el ID_numerico en tu tabla final
df_final <- df_final %>%
  left_join(diccionario_espacial, by = c("codigo_ine" = "cod_mun"))

## 2.1.3. Auditoría de calidad (Comprobamos si algún municipio se ha quedado sin ID)
sum(is.na(df_final$ID_numerico))
saveRDS(df_final, "output/df_final.rds")

# 3. Comprobación de que está en orden
head(mapa_limpio %>% select(cod_mun, ID_numerico))
# 4. Crear la matriz usando poly2nb (usamos contigüidad tipo "Queen" por defecto)
# Esto evalúa qué polígonos se tocan.
matriz_vecinos <- poly2nb(mapa_limpio, row.names = mapa_limpio$ID_numerico)



# 5. Identificar los municipios aislados (islas, Ceuta, Melilla...)
# La función card() cuenta el número de vecinos que tiene cada índice
aislados <- which(card(matriz_vecinos) == 0)

# Ver cuáles son esos municipios para apuntar su ID_numerico
municipios_aislados <- mapa_limpio[aislados, ]
print(municipios_aislados %>% select(ID_numerico, cod_mun, NAMEUNIT)) 

# 6. Buscar los ID numéricos concretos para unirlos
ids_numericos <- mapa_limpio |>
                  select(cod_mun, NAMEUNIT, ID_numerico)|>
                  filter(ID_numerico %in% aislados)
view(ids_numericos)

id_ilha_arousa <- 5325
id_vilanova_arousa <- 5324
  
id_ceuta <- 8105
id_algeciras <- 1765
  
id_formentera <- 817
id_ibiza <- 819

id_llivia <- 2537
id_puigcerda <- 2578
  
id_melilla <- 8106
id_malaga <- 4520

id_hacinas <- 1292
id_salas_infantes <- 1422
id_monasterio_sierra <- 1343

# FUNCIÓN: Unir Ceuta y Algeciras
# Como Ceuta es una isla (tenía 0 vecinos), su valor en la lista es 0. Lo sustituimos:
matriz_vecinos[[id_ceuta]] <- as.integer(id_algeciras)
# Como Algeciras ya tenía vecinos, AÑADIMOS a Ceuta a su lista y ordenamos de menor a mayor
matriz_vecinos[[id_algeciras]] <- sort(as.integer(c(matriz_vecinos[[id_algeciras]], id_ceuta)))

# Repetimos para Ilha de Arousa y Vilanova de Arousa
matriz_vecinos[[id_ilha_arousa]] <- as.integer(id_vilanova_arousa)
matriz_vecinos[[id_vilanova_arousa]] <- sort(as.integer(c(matriz_vecinos[[id_vilanova_arousa]], id_ilha_arousa)))

# Repetimos para Formentera e Ibiza
matriz_vecinos[[id_formentera]] <- as.integer(id_ibiza)
matriz_vecinos[[id_ibiza]] <- sort(as.integer(c(matriz_vecinos[[id_ibiza]], id_formentera)))

# Repetimos para Llivia y Puigcerda
matriz_vecinos[[id_llivia]] <- as.integer(id_puigcerda)
matriz_vecinos[[id_puigcerda]] <- sort(as.integer(c(matriz_vecinos[[id_puigcerda]], id_llivia)))

# Repetimos para Melilla y Malaga
matriz_vecinos[[id_melilla]] <- as.integer(id_malaga)
matriz_vecinos[[id_malaga]] <- sort(as.integer(c(matriz_vecinos[[id_malaga]], id_melilla)))

# Repetimos para Hacinas y Salas de los infantes
matriz_vecinos[[id_hacinas]] <- as.integer(id_salas_infantes)
matriz_vecinos[[id_salas_infantes]] <- sort(as.integer(c(matriz_vecinos[[id_salas_infantes]], id_hacinas)))

# Repetimos para Monasterio de la Sierra y Salas de los infantes
matriz_vecinos[[id_monasterio_sierra]] <- as.integer(id_salas_infantes)
matriz_vecinos[[id_salas_infantes]] <- sort(as.integer(c(matriz_vecinos[[id_salas_infantes]], id_monasterio_sierra)))

# Comprobación de que ya no hay municipios aislados
# Si esto da 0, significa que todos los municipios tienen al menos 1 vecino. ¡Misión cumplida!
sum(card(matriz_vecinos) == 0)

# Exportamos la matriz al formato .graph y rds
nb2INLA("output/matriz_espacial.graph", matriz_vecinos)
saveRDS(matriz_vecinos, "output/matriz_adyacencia.rds")

