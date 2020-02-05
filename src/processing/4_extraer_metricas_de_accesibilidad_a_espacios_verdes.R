########################################################
# Extraer métricas de accesibilidad a espacios verdes  #
########################################################

library(tidyverse)
library(sf)


radios_ciudades <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    filter(tiporad == "U") %>% 
    # creamos el identificador de RADIO como codprov + coddepto + frac2010
    mutate(RADIO = paste0(codprov, coddepto, frac2010, radio2010),
           id = as.numeric(id)) %>% 
    # Corregimos un error de tipeo en la data de origen ("Constit*i*ución")           
    mutate(eph_aglome = ifelse(eph_aglome == "San Nicolas - Villa Constitiución",
                             "San Nicolas - Villa Constitución",
                             eph_aglome)) %>% 
    # Retiramos el identificador censal que precede al nombre
    # lo cual ademas de hacer mas legible al nombre, combina a toda la CABA
    # en una sola "localidad" (de lo contrario, cada COMUNA tiene su propio idetificador) 
    mutate(localidade = str_replace(localidade, "\\(.*\\) ", "")) 

espacios_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp") %>% 
    mutate(area_m2 = as.numeric(st_area(.)))

accesibilidad  <- read_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando.csv") %>% 
    mutate(situacion = ifelse(total_ha > 0, "con_acceso", "sin_acceso"))

radios_deciles_NSE <- read_csv("data/processed/NSE/radios_urbanos_decil_NSE_por_aglomerado_EPH.csv")

#########################################
# Métrica tradicional: m2 por habitante #
#########################################


## Por aglomerado ##

# Disolvemos los radios urbanos en una "mancha" unificada para cada aglomerado
manchas_urbanas <- radios_ciudades %>%
    left_join(radios_deciles_NSE) %>% 
    group_by(eph_aglome) %>% 
    summarise(poblacion = sum(PERSONAS, na.rm = TRUE)) 
    
# Transformamos a la misma proyección que usa el shapefile de espacios verdes

manchas_urbanas <- st_transform(manchas_urbanas, crs = st_crs(espacios_verdes))


# Asignamos a cada mancha urbana los espacios verdes con los que intersecta,
# y calculamos m2 de verde por habitante

m2_hab_aglomerados <- manchas_urbanas %>% 
    st_join(espacios_verdes) %>% 
    st_set_geometry(NULL) %>% 
    group_by(eph_aglome) %>% 
    summarise(poblacion = last(poblacion),
              area_m2 = sum(area_m2),
              m2_hab = area_m2/poblacion)

# Guardamos los resultados

write_csv(m2_hab_aglomerados, "data/processed/metricas/m2_espacio_verde_por_habitante_aglomerados.csv")



## Por localidad ##

# Disolvemos los radios urbanos en una "mancha" unificada para cada localidad
manchas_localidades <- radios_ciudades %>% 
    # a partir de aquí es el mismo proceso que usamos para aglomerados
    left_join(radios_deciles_NSE) %>% 
    group_by(eph_aglome, localidade) %>% 
    summarise(poblacion = sum(PERSONAS, na.rm = TRUE)) %>% 
    ungroup()

# Transformamos a la misma proyección que usa el shapefile de espacios verdes

manchas_localidades <- st_transform(manchas_localidades, crs = st_crs(espacios_verdes))


# Asignamos a cada mancha urbana los espacios verdes con los que intersecta,
# y calculamos m2 de verde por habitante

m2_hab_localidades <- manchas_localidades %>% 
    st_join(espacios_verdes) %>% 
    st_set_geometry(NULL) %>% 
    group_by(eph_aglome, localidade) %>% 
    summarise(poblacion = last(poblacion),
              area_m2 = sum(area_m2),
              m2_hab = area_m2/poblacion)

# Guardamos los resultados

write_csv(m2_hab_localidades, "data/processed/metricas/m2_espacio_verde_por_habitante_localidades.csv")




########################################
# Métrica "sofisticada": accesibilidad #
########################################


# Juntamos todo

base_combinada <- radios_ciudades %>% 
    left_join(accesibilidad) %>% 
    left_join(radios_deciles_NSE) %>% 
    # quitamos informacion geo (hace mucho más rapido el procesamiento posterior)
    st_set_geometry(NULL) 


## Poblacion con acceso a espacios verdes (a 10 minutos de caminata)

## Por aglomerado ##

accesibilidad_espacios_verdes_aglomerados <- base_combinada %>% 
    group_by(eph_aglome, decil_NSE, situacion) %>% 
    summarise(poblacion = sum(PERSONAS)) %>% 
    pivot_wider(names_from = situacion, values_from = poblacion) %>% 
    # reemplazamos NA con 0, en los casos en los que nadie cumple con la situación
    replace_na(list(con_acceso = 0, sin_acceso = 0)) %>% 
    summarise(poblacion = sum(con_acceso, sin_acceso, na.rm = TRUE),
              tasa_acceso = (con_acceso / poblacion)) %>% 
    # Ahora calcular has accesibles 
    left_join(base_combinada %>% 
                  group_by(eph_aglome, decil_NSE) %>% 
                  summarise(total_ha_accesibles = sum(total_ha, na.rm = TRUE))) %>% 
    mutate(m2_accesibles_per_capita = (total_ha_accesibles / poblacion) * 10000)

# Guardamos los resultados

write_csv(accesibilidad_espacios_verdes_aglomerados, "data/processed/metricas/accesibilidad_espacios_verdes_aglomerados.csv")


## Por localidad ##

accesibilidad_espacios_verdes_localidades <- base_combinada %>% 
    group_by(eph_aglome, localidade, decil_NSE, situacion) %>% 
    summarise(poblacion = sum(PERSONAS)) %>% 
    pivot_wider(names_from = situacion, values_from = poblacion) %>% 
    # reemplazamos NA con 0, en los casos en los que nadie cumple con la situación
    replace_na(list(con_acceso = 0, sin_acceso = 0)) %>% 
    summarise(poblacion = sum(con_acceso, sin_acceso, na.rm = TRUE),
              tasa_acceso = (con_acceso / poblacion)) %>% 
    # Ahora calcular has accesibles 
    left_join(base_combinada %>% 
                  group_by(eph_aglome, localidade, decil_NSE) %>% 
                  summarise(total_ha_accesibles = sum(total_ha, na.rm = TRUE))) %>% 
    mutate(m2_accesibles_per_capita = (total_ha_accesibles / poblacion) * 10000)

# Guardamos los resultados

write_csv(accesibilidad_espacios_verdes_localidades, "data/processed/metricas/accesibilidad_espacios_verdes_localidades.csv")
