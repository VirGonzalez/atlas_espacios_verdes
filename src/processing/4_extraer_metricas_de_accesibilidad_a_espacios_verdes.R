########################################################
# Extraer métricas de accesibilidad a espacios verdes  #
########################################################

library(tidyverse)
library(sf)


radios_ciudades <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    filter(tiporad == "U") %>% 
    # creamos el identificador de RADIO como codprov + coddepto + frac2010
    mutate(RADIO = paste0(codprov, coddepto, frac2010, radio2010),
           id = as.numeric(id))

espacios_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp")

accesibilidad  <- read_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando.csv") %>% 
    mutate(situacion = ifelse(total_ha > 0, "con_acceso", "sin_acceso"))

radios_deciles_NSE <- read_csv("data/processed/NSE/radios_urbanos_decil_NSE_por_aglomerado_EPH.csv")

#########################################
# Métrica tradicional: m2 por habitante #
#########################################



# Por aglomerado





# Juntamos todo

base_combinada <- radios_ciudades %>% 
    left_join(accesibilidad) %>% 
    left_join(radios_deciles_NSE) %>% 
    # quitamos informacion geo (hace mucho más rapido el procesamiento posterior)
    st_set_geometry(NULL) %>% 


## Poblacion con acceso a espacios verdes (a 10 minutos de caminata)

# A escala aglomerado

base_combinada %>% 
    group_by(eph_aglome, decil_NSE, situacion) %>% 
    summarise(poblacion = sum(PERSONAS)) %>% 
    complete(aglomerado, decil_NSE, situacion, fill = list(poblacion = 0)) %>% 
    pivot_wider(names_from = situacion, values_from = poblacion) %>% 
    summarise(poblacion = sum(con_acceso, sin_acceso, na.rm = TRUE),
              tasa_acceso = (con_acceso / poblacion)) %>% 
    # Ahora calcular has accesibles 
    left_join(radios_ciudades %>% 
                  group_by(aglomerado, decil_NSE) %>% 
                  summarise(total_ha_accesibles = sum(total_ha, na.rm = TRUE))) %>% 
    mutate(ha_accesibles_per_capita = total_ha_accesibles / poblacion,
           m2_accesibles_per_capita = (total_ha_accesibles / poblacion) * 10000)

