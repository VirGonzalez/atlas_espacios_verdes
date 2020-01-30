## Verificando accesibilidad desde cada radio censal Urbano hasta el espacio verde más cercano
library(tidyverse)
library(sf)


# Cargamos espacios verdes
espacios_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp") %>% 
    mutate(ha = as.numeric(st_area(.))/10000)

# Cargamos isocronas calculadas con el script src/1_estimar_distancia_a_espacios_verdes.R
isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_urbanos.shp", 
                     stringsAsFactors = FALSE)

# Unificamos proyecciones
isocronas <- st_transform(isocronas, st_crs(espacios_verdes))

# Identificamos cantidad y area total de los espacios verdes dentro de la cobertura de cada isocrona

accesibilidad <- st_join(isocronas, espacios_verdes) %>% 
    group_by(id = id.x) %>% 
    summarise(n = n(),
              total_ha = sum(ha, na.rm = TRUE)) %>% 
    mutate(n = ifelse(total_ha == 0, 0, n)) %>% 
    st_set_geometry(NULL)

# Guardamos resultados

accesibilidad  %>% 
    write_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando.csv")
    
