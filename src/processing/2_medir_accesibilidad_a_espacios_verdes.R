## Estimando distancia desde cada radio censal Urbano hasta el espacio verde más cercano
library(tidyverse)
library(sf)
library(osrm)


# Cargamos radios censales urbanos usados para la EPH
# Retenemos sólo radios urbanos, en proyeccion Mercator como le gusta a OSRM
radios_ciudades <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    filter(tiporad == "U")

# Cargamos isocronas calculadas con el script src/2_medir_accesibilidad_a_espacios_verdes.R
isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_urbanos.shp", 
                     stringsAsFactors = FALSE)
