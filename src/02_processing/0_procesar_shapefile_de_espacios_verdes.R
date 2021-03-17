# Descargamos el export de todos los datos que tiene OSM para la Argentina, via GEOFABRIK: https://download.geofabrik.de/south-america/argentina.html
# El shapefile procesado fue obtenido de https://download.geofabrik.de/south-america/argentina-latest-free.shp.zip


## Carga de datos
library(sf)
library(tidyverse)

areas_verdes <- st_read("data/raw/OSM/gis_osm_landuse_a_free_1.shp", 
                        stringsAsFactors = F) %>%
    filter(fclass %in% c("nature_reserve", "park")) %>% 
    select(-code)

## Retenemos sólo espacios mayores a un umbral de corte
# descartamos los espacios con áreas menores a 1000 m2 (más pequeños que una plazoleta, aproximadamente)
umbral_descarte_m2 <- 1000

# Ante todo, proyección equiareal
areas_verdes <- areas_verdes %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

areas_verdes <- areas_verdes %>% # Pasamos a proyección equiareal para una medición precisa de áreas
    filter(as.numeric(st_area(.)) > umbral_descarte_m2) # Descartamos áreas menores al umbral


## Retirar Grandes Parques Nacionales del dataset


# Entre las áreas de categoría "nature_reserve" la mayoría de los polígonos representan 
# Parques Nacionales, y diversos territorios protegidos (biomas marinos, de alta montaña, humedales, etc) 
# que no pueden ser considerados "espacios verdes" en el sentido de opciones cotidianas de recreación 
# para la población urbana. Por otro lado, otros casos si corresponden a la categoría de espacios 
# accesibles de recreación: el Parque Pereyra Iraola, la Reserva Ecológica porteña, etc.


# Se prepara una capa para inspección visual
# check <- areas_verdes %>%
#     filter(fclass == "nature_reserve") %>%
#     mutate(area = as.numeric(st_area(.))) %>%
#     arrange(area)

#Tras la inspección, se categoriza cómo areas verdes con accesibilidad urbana a las de los índices 
# "612333451", "79291703", "255963872", "550726747", "220997430", "229796895", 
# "191347957", "90264216", "7322563", "51185722", "3810531", "3642306", 
# "350935744", "49772911", "5539208", "46945607", "185329483", "10343154"
# Retenemos esos, y descartamos los demas.

keep_ids <- c("612333451", "79291703", "255963872", "550726747", "220997430", "229796895", 
         "191347957", "90264216", "7322563", "51185722", "3810531", "3642306", 
         "350935744", "49772911", "5539208", "46945607", "185329483", "10343154")

areas_verdes <- areas_verdes %>% 
    filter(fclass != "nature_reserve" | osm_id %in% keep_ids) 

# Al 17/01/2018, hay varias reservas que figuran con categoría "park"
# son
# 729375334, 725714157, 205645634, 375780730
# "Reserva Natural Humedal Caleta Olivia", "Reserva Natural El Destino", 
# "Reserva Natural", "Reserva natural Abayubá"

areas_verdes <- areas_verdes %>% 
    filter(!(osm_id %in% c(729375334, 725714157, 205645634, 375780730))) 


    
## Retirar vias de circulación, boulevares, etc
# Existen una gran cantidad de calles, que por error o por tener canteros o areas parquizadas, 
# figuran con la categoría "park"
# Podemos detectar calles y otros elementos estrechos y largos comparando su área con su perímetro
# Tras inspección visual notamos que el número "mágico" parece ser 8.3 
# Ratios menores corresponden a boulevares  

areas_verdes <- areas_verdes %>% 
    filter((as.numeric(st_area(.)) / as.numeric(lwgeom::st_perimeter(.))) > 8.3)


## Combinar las áreas que estan muy próximas entre si
# (a menos de 10m en este caso)
# con eso unificamos predios que estab separados por alguna via de circulación interna, 
# asi podemos considerar su tamaño total luego


# Generamos un _buffer_ en torno a los polígonos, y los unimos 

umbral_de_proximidad <- 5

areas_unificadas <- st_buffer(areas_verdes, umbral_de_proximidad) %>% 
    st_union() %>% 
    st_cast("POLYGON") %>% 
    st_sf() %>% 
    mutate(id = factor(row_number())) 


# Funcion ad-hoc para agregar un feature a paste() que deberia tener...
# no escribas "NA" (como texto) donde encontras valores NA !!!

paste_skip_NAs <- function(lista, sep =", ") {
    paste(lista[!is.na(lista)], collapse = sep)
}

areas_verdes <- areas_verdes %>% 
    st_join(areas_unificadas) %>% 
    #lwgeom::st_make_valid() %>% #  si el summarise() falla con un error de topología inválida
    group_by(id) %>% 
    summarise(osm_id = paste(osm_id, collapse = ","),
              name = paste_skip_NAs(name),
              fclass = paste(fclass, collapse = ","),
              combina = n()) %>% 
    distinct(osm_id, .keep_all = TRUE) # este paso elimina poligonos que aparecen duplicados despues del join

# este paso elimina algunos espacios verdes individuales que aparecen sueltos pero 
# también estan incluidos en algún espacio combinado por el buffer
espacios_combinados <- areas_verdes %>% 
    filter(combina > 1) %>% 
    pull(osm_id) %>% 
    str_split(",") %>% 
    unlist()

areas_verdes <- areas_verdes %>% 
    filter(!(combina == 1 & (osm_id %in% espacios_combinados)))

# a guardar

st_write(areas_verdes, "data/processed/osm/areas_verdes_urbanas_argentina.shp", delete_dsn = TRUE)
