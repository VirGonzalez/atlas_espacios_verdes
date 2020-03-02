library(readxl)
library(tidyverse)
    
# Los datos de mortalidad por departamento fueron obtenidos de 
# http://www.deis.msal.gov.ar/index.php/estadisticas-vitales/
# Tienen un sistema lo mas molesto posible para descargar los datos, que requiere cliquear 
# en formularios javascript. Para no perder tiempo en automatizar un scraping que probablemente 
# deje de funcionar en cuanto hagan el siguiente cambio al sitio, hemos descargado a mano 
# los datos de mortalidad en CABA y PBA para 2018.
#   
# Vamos a generar un .csv con la mortalidad en departamentos del AMBA. En la práctica, 
# las comunas de la CABA y los partidos del conurbano

mortalidad <-  read_excel("data/raw/DEIS/NacimyDefuncSegunGrupoEdadxPeriodoProvResDepRes_CABA.xls") %>% 
    rbind(read_excel("data/raw/DEIS/NacimyDefuncSegunGrupoEdadxPeriodoProvResDepRes_BA.xls")) %>% 
    filter(!is.na(DepRes), DepRes != "999") %>% 
    rename(codprov = ProvRes, coddepto = DepRes)


denominacion_partidos_AMBA_EPH <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    st_set_geometry(NULL) %>% 
    filter(tiporad == "U", eph_aglome %in% c("CABA", "Partidos del GBA")) %>% 
    select(eph_aglome, codprov, coddepto, localidade) %>% 
    distinct() %>% 
    mutate(localidade = str_replace(localidade, "\\(.*\\) ", "")) 
    
mortalidad_AMBA <- denominacion_partidos_AMBA_EPH %>% 
    left_join(mortalidad) %>% 
    select(-`Jurisdicciones Departamentos`) 

write_csv(mortalidad_AMBA, "data/processed/DEIS")