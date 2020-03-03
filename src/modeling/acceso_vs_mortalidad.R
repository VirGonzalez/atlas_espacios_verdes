library(tidyverse)

accesibilidad <- read_csv("data/processed/metricas/accesibilidad_espacios_verdes_localidades.csv") 

mortalidad_AMBA <- read_csv("data/processed/DEIS/mortalidad_AMBA_2018.csv") %>% 
    mutate(tasa_m_infantil = (`Menores de 1`/`Nacidos Vivos`) * 1000,
           # Esto esta mal, necesitariamos la cantidad de partos. Pero para explorar lo hacemos
           tasa_m_maternal = (`Muertes Maternas`/`Nacidos Vivos`) * 1000)

####


get_effect_m_infantil <- function(dframe) {
    
    decil = last(dframe$decil_NSE)
    
    dframe %>% 
        {lm(tasa_m_infantil ~ tasa_acceso + total_ha_accesibles + m2_accesibles_per_capita, data = .)} %>% 
        broom::tidy() %>% 
        mutate(p.value = round(p.value, 2)) %>% 
        {cbind(decil, .)} %>% 
        filter(term != "(Intercept)")
}

plot_model <- function(dframe) {
    
    decil = last(dframe$decil_NSE)
    
    p <- ggplot(dframe, aes(x = tasa_acceso, y = tasa_m_infantil)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(title = paste(decil))
}


# Encontramos que una mayor tasa de acceso a esapacios verdes esta correlacionada con una menor
# mortandad infantil... para los estratos más altos de la población! Deciles NSE 7, 8, 9, 10

accesibilidad %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map_df(get_effect_m_infantil)

# Scatterplots

accesibilidad %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map(plot_model)
    

### Mortalidad maternal
# Tambien encontramos que una mayor tasa de acceso a esapacios verdes esta correlacionada con una menor
# tasa de mortandad materna / nacimientos... para los estratos más altos de la población! 
# Deciles NSE 6, 7, 8, 9, 10


get_effect_m_maternal <- function(dframe) {
    
    decil = last(dframe$decil_NSE)
    
    dframe %>% 
        {lm(tasa_m_maternal ~ tasa_acceso + total_ha_accesibles + m2_accesibles_per_capita, data = .)} %>% 
        broom::tidy() %>% 
        mutate(p.value = round(p.value, 2)) %>% 
        {cbind(decil, .)} %>% 
        filter(term != "(Intercept)")
}


accesibilidad %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map_df(get_effect_m_maternal)

###
###

accesibilidad_aglos <- read_csv("data/processed/metricas/accesibilidad_espacios_verdes_aglomerados.csv")

ggplot(accesibilidad_aglos) +
    geom_col(aes(x = decil_NSE, y = m2_accesibles_per_capita, fill = decil_NSE)) +
    facet_wrap(~eph_aglome, scales = "free_y") +
    scale_fill_viridis_c()
