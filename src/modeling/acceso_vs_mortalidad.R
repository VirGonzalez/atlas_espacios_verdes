library(tidyverse)

accesibilidad <- read_csv("data/processed/metricas/accesibilidad_espacios_verdes_localidades.csv") 

mortalidad <- read_csv("data/processed/DEIS/mortalidad_AMBA_2018.csv") %>% 
    mutate(tasa_m_infantil = (`Menores de 1`/`Nacidos Vivos`) * 1000 )

ax_decil_1_vs_mortandad <- accesibilidad %>% 
    filter(decil_NSE == 1) %>% 
    right_join(mortalidad) 

lm(tasa_m_infantil ~ tasa_acceso + total_ha_accesibles + m2_accesibles_per_capita, data = ax_decil_1_vs_mortandad) %>% 
    summary()



ax_sumario_vs_mortandad <- accesibilidad %>% 
    group_by(eph_aglome, localidade) %>%
    summarise(total_ha_accesibles = mean(total_ha_accesibles),
              m2_accesibles_per_capita = mean(m2_accesibles_per_capita)) %>% 
    right_join(mortalidad) %>% 
    mutate(tasa_m_infantil = (`Menores de 1`/`Nacidos Vivos`) * 1000 )

lm(tasa_m_infantil ~ total_ha_accesibles, data = ax_sumario_vs_mortandad) %>% broom::tidy()




####
####
####
####


get_effect <- function(dframe) {
    
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
    right_join(mortalidad) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map_df(get_effect)

# Scatterplots

accesibilidad %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map(plot_model)
    

