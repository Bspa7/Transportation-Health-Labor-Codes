# Date of creation: 30-Sep-2024
# Created by Brayan Pineda
# Objective: This code generates the info to update the total of registers between 2010-2019 by score.
#            This information allows to update the first table of all the project

paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")

# Initial databases ---------------------------------------------------------

main_base     <- open_dataset(sprintf('%s/%s', data_dir, "panel_total_sample1045_t1019.parquet")) %>% collect()
entire_sample <- open_dataset(sprintf('%s/%s', data_dir, "base_pila_rips_pbid_monthly.parquet")) %>% collect()

# ----

# Paso 1: Filtrar personas con registros en PILA y RIPS entre 2010-2019 --------

# Crear una variable de año usando 'monthly_date'
main_base <- main_base %>%
  mutate(year = year(monthly_date))  # Crear variable de año

entire_sample <- entire_sample %>%
  mutate(year = year(monthly_date))  # Crear variable de año


# 1. Registros en RIPS entre 2010-2019
main_rips_1019 <- main_base %>%
  filter(d_visitas_rips == 1 & year >= 2010 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

# 2. Registros en PILA entre 2010-2019
main_pila_1019 <- main_base %>%
  filter(d_registros_pila == 1 & year >= 2010 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

# 3. Registros en PILA y RIPS entre 2010-2019
main_pila_rips_1019 <- main_base %>%
  filter(d_visitas_rips == 1 & d_registros_pila == 1 & year >= 2010 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

# 4. Registros en PILA o RIPS entre 2010-2019
main_pila_or_rips_1019 <- main_base %>%
  filter((d_visitas_rips == 1 | d_registros_pila == 1) & year >= 2010 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

# ----

# Paso 2: Crear los grupos de puntaje para el periodo 2010-2019 ----------------
crear_grupos_puntaje <- function(df) {
  df %>%
    mutate(
           score_1015 = if_else(puntaje>10    & puntaje<=15, 1, 0),      
           score_1520 = if_else(puntaje>15    & puntaje<=20, 1, 0),
           score_2025 = if_else(puntaje>20    & puntaje<=25, 1, 0),
           score_2530 = if_else(puntaje>25    & puntaje<=30.56, 1, 0),
           score_3035 = if_else(puntaje>30.56 & puntaje<=35, 1, 0),
           score_3540 = if_else(puntaje>35    & puntaje<=40, 1, 0),
           score_4045 = if_else(puntaje>40    & puntaje<=45, 1, 0)
    )
}

main_rips_1019         <- crear_grupos_puntaje(main_rips_1019)
main_pila_1019         <- crear_grupos_puntaje(main_pila_1019)
main_pila_rips_1019    <- crear_grupos_puntaje(main_pila_rips_1019)
main_pila_or_rips_1019 <- crear_grupos_puntaje(main_pila_or_rips_1019)

# ----

# Paso 3: Resumir las personas por grupo de puntaje (para 2010-2019 ------------
resumir_personas <- function(df) {
  df %>%
    summarise(
      total_personas = n_distinct(personabasicaid),  # Total de personas
      personas_1015 = sum(score_1015),               # Personas con puntaje entre 10 y 15      
      personas_1520 = sum(score_1520),               # Personas con puntaje entre 15 y 20
      personas_2025 = sum(score_2025),               # Personas con puntaje entre 20 y 25
      personas_2530 = sum(score_2530),               # Personas con puntaje entre 25 y 30.56
      personas_3035 = sum(score_3035),               # Personas con puntaje entre 30.56 y 35
      personas_3540 = sum(score_3540),               # Personas con puntaje entre 35 y 40
      personas_4045 = sum(score_4045)                # Personas con puntaje entre 40 y 45
    )
}

# Resumen de personas para RIPS, PILA, PILA & RIPS, PILA o RIPS (2010-2019)
summary_rips_1019 <- resumir_personas(main_rips_1019)
summary_pila_1019 <- resumir_personas(main_pila_1019)
summary_pila_rips_1019 <- resumir_personas(main_pila_rips_1019)
summary_pila_or_rips_1019 <- resumir_personas(main_pila_or_rips_1019)

# ----

# Paso 4: Filtrar para 2015-2019 -----------------------------------------------

# Filtrar personas con registros en PILA y RIPS entre 2015-2019
main_rips_1519 <- main_base %>%
  filter(d_visitas_rips == 1 & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

main_pila_1519 <- main_base %>%
  filter(d_registros_pila == 1 & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

main_pila_rips_1519 <- main_base %>%
  filter(d_visitas_rips == 1 & d_registros_pila == 1 & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

main_pila_or_rips_1519 <- main_base %>%
  filter((d_visitas_rips == 1 | d_registros_pila == 1) & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

# Crear los grupos de puntaje
main_rips_1519 <- crear_grupos_puntaje(main_rips_1519)
main_pila_1519 <- crear_grupos_puntaje(main_pila_1519)
main_pila_rips_1519 <- crear_grupos_puntaje(main_pila_rips_1519)
main_pila_or_rips_1519 <- crear_grupos_puntaje(main_pila_or_rips_1519)

# Resumen para 2015-2019
summary_rips_1519 <- resumir_personas(main_rips_1519)
summary_pila_1519 <- resumir_personas(main_pila_1519)
summary_pila_rips_1519 <- resumir_personas(main_pila_rips_1519)
summary_pila_or_rips_1519 <- resumir_personas(main_pila_or_rips_1519)

# ----

summary_rips_1019         <- summary_rips_1019         %>% mutate(origen = "MAIN_RIPS-1019")
summary_pila_1019         <- summary_pila_1019         %>% mutate(origen = "MAIN_PILA-1019")
summary_pila_rips_1019    <- summary_pila_rips_1019    %>% mutate(origen = "MAIN_RIPS_AND_PILA_1019")
summary_pila_or_rips_1019 <- summary_pila_or_rips_1019 %>% mutate(origen = "MAIN_RIPS_OR_PILA_1019")
summary_rips_1519         <- summary_rips_1519         %>% mutate(origen = "MAIN_RIPS_1519")
summary_pila_1519         <- summary_pila_1519         %>% mutate(origen = "MAIN_PILA_1519")
summary_pila_rips_1519    <- summary_pila_rips_1519    %>% mutate(origen = "MAIN_RIPS_AND_PILA_1519")
summary_pila_or_rips_1519 <- summary_pila_or_rips_1519 %>% mutate(origen = "MAIN_RIPS_OR_PILA_1519")


# ----

# Paso 5: Filtrar para 2015-2019 en la base completa ---------------------------

# Filtrar personas con registros en PILA y RIPS entre 2015-2019
entire_rips_1519 <- entire_sample %>%
  filter(d_visitas_rips == 1 & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

entire_pila_1519 <- entire_sample %>%
  filter(d_registros_pila == 1 & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

entire_pila_rips_1519 <- entire_sample %>%
  filter(d_visitas_rips == 1 & d_registros_pila == 1 & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

entire_pila_or_rips_1519 <- entire_sample %>%
  filter((d_visitas_rips == 1 | d_registros_pila == 1) & year >= 2015 & year <= 2019) %>%
  select(personabasicaid, puntaje) %>%
  unique()

# Crear los grupos de puntaje
entire_rips_1519 <- crear_grupos_puntaje(entire_rips_1519)
entire_pila_1519 <- crear_grupos_puntaje(entire_pila_1519)
entire_pila_rips_1519 <- crear_grupos_puntaje(entire_pila_rips_1519)
entire_pila_or_rips_1519 <- crear_grupos_puntaje(entire_pila_or_rips_1519)

# Resumen para 2015-2019
summary_entire_rips_1519 <- resumir_personas(entire_rips_1519)
summary_entire_pila_1519 <- resumir_personas(entire_pila_1519)
summary_entire_pila_rips_1519 <- resumir_personas(entire_pila_rips_1519)
summary_entire_pila_or_rips_1519 <- resumir_personas(entire_pila_or_rips_1519)


summary_entire_rips_1519         <- summary_entire_rips_1519         %>% mutate(origen = "ENTIRE_RIPS_1519")
summary_entire_pila_1519         <- summary_entire_pila_1519         %>% mutate(origen = "ENTIRE_PILA_1519")
summary_entire_pila_rips_1519    <- summary_entire_pila_rips_1519    %>% mutate(origen = "ENTIRE_RIPS_AND_PILA_1519")
summary_entire_pila_or_rips_1519 <- summary_entire_pila_or_rips_1519 %>% mutate(origen = "ENTIRE_RIPS_OR_PILA_1519")



# Combinar todos los resúmenes en un solo dataframe
summary_total <- bind_rows(summary_rips_1019,        
                           summary_pila_1019,        
                           summary_pila_rips_1019,   
                           summary_pila_or_rips_1019,
                           summary_rips_1519,        
                           summary_pila_1519,        
                           summary_pila_rips_1519,   
                           summary_pila_or_rips_1519,
                           summary_entire_rips_1519,
                           summary_entire_pila_1519,
                           summary_entire_pila_rips_1519,
                           summary_entire_pila_or_rips_1519)

write_parquet(summary_total, file.path(output_folder, "parquet/20240930-stats_set5_step7.parquet"))

# Filtrar los objetos que comienzan con 'main_p', 'main_r', 'summary_r', o 'summary_p'
objetos_a_eliminar <- ls(pattern = "^(main_p|main_r|summary_r|summary_p|entire|summary_e)")

# Eliminar los objetos filtrados
rm(list = objetos_a_eliminar)

# Limpiar la memoria
gc()











