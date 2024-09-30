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

# Yearly Functions -------------------------------------------------------------

# Función auxiliar para calcular sumas por subgrupos
calcular_subgrupos <- function(data, var_name) {
  data %>%
    summarise(
      total   = sum(!!sym(var_name), na.rm = TRUE),
      male    = sum(!!sym(var_name) * (genero == 1), na.rm = TRUE),
      female  = sum(!!sym(var_name) * (genero == 0), na.rm = TRUE),
      s2025   = sum(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
      s2530   = sum(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
      s3035   = sum(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
      s3540   = sum(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
      s4045   = sum(!!sym(var_name) * (score_4045 == 1), na.rm = TRUE),
      s2025_m = sum(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
      s2025_f = sum(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
      s2530_m = sum(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
      s2530_f = sum(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
      s3035_m = sum(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
      s3035_f = sum(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
      s3540_m = sum(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
      s3540_f = sum(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
      s4045_m = sum(!!sym(var_name) * (score_4045 == 1 & genero == 1), na.rm = TRUE),
      s4045_f = sum(!!sym(var_name) * (score_4045 == 1 & genero == 0), na.rm = TRUE)
    )
}

# Función principal para calcular estadísticas agregadas anuales
calcular_stats_anual <- function(data, var_name) {
  data %>%
    group_by(year) %>%
    calcular_subgrupos(var_name) %>%
    rename_with(~paste(var_name, ., sep = "_"), -year) %>% 
    ungroup()
}

# ----

# 1. Initial databases ---------------------------------------------------------

main_base <- open_dataset(sprintf('%s/%s', data_dir, "panel_total_sample1045_t1019.parquet")) %>% collect()
main_base <- main_base %>% 
  mutate(score_2025 = if_else(puntaje>=20    & puntaje<=25, 1, 0),
         score_2530 = if_else(puntaje>25     & puntaje<=30.56, 1, 0),
         score_3035 = if_else(puntaje>30.56  & puntaje<=35, 1, 0),
         score_3540 = if_else(puntaje>35     & puntaje<=40, 1, 0),
         score_4045 = if_else(puntaje>40     & puntaje<=45, 1, 0),
         year = year(monthly_date)
  ) %>% collect()


yearly_base <- main_base %>%
  group_by(year, personabasicaid) %>%  
  summarise(
    # Informacion relacionada con conteos y dummies de RIPS en sí mismo  
    d_visitas_rips      = if_else(sum(d_visitas_rips     == 1, na.rm = TRUE) > 0, 1, 0),
    d_consultas         = if_else(sum(d_consultas        == 1, na.rm=TRUE) > 0, 1, 0),
    d_hospitalizaciones = if_else(sum(d_hospitalizaciones== 1, na.rm=TRUE) > 0, 1, 0),
    d_procedimientos    = if_else(sum(d_procedimientos   == 1, na.rm=TRUE) > 0, 1, 0),
    d_urgencias         = if_else(sum(d_urgencias        == 1, na.rm=TRUE) > 0, 1, 0),
    d_registros_pila    = if_else(sum(d_registros_pila   ==1, na.rm=TRUE) > 0, 1, 0)    
  )

yearly_stats <- yearly_base %>%
 group_by(year) %>%  
 summarise(
   # Informacion relacionada con conteos y dummies de RIPS en sí mismo  
   d_visitas_rips      = sum(d_visitas_rips, na.rm = TRUE),
   d_consultas         = sum(d_consultas, na.rm = TRUE),
   d_hospitalizaciones = sum(d_hospitalizaciones, na.rm = TRUE),
   d_procedimientos    = sum(d_procedimientos, na.rm = TRUE),
   d_urgencias         = sum(d_urgencias, na.rm = TRUE),
   d_registros_pila    = sum(d_registros_pila, na.rm = TRUE)    
 )



# Variables de conteo (solo las que empiezan con 'd_')
variables_conteo <- c("d_visitas_rips", "d_consultas", "d_hospitalizaciones", 
                      "d_procedimientos", "d_urgencias", "d_preven", "d_prenat", 
                      "d_registros_pila")

# Crear data frames vacíos para guardar los resultados
results_count_yearly <- data.frame()

# Iteración sobre variables de conteo
for (var in variables_conteo) {
  temp_df <- calcular_stats_anual(main_base, var)
  results_count_yearly <- if (nrow(results_count_yearly) == 0) temp_df else results_count_yearly %>% left_join(temp_df, by = "year")
}

write_parquet(yearly_stats,           file.path(output_folder, "parquet/20240919-stats_set3_step1.parquet"))
write_parquet(results_count_yearly,   file.path(output_folder, "parquet/20240919-stats_set3_step2.parquet"))


# Crear variable que indica si la persona tiene al menos un registro en d_registros_pila
main_base <- main_base %>%
  group_by(personabasicaid) %>%
  mutate(tiene_registro = if_else(sum(d_registros_pila, na.rm = TRUE) > 0, 1, 0)) %>%
  ungroup()

# Calcular el total de individuos que tienen al menos un registro en d_registros_pila
total_tiene_registro <- main_base %>%
  filter(tiene_registro == 1) %>%
  summarise(
    total   = n_distinct(personabasicaid),
    s2025   = n_distinct(personabasicaid[score_2025 == 1]),
    s2530   = n_distinct(personabasicaid[score_2530 == 1]),
    s3035   = n_distinct(personabasicaid[score_3035 == 1]),
    s3540   = n_distinct(personabasicaid[score_3540 == 1]),
    s4045   = n_distinct(personabasicaid[score_4045 == 1])
  )

# Mostrar el resultado
write_parquet(total_tiene_registro, file.path(output_folder, "parquet/20240919-stats_set3_step3.parquet"))

# ----


paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
#base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

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
      score_2025 = if_else(puntaje >= 20    & puntaje <= 25, 1, 0),
      score_2530 = if_else(puntaje > 25     & puntaje <= 30.56, 1, 0),
      score_3035 = if_else(puntaje > 30.56  & puntaje <= 35, 1, 0),
      score_3540 = if_else(puntaje > 35     & puntaje <= 40, 1, 0),
      score_4045 = if_else(puntaje > 40     & puntaje <= 45, 1, 0)
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

write_parquet(summary_total, file.path(output_folder, "parquet/20240923-stats_set4_step1.parquet"))

# Filtrar los objetos que comienzan con 'main_p', 'main_r', 'summary_r', o 'summary_p'
objetos_a_eliminar <- ls(pattern = "^(main_p|main_r|summary_r|summary_p|entire|summary_e)")

# Eliminar los objetos filtrados
rm(list = objetos_a_eliminar)

# Limpiar la memoria
gc()












