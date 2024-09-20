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

main_base <- open_dataset(sprintf('%s/%s', data_dir, "panel_total_sample2045_t1019.parquet")) %>% collect()
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















