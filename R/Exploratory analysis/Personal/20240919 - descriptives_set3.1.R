paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
#base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

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

yearly_base <- main_base %>%
  mutate( year = year(monthly_date) ) %>% 
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


write_parquet(yearly_stats,   file.path(output_folder, "parquet/20240919-stats_set3.1_unique_y.parquet"))







