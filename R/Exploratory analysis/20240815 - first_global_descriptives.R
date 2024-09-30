
# Date of creation: 13-Aug-2024
# Created by Brayan Pineda
# Objective: This code creates some global descriptives


paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_pbid <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/datos_originales/personabasicaid_all_projects/Bases insumos"
base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")



# 1. Initial databases ---------------------------------------------------------

main_base <- open_dataset(sprintf('%s/%s', data_dir, "base_pila_rips_pbid_monthly.parquet")) %>% collect()
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')

# Capturing gender from RIPS information to replace NA's in sisben gender
rips_gender <- open_dataset(RIPS_history_file) %>%
  select(personabasicaid, sexo_RIPS) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(rips_gender = if_else(sexo_RIPS == "M", 1, 0))

main_base <- main_base %>% 
  left_join(rips_gender, by = "personabasicaid") %>% 
  mutate(genero = coalesce(genero, rips_gender)) %>% 
  select(-rips_gender)  

rm(RIPS_history_file, rips_gender)
gc()

# ----

# 2. Dummy by score groups -----------------------------------------------------

main_base <- main_base %>% 
  mutate(score_1520 = if_else(puntaje>15    & puntaje<=20, 1, 0),
         score_2025 = if_else(puntaje>20    & puntaje<=25, 1, 0),
         score_2530 = if_else(puntaje>25    & puntaje<=30.56, 1, 0),
         score_3035 = if_else(puntaje>30.56 & puntaje<=35, 1, 0),
         score_3540 = if_else(puntaje>35    & puntaje<=40, 1, 0),
         score_4245 = if_else(puntaje>40    & puntaje<=45, 1, 0)
  )

# ----


# 3. Iteration to create stats -------------------------------------------------

calcular_stats <- function(data, var_name) {
  data %>%
    group_by(monthly_date) %>%
    summarise(
      total   = sum(!!sym(var_name), na.rm = TRUE),
      male    = sum(!!sym(var_name) * (genero == 0), na.rm = TRUE),
      female  = sum(!!sym(var_name) * (genero == 1), na.rm = TRUE),    
      s1520   = sum(!!sym(var_name) * (score_1520 == 1), na.rm = TRUE),
      s2025   = sum(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
      s2530   = sum(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
      s3035   = sum(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
      s3540   = sum(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
      s4245   = sum(!!sym(var_name) * (score_4245 == 1), na.rm = TRUE),
      s1520_m = sum(!!sym(var_name) * (score_1520 == 1 & genero == 0), na.rm = TRUE),
      s1520_f = sum(!!sym(var_name) * (score_1520 == 1 & genero == 1), na.rm = TRUE),
      s2025_m = sum(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
      s2025_f = sum(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
      s2530_m = sum(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
      s2530_f = sum(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
      s3035_m = sum(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
      s3035_f = sum(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
      s3540_m = sum(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
      s3540_f = sum(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
      s4245_m = sum(!!sym(var_name) * (score_4245 == 1 & genero == 0), na.rm = TRUE),
      s4245_f = sum(!!sym(var_name) * (score_4245 == 1 & genero == 1), na.rm = TRUE)
    ) %>%
    rename_with(~paste(var_name, ., sep = "_"), -monthly_date) %>% 
    ungroup()
}


# List of variables to obtain monthly counters
variables <- c("n_visitas_rips",  "n_consultas", "n_hospitalizaciones", 
               "n_procedimientos", "n_urgencias", "c_preven", "c_prenat", 
               "n_registros_pila", "ibc_salud", "sal_dias_cot_sum", "sal_dias_cot_max")

# Empty data frame to save results
resultados <- data.frame()

# Iteration over variables
for (var in variables) {
  # Calculating stats for each variable
  temp_df <- calcular_stats(main_base, var)
  
  # Combine the results in the final data frame
  if (nrow(resultados) == 0) {
    resultados <- temp_df
  } else {
    resultados <- resultados %>% left_join(temp_df, by = "monthly_date")
  }
}

# Saving results
write_parquet(resultados, file.path(output_folder, "parquet/20240814-gnral_stats.parquet"))

# Cleaning environment 
rm(list=ls())












