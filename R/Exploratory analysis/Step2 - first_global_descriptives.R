
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

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")


# 1. Definitions and functions -------------------------------------------------

# Define a variable to categorize age into group - RIPS
categorize_age <- function(df) {
  df <- df %>% 
    mutate(
      grupo_edad = case_when(
        between(edad_RIPS , 18, 26) ~ 1,
        between(edad_RIPS , 27, 59) ~ 2,
        edad_RIPS >= 60 ~ 3      
      )
    )
  return(df)
}

# Define a function to create the running variables centered in zero and treatments
centrar_puntaje <- function(df) {
  df <- df %>%
    mutate(
      cutoff30 = puntaje - 30.56,
      cutoff40 = puntaje - 40,
      treatment1 = ifelse(puntaje <= 30.56, 1, 0),
      treatment2 = ifelse(puntaje > 30.56 & puntaje <= 40, 1, 0),
      control1 = ifelse(puntaje > 40, 1, 0)
    )
  return(df)
}

# ----

# 2. Initial databases ---------------------------------------------------------

main_base <- open_dataset(sprintf('%s/%s', data_dir, "base_pila_rips_pbid_monthly.parquet")) %>% collect()

# ----

variables_to_replace <- c("n_visitas_rips", "n_consultas", "n_hospitalizaciones", 
                          "n_procedimientos", "n_urgencias", "c_preven", "c_prenat", 
                          "n_registros_pila", "ibc_salud", "sal_dias_cot_sum", "sal_dias_cot_max")

# Reemplazar valores con una distribución normal entre 0 y 100
set.seed(123)  # Para reproducibilidad
main_base[variables_to_replace] <- lapply(main_base[variables_to_replace], function(x) {
  rnorm(n = length(x), mean = 50, sd = 25) %>% pmax(0) %>% pmin(100)
})


# 3. Modifications to initial database -----------------------------------------
main_base <- main_base %>% 
  mutate(score_1520 = if_else(puntaje>15    & puntaje<=20, 1, 0),
         score_2025 = if_else(puntaje>20    & puntaje<=25, 1, 0),
         score_2530 = if_else(puntaje>25    & puntaje<=30.56, 1, 0),
         score_3035 = if_else(puntaje>30.56 & puntaje<=35, 1, 0),
         score_3540 = if_else(puntaje>35    & puntaje<=40, 1, 0),
         score_4245 = if_else(puntaje>40    & puntaje<=45, 1, 0)
  )

# ----



# 4. Creating a monthly database with descriptives -----------------------------

calculate_descriptives <- function(data, var_name) {
  descriptivas <- data %>%
    group_by(monthly_date) %>%
    summarise(
      # Totales generales
      total = sum(!!sym(var_name), na.rm = TRUE),
      # Totales por género
      male   = sum(!!sym(var_name) * (genero == 0), na.rm = TRUE),
      female = sum(!!sym(var_name) * (genero == 1), na.rm = TRUE),
      
      # Totales por grupo de puntaje
      score_1520 = sum(!!sym(var_name) * (score_1520 == 1), na.rm = TRUE),
      score_2025 = sum(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
      score_2530 = sum(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
      score_3035 = sum(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
      score_3540 = sum(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
      score_4245 = sum(!!sym(var_name) * (score_4245 == 1), na.rm = TRUE),
      
      # Totales por grupo de puntaje y género
      score_1520_m = sum(!!sym(var_name) * (score_1520 == 1 & genero == 0), na.rm = TRUE),
      score_1520_f = sum(!!sym(var_name) * (score_1520 == 1 & genero == 1), na.rm = TRUE),
      score_2025_m = sum(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
      score_2025_f = sum(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
      score_2530_m = sum(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
      score_2530_f = sum(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
      score_3035_m = sum(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
      score_3035_f = sum(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
      score_3540_m = sum(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
      score_3540_f = sum(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
      score_4245_m = sum(!!sym(var_name) * (score_4245 == 1 & genero == 0), na.rm = TRUE),
      score_4245_f = sum(!!sym(var_name) * (score_4245 == 1 & genero == 1), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Mantener solo las columnas necesarias y evitar repeticiones
#    select(monthly_date, total, male, female) %>%
    unique()
  
  return(descriptivas)
}

# Ejecutar la función con "n_consultas"
descriptivas_n_consultas <- calculate_descriptives(main_base, "n_consultas")


# Renombrar las columnas para identificar que son de n_consultas
descriptivas_n_consultas <- descriptivas_n_consultas %>%
  rename_with(~ paste0("n_consultas_", .), -monthly_date)

# Calcular descriptivas para n_procedimientos
descriptivas_n_procedimientos <- calculate_descriptives(main_base, "n_procedimientos")

# Renombrar las columnas para identificar que son de n_procedimientos
descriptivas_n_procedimientos <- descriptivas_n_procedimientos %>%
  rename_with(~ paste0("n_procedimientos_", .), -monthly_date)

# Unir los resultados en un solo dataframe
final_descriptives <- full_join(descriptivas_n_consultas, descriptivas_n_procedimientos, by = "monthly_date")

# Ver el resultado
print(final_descriptives)














# Sumar n_consultas por monthly_date y genero
descriptivas <- main_base %>%
  group_by(monthly_date, genero) %>%
  summarise(
    n_consultas_total = sum(n_consultas, na.rm = TRUE),
    n_consultas_m = sum(n_consultas[genero == 1], na.rm = TRUE),
    n_consultas_f = sum(n_consultas[genero == 0], na.rm = TRUE)
  ) %>%
  ungroup()

# Si quieres obtenerlo en un formato específico con solo las columnas necesarias:
descriptivas <- descriptivas %>%
  select(monthly_date, n_consultas_total, n_consultas_m, n_consultas_f) %>% unique()









