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

# Función auxiliar para calcular sumas o promedios para subgrupos a nivel anual
calcular_subgrupos_anual <- function(data, var_name, tipo_calculo) {
  if (tipo_calculo == "conteo") {
    return(data %>%
             summarise(
               total   = sum(!!sym(var_name), na.rm = TRUE),
               male    = sum(!!sym(var_name) * (genero == 1), na.rm = TRUE),
               female  = sum(!!sym(var_name) * (genero == 0), na.rm = TRUE),
               s2025   = sum(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
               s2530   = sum(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
               s3035   = sum(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
               s3540   = sum(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
               s4245   = sum(!!sym(var_name) * (score_4045 == 1), na.rm = TRUE),
               s2025_m = sum(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
               s2025_f = sum(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
               s2530_m = sum(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
               s2530_f = sum(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
               s3035_m = sum(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
               s3035_f = sum(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
               s3540_m = sum(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
               s3540_f = sum(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
               s4245_m = sum(!!sym(var_name) * (score_4045 == 1 & genero == 1), na.rm = TRUE),
               s4245_f = sum(!!sym(var_name) * (score_4045 == 1 & genero == 0), na.rm = TRUE)
             ))
  } else if (tipo_calculo == "continuo") {
    return(data %>%
             summarise(
               promedio = mean(!!sym(var_name), na.rm = TRUE),
               male     = mean(!!sym(var_name) * (genero == 1), na.rm = TRUE),
               female   = mean(!!sym(var_name) * (genero == 0), na.rm = TRUE),
               s2025    = mean(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
               s2530    = mean(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
               s3035    = mean(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
               s3540    = mean(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
               s4245    = mean(!!sym(var_name) * (score_4045 == 1), na.rm = TRUE),
               s2025_m  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
               s2025_f  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
               s2530_m  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
               s2530_f  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
               s3035_m  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
               s3035_f  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
               s3540_m  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
               s3540_f  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
               s4245_m  = mean(!!sym(var_name) * (score_4045 == 1 & genero == 1), na.rm = TRUE),
               s4245_f  = mean(!!sym(var_name) * (score_4045 == 1 & genero == 0), na.rm = TRUE)
             ))
  }
}

# Función principal para calcular estadísticas a nivel anual
calcular_stats_anuales <- function(data, var_name, tipo_variable) {
  data %>%
    # Crear la variable de año a partir de monthly_date
    mutate(year = lubridate::year(monthly_date)) %>%
    # Agrupar por año
    group_by(year) %>%
    calcular_subgrupos_anual(var_name, tipo_variable) %>%
    rename_with(~paste(var_name, ., sep = "_"), -year) %>%
    ungroup()
}


calcular_estadisticas_ingreso_anual <- function(data, var_name) {
  # Crear la variable de año a partir de monthly_date
  data <- data %>%
    mutate(year = lubridate::year(monthly_date))
  
  # Calcular estadísticas para la población total a nivel anual
  resumen_total_anual <- data %>%
    group_by(year) %>%
    summarise(
      promedio_total = mean(!!sym(var_name), na.rm = TRUE),
      mediana_total = median(!!sym(var_name), na.rm = TRUE),
      p10_total = quantile(!!sym(var_name), 0.10, na.rm = TRUE),
      p25_total = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      p75_total = quantile(!!sym(var_name), 0.75, na.rm = TRUE),
      p90_total = quantile(!!sym(var_name), 0.90, na.rm = TRUE)
    )
  
  # Función para calcular estadísticas por subgrupo a nivel anual
  calcular_estadisticas_subgrupo_anual <- function(data, score_var, suffix) {
    data %>%
      filter(!!sym(score_var) == 1) %>%
      group_by(year) %>%
      summarise(
        !!paste0("promedio_", suffix) := mean(!!sym(var_name), na.rm = TRUE),
        !!paste0("mediana_", suffix) := median(!!sym(var_name), na.rm = TRUE),
        !!paste0("p10_", suffix) := quantile(!!sym(var_name), 0.10, na.rm = TRUE),
        !!paste0("p25_", suffix) := quantile(!!sym(var_name), 0.25, na.rm = TRUE),
        !!paste0("p75_", suffix) := quantile(!!sym(var_name), 0.75, na.rm = TRUE),
        !!paste0("p90_", suffix) := quantile(!!sym(var_name), 0.90, na.rm = TRUE)
      )
  }
  
  # Calcular estadísticas para cada subgrupo a nivel anual
  resumen_2025_anual <- calcular_estadisticas_subgrupo_anual(data, "score_2025", "2025")
  resumen_2530_anual <- calcular_estadisticas_subgrupo_anual(data, "score_2530", "2530")
  resumen_3035_anual <- calcular_estadisticas_subgrupo_anual(data, "score_3035", "3035")
  resumen_3540_anual <- calcular_estadisticas_subgrupo_anual(data, "score_3540", "3540")
  resumen_4245_anual <- calcular_estadisticas_subgrupo_anual(data, "score_4045", "4245")
  
  # Combinar todas las estadísticas anuales en un solo dataframe
  resumen_total_anual %>%
    left_join(resumen_2025_anual, by = "year") %>%
    left_join(resumen_2530_anual, by = "year") %>%
    left_join(resumen_3035_anual, by = "year") %>%
    left_join(resumen_3540_anual, by = "year") %>%
    left_join(resumen_4245_anual, by = "year")
}

# ----

# Monthly Functions ------------------------------------------------------------

# Función auxiliar para calcular sumas o promedios para subgrupos
calcular_subgrupos <- function(data, var_name, tipo_calculo) {
  if (tipo_calculo == "conteo") {
    return(data %>%
             summarise(
               total   = sum(!!sym(var_name), na.rm = TRUE),
               male    = sum(!!sym(var_name) * (genero == 1), na.rm = TRUE),
               female  = sum(!!sym(var_name) * (genero == 0), na.rm = TRUE),
               s2025   = sum(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
               s2530   = sum(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
               s3035   = sum(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
               s3540   = sum(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
               s4245   = sum(!!sym(var_name) * (score_4045 == 1), na.rm = TRUE),
               s2025_m = sum(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
               s2025_f = sum(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
               s2530_m = sum(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
               s2530_f = sum(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
               s3035_m = sum(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
               s3035_f = sum(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
               s3540_m = sum(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
               s3540_f = sum(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
               s4245_m = sum(!!sym(var_name) * (score_4045 == 1 & genero == 1), na.rm = TRUE),
               s4245_f = sum(!!sym(var_name) * (score_4045 == 1 & genero == 0), na.rm = TRUE)
             ))
  } else if (tipo_calculo == "continuo") {
    return(data %>%
             summarise(
               promedio = mean(!!sym(var_name), na.rm = TRUE),
               male     = mean(!!sym(var_name) * (genero == 1), na.rm = TRUE),
               female   = mean(!!sym(var_name) * (genero == 0), na.rm = TRUE),
               s2025    = mean(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
               s2530    = mean(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
               s3035    = mean(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
               s3540    = mean(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
               s4245    = mean(!!sym(var_name) * (score_4045 == 1), na.rm = TRUE),
               s2025_m  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
               s2025_f  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
               s2530_m  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
               s2530_f  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
               s3035_m  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
               s3035_f  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
               s3540_m  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
               s3540_f  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
               s4245_m  = mean(!!sym(var_name) * (score_4045 == 1 & genero == 1), na.rm = TRUE),
               s4245_f  = mean(!!sym(var_name) * (score_4045 == 1 & genero == 0), na.rm = TRUE)
             ))
  }
}

# Función principal para calcular estadísticas
calcular_stats <- function(data, var_name, tipo_variable) {
  data %>%
    group_by(monthly_date) %>%
    calcular_subgrupos(var_name, tipo_variable) %>%
    rename_with(~paste(var_name, ., sep = "_"), -monthly_date) %>% 
    ungroup()
}




calcular_estadisticas_ingreso_mensual <- function(data, var_name) {
  # Calcular estadísticas para la población total a nivel mensual
  resumen_total_mensual <- data %>%
    group_by(monthly_date) %>%
    summarise(
      promedio_total = mean(!!sym(var_name), na.rm = TRUE),
      mediana_total = median(!!sym(var_name), na.rm = TRUE),
      p10_total = quantile(!!sym(var_name), 0.10, na.rm = TRUE),
      p25_total = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      p75_total = quantile(!!sym(var_name), 0.75, na.rm = TRUE),
      p90_total = quantile(!!sym(var_name), 0.90, na.rm = TRUE)
    )
  
  # Función para calcular estadísticas por subgrupo a nivel mensual
  calcular_estadisticas_subgrupo_mensual <- function(data, score_var, suffix) {
    data %>%
      filter(!!sym(score_var) == 1) %>%
      group_by(monthly_date) %>%
      summarise(
        !!paste0("promedio_", suffix) := mean(!!sym(var_name), na.rm = TRUE),
        !!paste0("mediana_", suffix) := median(!!sym(var_name), na.rm = TRUE),
        !!paste0("p10_", suffix) := quantile(!!sym(var_name), 0.10, na.rm = TRUE),
        !!paste0("p25_", suffix) := quantile(!!sym(var_name), 0.25, na.rm = TRUE),
        !!paste0("p75_", suffix) := quantile(!!sym(var_name), 0.75, na.rm = TRUE),
        !!paste0("p90_", suffix) := quantile(!!sym(var_name), 0.90, na.rm = TRUE)
      )
  }
  
  # Calcular estadísticas para cada subgrupo a nivel mensual
  resumen_2025_mensual <- calcular_estadisticas_subgrupo_mensual(data, "score_2025", "2025")
  resumen_2530_mensual <- calcular_estadisticas_subgrupo_mensual(data, "score_2530", "2530")
  resumen_3035_mensual <- calcular_estadisticas_subgrupo_mensual(data, "score_3035", "3035")
  resumen_3540_mensual <- calcular_estadisticas_subgrupo_mensual(data, "score_3540", "3540")
  resumen_4245_mensual <- calcular_estadisticas_subgrupo_mensual(data, "score_4045", "4245")
  
  # Combinar todas las estadísticas mensuales en un solo dataframe
  resumen_total_mensual %>%
    left_join(resumen_2025_mensual, by = "monthly_date") %>%
    left_join(resumen_2530_mensual, by = "monthly_date") %>%
    left_join(resumen_3035_mensual, by = "monthly_date") %>%
    left_join(resumen_3540_mensual, by = "monthly_date") %>%
    left_join(resumen_4245_mensual, by = "monthly_date")
}


# ----

# 1. Initial databases ---------------------------------------------------------

main_base <- open_dataset(sprintf('%s/%s', data_dir, "panel_total_sample2045_t1019.parquet")) %>% glimpse()

main_base <- main_base %>% 
  mutate(score_2025 = if_else(puntaje>=20    & puntaje<=25, 1, 0),
         score_2530 = if_else(puntaje>25     & puntaje<=30.56, 1, 0),
         score_3035 = if_else(puntaje>30.56  & puntaje<=35, 1, 0),
         score_3540 = if_else(puntaje>35     & puntaje<=40, 1, 0),
         score_4045 = if_else(puntaje>40     & puntaje<=45, 1, 0)
  ) %>% collect()


# ----

# 3. Variable sets to apply function -------------------------------------------
# Variables de conteo
variables_conteo <- c("d_visitas_rips", "n_visitas_rips", "n_consultas", "n_hospitalizaciones", 
                      "n_procedimientos", "n_urgencias", "c_preven", "c_prenat", 
                      "d_registros_pila", "n_registros_pila", "pila_depen", "pila_indep", 
                      "reg_contributivo", "reg_subsidiado", "reg_vinculado",
                      "reg_particular",  "reg_otro", "reg_desp_contributivo", 
                      "reg_desp_subsidiado", "reg_desp_no_asegurado") 


# Variables continuas
variables_continuas <- c("ibc_salud_max", "ibc_ccf_max", "ibc_pens_max", "ibc_rprof_max", 
                         "income_base_max", "income_base_sum", 
                         "sal_dias_cot_max", "sal_dias_cot_sum" )

# ----

# 4. Yearly stats --------------------------------------------------------------

# Crear data frames vacíos para guardar los resultados
results_count_yearly <- data.frame()
results_continuous_yearly <- data.frame()

# Iteración sobre variables de conteo
for (var in variables_conteo) {
  temp_df <- calcular_stats_anuales(main_base, var, "conteo")
  results_count_yearly <- if (nrow(results_count_yearly) == 0) temp_df else results_count_yearly %>% left_join(temp_df, by = "year")
}

# Iteración sobre variables continuas
for (var in variables_continuas) {
  temp_df <- calcular_stats_anuales(main_base, var, "continuo")
  results_continuous_yearly <- if (nrow(results_continuous_yearly) == 0) temp_df else results_continuous_yearly %>% left_join(temp_df, by = "year")
}

# Llamar a la función para calcular las estadísticas anuales de 'income_base_max'
estadisticas_income_anual <- calcular_estadisticas_ingreso_anual(main_base, "income_base_max")

# ----

# 5. Monthly stats --------------------------------------------------------------

# Crear data frames vacíos para guardar los resultados
results_count_monthly <- data.frame()
results_continuous_monthly <- data.frame()

# Iteración sobre variables de conteo
for (var in variables_conteo) {
  temp_df <- calcular_stats(main_base, var, "conteo")
  results_count_monthly <- if (nrow(results_count_monthly) == 0) temp_df else results_count_monthly %>% left_join(temp_df, by = "monthly_date")
}

# Iteración sobre variables continuas
for (var in variables_continuas) {
  temp_df <- calcular_stats(main_base, var, "continuo")
  results_continuous_monthly <- if (nrow(results_continuous_monthly) == 0) temp_df else results_continuous_monthly %>% left_join(temp_df, by = "monthly_date")
}

# Llamar a la función para calcular las estadísticas mensuales de 'income_base_max'
estadisticas_income_mensual <- calcular_estadisticas_ingreso_mensual(main_base, "income_base_max")


# ----

# 6. Unique reports -----------------------------------------------------------


# Inicializar el dataframe de resultados
results_unique_monthly <- data.frame()

# Definir grupos de puntaje
grupos_puntaje <- c("score_2025", "score_2530", "score_3035", "score_3540", "score_4045")

# Función para contar personas únicas con al menos un registro en el mes
calcular_unicos <- function(data, var_name) {
  data %>%
    filter(!!sym(var_name) == 1) %>%
    group_by(monthly_date) %>%
    summarise(
      total = n_distinct(personabasicaid),  # Total de personas únicas
      male = n_distinct(personabasicaid[genero == 1]),  # Hombres
      female = n_distinct(personabasicaid[genero == 0]),  # Mujeres
      edad_18_24 = n_distinct(personabasicaid[edad >= 18 & edad < 25]),  # Grupo de edad 18-24
      edad_25_55 = n_distinct(personabasicaid[edad >= 25 & edad <= 55]),  # Grupo de edad 25-55
      edad_55_mas = n_distinct(personabasicaid[edad > 55]),  # Grupo de edad 55+
      score_2025 = n_distinct(personabasicaid[puntaje >= 20 & puntaje < 25]),  # Puntaje entre 20 y 25
      score_2530 = n_distinct(personabasicaid[puntaje >= 25 & puntaje < 30]),  # Puntaje entre 25 y 30
      score_3035 = n_distinct(personabasicaid[puntaje >= 30 & puntaje < 35]),  # Puntaje entre 30 y 35
      score_3540 = n_distinct(personabasicaid[puntaje >= 35 & puntaje < 40]),  # Puntaje entre 35 y 40
      score_4045 = n_distinct(personabasicaid[puntaje >= 40 & puntaje <= 45]),  # Puntaje entre 40 y 45
      mujeres_25_55_score_2025 = n_distinct(personabasicaid[genero == 0 & edad >= 25 & edad <= 55 & puntaje >= 20 & puntaje < 25]),  # Mujeres 25-55 años y puntaje 20-25
      hombres_25_55_score_2025 = n_distinct(personabasicaid[genero == 1 & edad >= 25 & edad <= 55 & puntaje >= 20 & puntaje < 25]),  # Hombres 25-55 años y puntaje 20-25
      # Repetir para otros grupos de puntaje
    ) %>%
    rename_with(~paste0("unicos_", var_name, "_", .), -monthly_date)
}

# Variables dummy para contar personas únicas con registros
variables_unicos <- c("d_visitas_rips", "d_consultas", "d_hospitalizaciones", 
                      "d_procedimientos", "d_urgencias", "d_preven", "d_prenat", 
                      "d_registros_pila")

# Iteración sobre variables de conteo de personas únicas
for (var in variables_unicos) {
  temp_df_unicos <- calcular_unicos(main_base, var)
  
  if (nrow(results_unique_monthly) == 0) {
    results_unique_monthly <- temp_df_unicos
  } else {
    results_unique_monthly <- left_join(results_unique_monthly, temp_df_unicos, by = "monthly_date")
  }
}

# ----

write_parquet(estadisticas_income_anual,   file.path(output_folder, "parquet/20240918-stats_set2_income_y.parquet"))
write_parquet(estadisticas_income_mensual, file.path(output_folder, "parquet/20240918-stats_set2_income_m.parquet"))
write_parquet(results_continuous_yearly,   file.path(output_folder, "parquet/20240918-stats_set2_cont_y.parquet"))
write_parquet(results_continuous_monthly,  file.path(output_folder, "parquet/20240918-stats_set2_cont_m.parquet"))
write_parquet(results_count_yearly,        file.path(output_folder, "parquet/20240918-stats_set2_count_y.parquet"))
write_parquet(results_count_monthly,       file.path(output_folder, "parquet/20240918-stats_set2_count_m.parquet"))
write_parquet(results_unique_monthly,      file.path(output_folder, "parquet/20240918-stats_set2_unique_m.parquet"))

