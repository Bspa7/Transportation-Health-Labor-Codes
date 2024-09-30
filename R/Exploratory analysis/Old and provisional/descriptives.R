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

# 1. Initial databases ---------------------------------------------------------
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))

# ----

# 2. Basic Descriptive analysis ------------------------------------------------


# Tables counting individuals
summary_counts <- unique_individuals %>%
  summarise(
    total_individuals = n(),
    total_male = sum(male),
    total_female = sum(female),
    total_score_m15 = sum(score_m15),
    total_score_1520 = sum(score_1520),
    total_score_2025 = sum(score_2025),
    total_score_2530 = sum(score_2530),
    total_score_3035 = sum(score_3035),
    total_score_3540 = sum(score_3540),
    total_score_4245 = sum(score_4245),
    total_score_45m = sum(score_45m),
    total_estrato_1 = sum(estrato_1),
    total_estrato_2 = sum(estrato_2),
    total_estrato_3 = sum(estrato_3),
    total_estrato_4 = sum(estrato_4),
    total_estrato_5 = sum(estrato_5),
    total_estrato_6 = sum(estrato_6),
    total_activi_sin = sum(activi_sin),
    total_activi_tra = sum(activi_tra),
    total_activi_bus = sum(activi_bus),
    total_activi_est = sum(activi_est),
    total_activi_hog = sum(activi_hog),
    total_estcivil_unlibr = sum(estcivil_unlibr),
    total_estcivil_casado = sum(estcivil_casado),
    total_estcivil_solter = sum(estcivil_solter)
  )

# Calculate some descriptive statistics (min, mean, max) for each dummy  
summary_stats <- unique_individuals %>%
  summarise(
    male_min = min(male), male_mean = mean(male), male_max = max(male),
    female_min = min(female), female_mean = mean(female), female_max = max(female),
    score_m15_min = min(score_m15), score_m15_mean = mean(score_m15), score_m15_max = max(score_m15),
    score_1520_min = min(score_1520), score_1520_mean = mean(score_1520), score_1520_max = max(score_1520),
    score_2025_min = min(score_2025), score_2025_mean = mean(score_2025), score_2025_max = max(score_2025),
    score_2530_min = min(score_2530), score_2530_mean = mean(score_2530), score_2530_max = max(score_2530),
    score_3035_min = min(score_3035), score_3035_mean = mean(score_3035), score_3035_max = max(score_3035),
    score_3540_min = min(score_3540), score_3540_mean = mean(score_3540), score_3540_max = max(score_3540),
    score_4245_min = min(score_4245), score_4245_mean = mean(score_4245), score_4245_max = max(score_4245),
    score_45m_min = min(score_45m), score_45m_mean = mean(score_45m), score_45m_max = max(score_45m),
    estrato_1_min = min(estrato_1), estrato_1_mean = mean(estrato_1), estrato_1_max = max(estrato_1),
    estrato_2_min = min(estrato_2), estrato_2_mean = mean(estrato_2), estrato_2_max = max(estrato_2),
    estrato_3_min = min(estrato_3), estrato_3_mean = mean(estrato_3), estrato_3_max = max(estrato_3),
    estrato_4_min = min(estrato_4), estrato_4_mean = mean(estrato_4), estrato_4_max = max(estrato_4),
    estrato_5_min = min(estrato_5), estrato_5_mean = mean(estrato_5), estrato_5_max = max(estrato_5),
    estrato_6_min = min(estrato_6), estrato_6_mean = mean(estrato_6), estrato_6_max = max(estrato_6),
    activi_sin_min = min(activi_sin), activi_sin_mean = mean(activi_sin), activi_sin_max = max(activi_sin),
    activi_tra_min = min(activi_tra), activi_tra_mean = mean(activi_tra), activi_tra_max = max(activi_tra),
    activi_bus_min = min(activi_bus), activi_bus_mean = mean(activi_bus), activi_bus_max = max(activi_bus),
    activi_est_min = min(activi_est), activi_est_mean = mean(activi_est), activi_est_max = max(activi_est),
    activi_hog_min = min(activi_hog), activi_hog_mean = mean(activi_hog), activi_hog_max = max(activi_hog),
    estcivil_unlibr_min = min(estcivil_unlibr), estcivil_unlibr_mean = mean(estcivil_unlibr), estcivil_unlibr_max = max(estcivil_unlibr),
    estcivil_casado_min = min(estcivil_casado), estcivil_casado_mean = mean(estcivil_casado), estcivil_casado_max = max(estcivil_casado),
    estcivil_solter_min = min(estcivil_solter), estcivil_solter_mean = mean(estcivil_solter), estcivil_solter_max = max(estcivil_solter)
  )

write_parquet(summary_counts, file.path(output_folder, "parquet/20240826-gnral_counts.parquet"))
write_parquet(summary_stats,  file.path(output_folder, "parquet/20240826-gnral_stats.parquet"))

# Figures (frequency and density plots)
fem <- unique_individuals %>% filter(genero==0)
mal <- unique_individuals %>% filter(genero==1)
scores1545 <- unique_individuals %>% filter(puntaje > 15 & puntaje <= 45)
scores1545_fem <- unique_individuals %>% filter(puntaje > 15 & puntaje <= 45 & genero==0)
scores1545_mal <- unique_individuals %>% filter(puntaje > 15 & puntaje <= 45 & genero==1)

# All sample
ggplot(unique_individuals, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(unique_individuals)), color = "red") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_population.png"), width = 8, height = 6)

# Female population
ggplot(fem, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(fem)), color = "purple") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_females.png"), width = 8, height = 6)

# Male population
ggplot(mal, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(mal)), color = "blue") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_males.png"), width = 8, height = 6)

# All sample: 15-45 scores
ggplot(scores1545, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(scores1545)), color = "red") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_scores1545.png"), width = 8, height = 6)

# Female sample: 15-45 scores
ggplot(scores1545_fem, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(scores1545_fem)), color = "purple") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_scores1545_fem.png"), width = 8, height = 6)

# Male sample: 15-45 scores
ggplot(scores1545_mal, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(scores1545_mal)), color = "blue") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_scores1545_mal.png"), width = 8, height = 6)

# Cleaning environment 
rm(list=ls())

# ----

# 3. Descriptive analysis to time series ---------------------------------------

calcular_stats <- function(data, var_name, tipo_variable) {
  if (tipo_variable == "conteo") {
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
  } else if (tipo_variable == "continuo") {
    data %>%
      group_by(monthly_date) %>%
      summarise(
        promedio = mean(!!sym(var_name), na.rm = TRUE),
        male     = mean(!!sym(var_name) * (genero == 0), na.rm = TRUE),
        female   = mean(!!sym(var_name) * (genero == 1), na.rm = TRUE),    
        s1520    = mean(!!sym(var_name) * (score_1520 == 1), na.rm = TRUE),
        s2025    = mean(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
        s2530    = mean(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
        s3035    = mean(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
        s3540    = mean(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
        s4245    = mean(!!sym(var_name) * (score_4245 == 1), na.rm = TRUE),
        s1520_m  = mean(!!sym(var_name) * (score_1520 == 1 & genero == 0), na.rm = TRUE),
        s1520_f  = mean(!!sym(var_name) * (score_1520 == 1 & genero == 1), na.rm = TRUE),
        s2025_m  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
        s2025_f  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
        s2530_m  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
        s2530_f  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
        s3035_m  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
        s3035_f  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
        s3540_m  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
        s3540_f  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
        s4245_m  = mean(!!sym(var_name) * (score_4245 == 1 & genero == 0), na.rm = TRUE),
        s4245_f  = mean(!!sym(var_name) * (score_4245 == 1 & genero == 1), na.rm = TRUE)
      ) %>%
      rename_with(~paste(var_name, ., sep = "_"), -monthly_date) %>% 
      ungroup()
  }
}

# Variables de conteo
variables_conteo <- c("n_visitas_rips",  "n_consultas", "n_hospitalizaciones", 
                      "n_procedimientos", "n_urgencias", "c_preven", "c_prenat", 
                      "d_registros_pila")

# Variables continuas
variables_continuas <- c("n_registros_pila", "ibc_salud", "sal_dias_cot_sum", "sal_dias_cot_max")

# Crear un data frame vacío para guardar los resultados
resultados <- data.frame()

# Iteración sobre variables de conteo
for (var in variables_conteo) {
  temp_df <- calcular_stats(main_base, var, "conteo")
  resultados <- if (nrow(resultados) == 0) temp_df else resultados %>% left_join(temp_df, by = "monthly_date")
}

# Iteración sobre variables continuas
for (var in variables_continuas) {
  temp_df <- calcular_stats(main_base, var, "continuo")
  resultados <- if (nrow(resultados) == 0) temp_df else resultados %>% left_join(temp_df, by = "monthly_date")
}

# Guardar los resultados
write_parquet(resultados, file.path(output_folder, "parquet/20240814-gnral_stats.parquet"))




# ----













