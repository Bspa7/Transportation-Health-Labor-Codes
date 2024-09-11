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

# 1. Initial databases ---------------------------------------------------------
master    <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>% collect()
main_base <- open_dataset(sprintf('%s/%s', data_dir, "base_pila_rips_pbid_monthly.parquet")) 

# 1.1. Creating some variables
# Creating variables 
master <- master %>%
  mutate(
    # Date variables
    date_fechanto = as.Date(fechanto),
    year_fechanto = year(date_fechanto),
    month_fechanto = floor_date(date_fechanto, "month"),
    # Age groups and gender variable
    genero = if_else(sexo == 1, 1, 0), # zero is for female    
    edad_1824 = if_else(edad >= 18 & edad <= 24, 1, 0),
    edad_2555 = if_else(edad > 24 & edad <= 55, 1, 0),
    edad_55m  = if_else(edad > 55, 1, 0),    
    # Score groups
    score_m15 =  if_else(puntaje <= 15, 1, 0),    
    score_1520 = if_else(puntaje > 15 & puntaje <= 20, 1, 0),
    score_2025 = if_else(puntaje > 20 & puntaje <= 25, 1, 0),
    score_2530 = if_else(puntaje > 25 & puntaje <= 30.56, 1, 0),
    score_3035 = if_else(puntaje > 30.56 & puntaje <= 35, 1, 0),
    score_3540 = if_else(puntaje > 35 & puntaje <= 40, 1, 0),
    score_4245 = if_else(puntaje > 40 & puntaje <= 45, 1, 0),
    score_45m = if_else(puntaje > 45, 1, 0),
    # Personal information
    estrato_1 = if_else(estrato == 1, 1, 0),
    estrato_2 = if_else(estrato == 2, 1, 0),
    estrato_3 = if_else(estrato == 3, 1, 0),
    estrato_4 = if_else(estrato == 4, 1, 0),
    estrato_5 = if_else(estrato == 5, 1, 0),
    estrato_6 = if_else(estrato == 6, 1, 0),
    activi_sin = if_else(activi == 0, 1, 0),
    activi_tra = if_else(activi == 1, 1, 0),
    activi_bus = if_else(activi == 2, 1, 0),
    activi_est = if_else(activi == 3, 1, 0),
    activi_hog = if_else(activi == 4, 1, 0),
    estcivil_unlibr = if_else(estcivil == 1, 1, 0),
    estcivil_casado = if_else(estcivil == 2, 1, 0),
    estcivil_solter = if_else(estcivil == 4, 1, 0),
    anios_educacion = case_when(
      # Primaria
      nivel == 3 & (grado == 0 | is.na(grado)) ~ 0,
      nivel == 3 & grado == 1 ~ 1,
      nivel == 3 & grado == 2 ~ 2,
      nivel == 3 & grado == 3 ~ 3,
      nivel == 3 & grado == 4 ~ 4,
      nivel == 3 & grado == 5 ~ 5,
      
      # Secundaria
      nivel == 2 & (grado == 0 | is.na(grado)) ~ 5,
      nivel == 2 & (grado == 6 | grado == 1) ~ 6,
      nivel == 2 & (grado == 7 | grado == 2) ~ 7,
      nivel == 2 & (grado == 8 | grado == 3) ~ 8,
      nivel == 2 & (grado == 9 | grado == 4) ~ 9,
      nivel == 2 & (grado == 10 | grado == 5) ~ 10,
      nivel == 2 & (grado == 11 | grado == 6) ~ 11,
      
      # Técnica o tecnología
      nivel == 3 ~ 12,
      
      # Universitaria
      nivel == 4 ~ 13,
      
      # Postgrado
      nivel == 5 ~ 14,
      
      # Valor por defecto
      TRUE ~ 0
    )    
  )



# ----

# 2. Basic information from master data ----------------------------------------

# General descriptives for continuous variables
descriptivas_continuas <- master %>%
  select(puntaje, edad, estrato, anios_educacion, ingresos) %>%
  summarise_all(list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  ))

# Dummy variables - counters
conteo_dummys <- master %>%
  summarise(across(c(genero, score_m15, score_1520, score_2025, score_2530, score_3035, score_3540, score_4245, score_45m, 
                     estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
                     activi_sin, activi_tra, activi_bus, activi_est, activi_hog,
                     estcivil_unlibr, estcivil_casado, estcivil_solter), 
                   ~ sum(., na.rm = TRUE)))

# Analysis by gender and age groups

conteo_dummys_genero <- master %>%
  group_by(genero) %>%
  summarise(across(c(score_m15, score_1520, score_2025, score_2530, score_3035, score_3540, score_4245, score_45m, 
                     estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
                     activi_sin, activi_tra, activi_bus, activi_est, activi_hog,
                     estcivil_unlibr, estcivil_casado, estcivil_solter), 
                   ~ sum(., na.rm = TRUE))) %>%
  mutate(genero = if_else(genero == 1, "Hombres", "Mujeres"))

# Conteo de variables dummy por género y grupos de edad
conteo_dummys_edad_genero <- master %>%
  group_by(genero, edad_group = case_when(
    edad >= 18 & edad <= 24 ~ "18-24",
    edad > 24 & edad <= 55 ~ "25-55",
    edad > 55 ~ "55+",
    TRUE ~ "Otro" # En caso de que haya edades fuera del rango esperado
  )) %>%
  summarise(across(c(score_m15, score_1520, score_2025, score_2530, score_3035, score_3540, score_4245, score_45m, 
                     estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
                     activi_sin, activi_tra, activi_bus, activi_est, activi_hog,
                     estcivil_unlibr, estcivil_casado, estcivil_solter), 
                   ~ sum(., na.rm = TRUE))) %>%
  mutate(genero = if_else(genero == 1, "Hombres", "Mujeres"))

# Export results
write_parquet(descriptivas_continuas,    file.path(output_folder, "parquet/20240905-stats_set1_step1.parquet"))
write_parquet(conteo_dummys,             file.path(output_folder, "parquet/20240905-stats_set1_step2.parquet"))
write_parquet(conteo_dummys_genero,      file.path(output_folder, "parquet/20240905-stats_set1_step3.parquet"))
write_parquet(conteo_dummys_edad_genero, file.path(output_folder, "parquet/20240905-stats_set1_step4.parquet"))

# Clean up by removing all created dataframes
rm(descriptivas_continuas, conteo_dummys, conteo_dummys_genero, conteo_dummys_edad_genero)

# ----

# 3. Histograms from master data -----------------------------------------------

# .... Step 1: Create all the samples ....

# Total sample (no filters)
sample_total <- master %>% collect()

# Total sample (25-45 pts)
sample_total_s2545 <- master %>% filter(puntaje>=25 & puntaje<=45) %>%  collect()

# Sample by gender - Male
sample_male <- master %>% filter(genero == 1) %>% collect()

# Sample by gender - Female
sample_female <- master %>% filter(genero == 0) %>% collect()

# Define age groups (18-24, 25-55, 55+)
master <- master %>%
  mutate(edad_group = case_when(
    edad >= 18 & edad <= 24 ~ "18-24",
    edad > 24 & edad <= 55 ~ "25-55",
    edad > 55 ~ "55+",
    TRUE ~ NA_character_
  ))

# Sample by age group - 18-24
sample_1824 <- master %>% filter(edad_group == "18-24") %>% collect()

# Sample by age group - 25-55
sample_2555 <- master %>% filter(edad_group == "25-55") %>% collect()

# Sample by age group - 55+
sample_m55 <- master %>% filter(edad_group == "55+") %>% collect()

# Sample by gender and age group - Male in 18-24
sample_male_1824 <- master %>% filter(genero == 1 & edad_group == "18-24") %>% collect()

# Sample by gender and age group - Female in 18-24
sample_female_1824 <- master %>% filter(genero == 0 & edad_group == "18-24") %>% collect()

# Sample by gender and age group - Male in 25-55
sample_male_2555 <- master %>% filter(genero == 1 & edad_group == "25-55") %>% collect()

# Sample by gender and age group - Female in 25-55
sample_female_2555 <- master %>% filter(genero == 0 & edad_group == "25-55") %>% collect()

# Sample by gender and age group - Male 55+
sample_male_55m <- master %>% filter(genero == 1 & edad_group == "55+") %>% collect()

# Sample by gender and age group - Female 55+
sample_female_55m <- master %>% filter(genero == 0 & edad_group == "55+") %>% collect()

# .... Step 2: Create histograms with frequency and density ....

# Function to create histogram with both frequency and density
create_histogram_density <- function(data, variable, title, xlab, figure_filename) {
  p <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
    geom_density(aes(y = ..density.. * nrow(data)), color = "purple") +
    geom_vline(xintercept = 30.56, color = "red", linetype = "dashed") +  # Línea roja punteada
    labs(title = title, x = xlab, y = "Frequency / Density") +
    theme_minimal()
  
  # Save the plot
  ggsave(file.path(figure_folder, figure_filename), plot = p, width = 8, height = 6)
  print(p)
}


# Histograms with frequency and density for different samples
create_histogram_density(sample_total,       "puntaje", "", "Score", "20240905-histogram_total.png")
create_histogram_density(sample_male,        "puntaje", "", "Score", "20240905-histogram_male.png")
create_histogram_density(sample_female,      "puntaje", "", "Score", "20240905-histogram_female.png")
create_histogram_density(sample_total_s2545, "puntaje", "", "Score", "20240905-histogram_total_s1545.png")
create_histogram_density(sample_1824,        "puntaje", "", "Score", "20240905-histogram_1824.png")
create_histogram_density(sample_2555,        "puntaje", "", "Score", "20240905-histogram_2555.png")
create_histogram_density(sample_m55,         "puntaje", "", "Score", "20240905-histogram_55plus.png")
create_histogram_density(sample_male_1824,   "puntaje", "", "Score", "20240905-histogram_male_1824.png")
create_histogram_density(sample_female_1824, "puntaje", "", "Score", "20240905-histogram_female_1824.png")
create_histogram_density(sample_male_2555,   "puntaje", "", "Score", "20240905-histogram_male_2555.png")
create_histogram_density(sample_female_2555, "puntaje", "", "Score", "20240905-histogram_female_2555.png")
create_histogram_density(sample_male_55m,    "puntaje", "", "Score", "20240905-histogram_male_55plus.png")
create_histogram_density(sample_female_55m,  "puntaje", "", "Score", "20240905-histogram_female_55plus.png")

# .... Step 3: Clean up by removing all created dataframes ....

rm(sample_total, sample_male, sample_total_s2545, sample_female, sample_1824, sample_2555, sample_m55,
   sample_male_1824, sample_female_1824, sample_male_2555, sample_female_2555, 
   sample_male_55m, sample_female_55m)


# ----

# Time series from PANEL (complete data) ---------------------------------------


# Crear un data frame vacío para guardar los resultados
resultados <- data.frame()

# Main base
main_base <- main_base %>% 
  mutate(score_1520 = if_else(puntaje>15    & puntaje<=20, 1, 0),
         score_2025 = if_else(puntaje>20    & puntaje<=25, 1, 0),
         score_2530 = if_else(puntaje>25    & puntaje<=30.56, 1, 0),
         score_3035 = if_else(puntaje>30.56 & puntaje<=35, 1, 0),
         score_3540 = if_else(puntaje>35    & puntaje<=40, 1, 0),
         score_4245 = if_else(puntaje>40    & puntaje<=45, 1, 0)
  ) %>% collect()

# Función auxiliar para calcular sumas o promedios para subgrupos
calcular_subgrupos <- function(data, var_name, tipo_calculo) {
  if (tipo_calculo == "conteo") {
    return(data %>%
             summarise(
               total   = sum(!!sym(var_name), na.rm = TRUE),
               male    = sum(!!sym(var_name) * (genero == 1), na.rm = TRUE),
               female  = sum(!!sym(var_name) * (genero == 0), na.rm = TRUE),
               s1520   = sum(!!sym(var_name) * (score_1520 == 1), na.rm = TRUE),
               s2025   = sum(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
               s2530   = sum(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
               s3035   = sum(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
               s3540   = sum(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
               s4245   = sum(!!sym(var_name) * (score_4245 == 1), na.rm = TRUE),
               s1520_m = sum(!!sym(var_name) * (score_1520 == 1 & genero == 1), na.rm = TRUE),
               s1520_f = sum(!!sym(var_name) * (score_1520 == 1 & genero == 0), na.rm = TRUE),
               s2025_m = sum(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
               s2025_f = sum(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
               s2530_m = sum(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
               s2530_f = sum(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
               s3035_m = sum(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
               s3035_f = sum(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
               s3540_m = sum(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
               s3540_f = sum(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
               s4245_m = sum(!!sym(var_name) * (score_4245 == 1 & genero == 1), na.rm = TRUE),
               s4245_f = sum(!!sym(var_name) * (score_4245 == 1 & genero == 0), na.rm = TRUE)
             ))
  } else if (tipo_calculo == "continuo") {
    return(data %>%
             summarise(
               promedio = mean(!!sym(var_name), na.rm = TRUE),
               male     = mean(!!sym(var_name) * (genero == 1), na.rm = TRUE),
               female   = mean(!!sym(var_name) * (genero == 0), na.rm = TRUE),
               s1520    = mean(!!sym(var_name) * (score_1520 == 1), na.rm = TRUE),
               s2025    = mean(!!sym(var_name) * (score_2025 == 1), na.rm = TRUE),
               s2530    = mean(!!sym(var_name) * (score_2530 == 1), na.rm = TRUE),
               s3035    = mean(!!sym(var_name) * (score_3035 == 1), na.rm = TRUE),
               s3540    = mean(!!sym(var_name) * (score_3540 == 1), na.rm = TRUE),
               s4245    = mean(!!sym(var_name) * (score_4245 == 1), na.rm = TRUE),
               s1520_m  = mean(!!sym(var_name) * (score_1520 == 1 & genero == 1), na.rm = TRUE),
               s1520_f  = mean(!!sym(var_name) * (score_1520 == 1 & genero == 0), na.rm = TRUE),
               s2025_m  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 1), na.rm = TRUE),
               s2025_f  = mean(!!sym(var_name) * (score_2025 == 1 & genero == 0), na.rm = TRUE),
               s2530_m  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 1), na.rm = TRUE),
               s2530_f  = mean(!!sym(var_name) * (score_2530 == 1 & genero == 0), na.rm = TRUE),
               s3035_m  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 1), na.rm = TRUE),
               s3035_f  = mean(!!sym(var_name) * (score_3035 == 1 & genero == 0), na.rm = TRUE),
               s3540_m  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 1), na.rm = TRUE),
               s3540_f  = mean(!!sym(var_name) * (score_3540 == 1 & genero == 0), na.rm = TRUE),
               s4245_m  = mean(!!sym(var_name) * (score_4245 == 1 & genero == 1), na.rm = TRUE),
               s4245_f  = mean(!!sym(var_name) * (score_4245 == 1 & genero == 0), na.rm = TRUE)
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

# Variables de conteo
variables_conteo <- c("n_visitas_rips", "n_consultas", "n_hospitalizaciones", 
                      "n_procedimientos", "n_urgencias", "c_preven", "c_prenat", 
                      "d_registros_pila", "pila_depen", "pila_indep")

# Variables continuas
variables_continuas <- c("ibc_salud", "sal_dias_cot_sum", "sal_dias_cot_max")

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
  resume_resultados <- if (nrow(resultados) == 0) temp_df else resultados %>% left_join(temp_df, by = "monthly_date")
}


# Función para contar personas únicas con al menos un registro en el mes
calcular_unicos <- function(data, var_name) {
  data %>%
    filter(!!sym(var_name) == 1) %>%
    group_by(monthly_date) %>%
    summarise(
      total  = n_distinct(personabasicaid), # Total de personas únicas
      male   = n_distinct(personabasicaid[genero == 1]), # Hombres
      female = n_distinct(personabasicaid[genero == 0]), # Mujeres
      edad_18_24 = n_distinct(personabasicaid[edad >= 18 & edad < 25]), # Grupo de edad 18-24
      edad_25_55 = n_distinct(personabasicaid[edad >= 25 & edad <= 55]), # Grupo de edad 25-55
      edad_55_mas = n_distinct(personabasicaid[edad > 55]), # Grupo de edad 55+
      score_25_45 = n_distinct(personabasicaid[puntaje >= 25 & puntaje <= 45]), # Puntaje entre 25 y 45
      mujeres_25_55_score = n_distinct(personabasicaid[genero == 0 & edad >= 25 & edad <= 55 & puntaje >= 25 & puntaje <= 45]), # Mujeres de 25-55 años y puntaje 25-45
      hombres_25_55_score = n_distinct(personabasicaid[genero == 1 & edad >= 25 & edad <= 55 & puntaje >= 25 & puntaje <= 45]) # Mujeres de 25-55 años y puntaje 25-45      
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
  unique_resultados <- if (nrow(resultados) == 0) temp_df_unicos else resultados %>% left_join(temp_df_unicos, by = "monthly_date")
}

# Guardar los resultados
write_parquet(resume_resultados, file.path(output_folder, "parquet/20240905-stats_set1_step5.parquet"))
write_parquet(unique_resultados, file.path(output_folder, "parquet/20240905-stats_set1_step6.parquet"))












