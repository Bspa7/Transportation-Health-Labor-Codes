paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


#base_dir <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")

# ----

# 1. Definitions and functions -----------------------------------------------------


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

# Define a variable to categorize age into group - PILA
categorize_age_pila <- function(df) {
  df <- df %>% 
    mutate(
      grupo_edad = case_when(
        between(edad_PILA , 18, 26) ~ 1,
        between(edad_PILA , 27, 59) ~ 2,
        edad_PILA >= 60 ~ 3      
      )
    )
  return(df)
}

# Define a function to create the running variables centered in zero
centrar_puntaje <- function(df) {
  df <- df %>% 
    mutate(
      cutoff30 = puntaje - 30.56,
      cutoff40 = puntaje - 40      
    )
  return(df)
}

# Define a function to create all set of estimations
# Define a function to create all set of estimations in cutoff=30
run_estimations <- function(data, var_name, cutoff_var, df_name) {
  # Define time window
  years <- 2013:2019
  quarters <- 1:4
  
  # Empty data frame to save results
  results_df <- data.frame(
    coef_robust = numeric(),
    se_robust = numeric(),
    pv_robust = numeric(),
    ci_lower_robust = numeric(),
    ci_upper_robust = numeric(),
    N1 = numeric(),
    N2 = numeric(),
    period = character(),
    variable = character(),
    stringsAsFactors = FALSE
  )
  
  # Nested loop for each year and quarter
  for (year in years) {
    for (qtr in quarters) {
      # Filter the data for the specific quarter and year
      data_subset <- data %>%
        filter(year(quarterly_date) == year & quarter(quarterly_date) == qtr)
      
      # Make the estimate
      est <- rdrobust(data_subset[[var_name]], data_subset[[cutoff_var]], all=TRUE)
      summary(est)
      
      # Extract values corresponding to the robust estimate
      coef_robust <- est$coef["Robust", ]
      se_robust <- est$se["Robust", ]
      pv_robust <- est$pv["Robust", ]
      ci_lower_robust <- est$ci["Robust", "CI Lower"]
      ci_upper_robust <- est$ci["Robust", "CI Upper"]
      
      # Get sample sizes
      N1 <- est$N[1]
      N2 <- est$N[2]
      
      # Create a temporary data frame with the current results
      temp_df <- data.frame(
        coef_robust = coef_robust,
        se_robust = se_robust,
        pv_robust = pv_robust,
        ci_lower_robust = ci_lower_robust,
        ci_upper_robust = ci_upper_robust,
        N1 = N1,
        N2 = N2,
        period = paste0(year, "q", qtr),
        variable = var_name,
        stringsAsFactors = FALSE
      )
      
      # Add the current results to the results data frame
      results_df <- rbind(results_df, temp_df)
    }
  }
  
  # Assign the resulting data frame to the specified name
  assign(df_name, results_df, envir = .GlobalEnv)
}




# ----

# 2. Initial databases ------------------------------------------------------------

# Databases - SISBEN and MASTER
all_sisben_bgta <- open_dataset(sprintf('%s/%s', project_folder, 'data/sisben3_bgta_demog.parquet'))
master          <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))

# Databases - RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')
all_rips          <- open_dataset(RIPS_history_file)
main_rips         <- open_dataset(RIPS_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  rename(COD_MPIO = COD_MPIO.x, COD_DPTO = COD_DPTO.x, puntaje = puntaje.x, sexo = sexo.x, estrato = estrato.x, nivel=nivel.x, grado=grado.x) %>% 
  filter(!is.na(puntaje))

# Databases - PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
all_pila          <- open_dataset(PILA_history_file) 
main_pila         <- open_dataset(PILA_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  rename(puntaje = puntaje.x) %>% 
  filter(!is.na(puntaje))

# ----


# 3. Modified databases to estimations -----------------------------------------

# 3.1. RIPS: panel at personabasicaid-quarter level outcomes -------------------
rips_pbid <- main_rips %>%
  filter(COD_DPTO == 11 & COD_MPIO == 1) %>%
  categorize_age() %>%
  mutate(
    DATE = as.Date(DATE),
    quarterly_date = floor_date(DATE, "quarter"),
    year = year(DATE)
  )

rips_pbid <- rips_pbid %>%
  mutate(
    genero    = if_else(sexo == 1, 1, 0),
    estrato_1 = if_else(estrato == 1, 1, 0),
    estrato_2 = if_else(estrato == 2, 1, 0),    
    estrato_3 = if_else(estrato == 3, 1, 0),
    estrato_4 = if_else(estrato == 4, 1, 0),
    estrato_5 = if_else(estrato == 5, 1, 0),
    estrato_6 = if_else(estrato == 6, 1, 0),    
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
  ) %>% 
  group_by(quarterly_date, personabasicaid) %>%  
  summarise(
    puntaje   = max(puntaje, na.rm = TRUE),    
    genero    = max(genero,  na.rm = TRUE),
    estrato_1 = max(estrato_1, na.rm = TRUE),
    estrato_2 = max(estrato_2, na.rm = TRUE),
    estrato_3 = max(estrato_3, na.rm = TRUE),
    estrato_4 = max(estrato_4, na.rm = TRUE),
    estrato_5 = max(estrato_5, na.rm = TRUE),
    estrato_6 = max(estrato_6, na.rm = TRUE),
    educacion = max(anios_educacion, na.rm = TRUE),
    edad_RIPS = max(edad_RIPS, na.rm = TRUE),
    n_visitas = n(),
    n_consultas         = sum(MODULE == "c", na.rm = TRUE),
    n_hospitalizaciones = sum(MODULE == "h", na.rm = TRUE),
    n_procedimientos    = sum(MODULE == "p", na.rm = TRUE),
    n_urgencias         = sum(MODULE == "u", na.rm = TRUE),    
    c_cancer = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "C", 1, 0), na.rm = TRUE),
    c_cardio = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "I", 1, 0), na.rm = TRUE),    
    c_preven = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 0:139), 1, 0), na.rm = TRUE),
    c_prenat = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE),  
    c_respir = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "J" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 401:459), 1, 0), na.rm = TRUE)
      )


# ----

# 3.2. PILA: panel at personabasicaid-quarter level outcomes -------------------

pila_pbid <- main_pila %>%
  filter(depto_cod == 11 & ciudad_cod == 1) %>%
  mutate(
    DATE = as.Date(DATE),
    quarterly_date = floor_date(DATE, "quarter"),
    year = year(DATE),
    tipo_cotizante = case_when(
      tipo_cotiz == 1 ~ 1,
      tipo_cotiz == 3 ~ 2,
      (tipo_cotiz != 1 & tipo_cotiz != 3) ~ 3,
      is.na(tipo_cotiz) ~ 3),
      genero    = if_else(sexomode == 1, 1, 0),
      estrato_1 = if_else(estrato == 1, 1, 0),
      estrato_2 = if_else(estrato == 2, 1, 0),    
      estrato_3 = if_else(estrato == 3, 1, 0),
      estrato_4 = if_else(estrato == 4, 1, 0),
      estrato_5 = if_else(estrato == 5, 1, 0),
      estrato_6 = if_else(estrato == 6, 1, 0),    
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
  ) %>% 
  group_by(quarterly_date, personabasicaid) %>%  
  summarise(
    year      = max(year, na.rm = TRUE),
    yearmode  = max(yearmode, na.rm = TRUE),    
    puntaje   = max(puntaje, na.rm = TRUE),    
    genero    = max(genero, na.rm = TRUE),
    estrato_1 = max(estrato_1, na.rm = TRUE),
    estrato_2 = max(estrato_2, na.rm = TRUE),
    estrato_3 = max(estrato_3, na.rm = TRUE),
    estrato_4 = max(estrato_4, na.rm = TRUE),
    estrato_5 = max(estrato_5, na.rm = TRUE),
    estrato_6 = max(estrato_6, na.rm = TRUE),
    educacion = max(anios_educacion, na.rm = TRUE),
    tipo_cotizante  = max(tipo_cotizante, na.rm = TRUE),
    n_registros = n(),
    ibc_salud     = max(ibc_salud, na.rm = TRUE),
    sal_dias_cot  = sum(sal_dias_cot, na.rm = TRUE)
  )

pila_pbid <- pila_pbid %>% 
  collect

gc()

pila_pbid <- pila_pbid %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode,
  ) %>% 
  categorize_age_pila()

# ----


# 4. ESTIMATES -----------------------------------------------------------------

rips_est <- rips_pbid %>%
  mutate(quarterly_date = as.Date(quarterly_date)) %>% 
  centrar_puntaje() %>% 
  collect

pila_est <- pila_pbid %>%
  mutate(quarterly_date = as.Date(quarterly_date)) %>% 
  centrar_puntaje()

rm(rips_pbid, pila_pbid)
gc()

pila_est <- pila_est %>% 
  mutate(
    asalariado  = if_else(tipo_cotizante == 1, 1, 0)
  )

# RIPS

run_estimations(rips_est, "genero", "cutoff30", "rips30_genero")
run_estimations(rips_est, "estrato_1", "cutoff30", "rips30_estrato1")
run_estimations(rips_est, "estrato_2", "cutoff30", "rips30_estrato2")
run_estimations(rips_est, "estrato_3", "cutoff30", "rips30_estrato3")
run_estimations(rips_est, "estrato_4", "cutoff30", "rips30_estrato4")
run_estimations(rips_est, "estrato_5", "cutoff30", "rips30_estrato5")
run_estimations(rips_est, "estrato_6", "cutoff30", "rips30_estrato6")
run_estimations(rips_est, "educacion", "cutoff30", "rips30_estrato7")
run_estimations(rips_est, "edad_RIPS", "cutoff30", "rips30_edad")
run_estimations(rips_est, "n_visitas", "cutoff30", "rips30_visitas")
run_estimations(rips_est, "c_preven" , "cutoff30", "rips30_c_prev")
run_estimations(rips_est, "c_prenat" , "cutoff30", "rips30_c_pren")
run_estimations(rips_est, "c_cancer" , "cutoff30", "rips30_c_cancer")
run_estimations(rips_est, "c_cardio" , "cutoff30", "rips30_c_cardio")
run_estimations(rips_est, "c_respir" , "cutoff30", "rips30_c_respir")
run_estimations(rips_est, "n_consultas", "cutoff30", "rips30_consultas")
run_estimations(rips_est, "n_hospitalizaciones", "cutoff30", "rips30_hosp")
run_estimations(rips_est, "n_procedimientos", "cutoff30", "rips30_proc")
run_estimations(rips_est, "n_urgencias", "cutoff30", "rips30_urgencias")


run_estimations(rips_est, "genero", "cutoff40", "rips40_genero")
run_estimations(rips_est, "estrato_1", "cutoff40", "rips40_estrato1")
run_estimations(rips_est, "estrato_2", "cutoff40", "rips40_estrato2")
run_estimations(rips_est, "estrato_3", "cutoff40", "rips40_estrato3")
run_estimations(rips_est, "estrato_4", "cutoff40", "rips40_estrato4")
run_estimations(rips_est, "estrato_5", "cutoff40", "rips40_estrato5")
run_estimations(rips_est, "estrato_6", "cutoff40", "rips40_estrato6")
run_estimations(rips_est, "educacion", "cutoff40", "rips40_estrato7")
run_estimations(rips_est, "edad_RIPS", "cutoff40", "rips40_edad")
run_estimations(rips_est, "n_visitas", "cutoff40", "rips40_visitas")
run_estimations(rips_est, "c_preven" , "cutoff40", "rips40_c_prev")
run_estimations(rips_est, "c_prenat" , "cutoff40", "rips40_c_pren")
run_estimations(rips_est, "c_cancer" , "cutoff40", "rips40_c_cancer")
run_estimations(rips_est, "c_cardio" , "cutoff40", "rips40_c_cardio")
run_estimations(rips_est, "c_respir" , "cutoff40", "rips40_c_respir")
run_estimations(rips_est, "n_consultas", "cutoff40", "rips40_consultas")
run_estimations(rips_est, "n_hospitalizaciones", "cutoff40", "rips40_hosp")
run_estimations(rips_est, "n_procedimientos", "cutoff40", "rips40_proc")
run_estimations(rips_est, "n_urgencias", "cutoff40", "rips40_urgencias")


# PILA
run_estimations(pila_est, "genero", "cutoff30", "pila30_genero")
run_estimations(pila_est, "estrato_1", "cutoff30", "pila30_estrato1")
run_estimations(pila_est, "estrato_2", "cutoff30", "pila30_estrato2")
run_estimations(pila_est, "estrato_3", "cutoff30", "pila30_estrato3")
run_estimations(pila_est, "estrato_4", "cutoff30", "pila30_estrato4")
run_estimations(pila_est, "estrato_5", "cutoff30", "pila30_estrato5")
run_estimations(pila_est, "estrato_6", "cutoff30", "pila30_estrato6")
run_estimations(pila_est, "educacion", "cutoff30", "pila30_estrato7")
run_estimations(pila_est, "tipo_cotizante", "cutoff30", "pila30_tipo_czte")
run_estimations(pila_est, "n_registros",    "cutoff30", "pila30_registros")
run_estimations(pila_est, "ibc_salud" ,     "cutoff30", "pila30_ibc_salud")
run_estimations(pila_est, "sal_dias_cot" ,  "cutoff30", "pila30_sal_dias_cot")
run_estimations(pila_est, "edad_PILA" ,     "cutoff30", "pila30_edad_pila")


run_estimations(pila_est, "genero", "cutoff40", "pila40_genero")
run_estimations(pila_est, "estrato_1", "cutoff40", "pila40_estrato1")
run_estimations(pila_est, "estrato_2", "cutoff40", "pila40_estrato2")
run_estimations(pila_est, "estrato_3", "cutoff40", "pila40_estrato3")
run_estimations(pila_est, "estrato_4", "cutoff40", "pila40_estrato4")
run_estimations(pila_est, "estrato_5", "cutoff40", "pila40_estrato5")
run_estimations(pila_est, "estrato_6", "cutoff40", "pila40_estrato6")
run_estimations(pila_est, "educacion", "cutoff40", "pila40_estrato7")
run_estimations(pila_est, "n_registros",    "cutoff40", "pila40_registros")
run_estimations(pila_est, "ibc_salud" ,     "cutoff40", "pila40_ibc_salud")
run_estimations(pila_est, "sal_dias_cot" ,  "cutoff40", "pila40_sal_dias_cot")
run_estimations(pila_est, "edad_PILA" ,     "cutoff40", "pila40_edad_pila")
run_estimations(pila_est, "asalariado", "cutoff40", "pila40_asalariado")

# ----

print(Sys.time())











# ----



# RIPS - estimates using the function
run_estimations(rips_pbid, "n_hospitalizaciones", "df_hospitalizaciones")
run_estimations(rips_pbid, "n_urgencias", "df_urgencias")


# Ejemplo de uso:
# Suponiendo que df_urgencias es tu conjunto de datos y deseas calcular las estimaciones para la variable n_hospitalizaciones
run_estimations(df_urgencias, "n_hospitalizaciones", "cutoff30", "df_urgencias_estimations_cutoff30")

# Puedes llamar a la función múltiples veces para diferentes variables y cutoffs
run_estimations(df_urgencias, "n_urgencias", "cutoff40", "df_urgencias_estimations_cutoff40")
run_estimations(df_urgencias, "n_hospitalizaciones", "cutoff21", "df_urgencias_estimations_cutoff21")

# Para acceder a los resultados:
# df_urgencias_estimations_cutoff30
# df_urgencias_estimations_cutoff40
# df_urgencias_estimations_cutoff21

# PILA - estimates using the function



# 5. Saving results ------------------------------------------------------------






















































# ----


# 4. RIPS-SISBEN: estimations --------------------------------------------------

# ----

# 5. RIPS-SISBEN: estimations -------------------------------------------------

# ----

# 6. Saving results ------------------------------------------------------------

# ----



