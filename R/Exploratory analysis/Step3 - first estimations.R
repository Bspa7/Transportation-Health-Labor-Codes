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
run_estimations <- function(data, var_name, df_name) {
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
      est <- rdrobust(data_subset[[var_name]], data_subset$cutoff30, all=TRUE)
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

# 3.1. Databases to estimations (rdrobust)

# RIPS: creating a monthly panel at personabasicaid-month level outcomes
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
    puntaje   = max(puntaje),    
    genero    = max(genero),
    estrato_1 = max(estrato_1),
    estrato_2 = max(estrato_2),
    estrato_3 = max(estrato_3),
    estrato_4 = max(estrato_4),
    estrato_5 = max(estrato_5),
    estrato_6 = max(estrato_6),
    educacion = max(anios_educacion),
    n_visitas = n(),
    n_consultas         = sum(MODULE == "c"),
    n_hospitalizaciones = sum(MODULE == "h"),
    n_procedimientos    = sum(MODULE == "p"),
    n_urgencias         = sum(MODULE == "u"),    
  )

# --


rips_pbid <- rips_pbid %>%
  mutate(quarterly_date = as.Date(quarterly_date))

rips_pbid <- rips_pbid %>% 
  collect

rips_pbid <- rips_pbid %>% 
  centrar_puntaje()






# 4. ESTIMATES -----------------------------------------------------------------

# RIPS - estimates using the function
run_estimations(rips_pbid, "n_hospitalizaciones", "df_hospitalizaciones")
run_estimations(rips_pbid, "n_urgencias", "df_urgencias")


# PILA - estimates using the function



# 5. Saving results ------------------------------------------------------------






















































# ----


# 4. RIPS-SISBEN: estimations --------------------------------------------------

# ----

# 5. RIPS-SISBEN: estimations -------------------------------------------------

# ----

# 6. Saving results ------------------------------------------------------------

# ----



