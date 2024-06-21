paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


base_dir <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")

# ----
# 1. Definitions and functions -----------------------------------------------------

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
run_estimations <- function(data, var_name, cutoff_var, df_name) {
  # Definir ventana de tiempo
  years <- 2012:2019
  quarters <- 1:4
  
  # Dataframe vacío para guardar resultados
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
  
  # Bucle anidado para cada año y trimestre
  for (year in years) {
    for (qtr in quarters) {
      # Filtrar los datos para el trimestre y año específicos
      data_subset <- data %>%
        filter(year(quarterly_date) == year & quarter(quarterly_date) == qtr) %>%
        drop_na(all_of(c(var_name, cutoff_var)))
      
      # Verificar si hay suficientes datos para realizar la estimación
      if (nrow(data_subset) > 20) {  # Ajusta el umbral según sea necesario
        # Imprimir información actual
        cat("Variable:", var_name, "Cutoff:", cutoff_var, "Trimestre:", paste0(year, "q", qtr), "Hora:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
        
        # Realizar la estimación
        est <- tryCatch(
          {
            rdrobust(data_subset[[var_name]], data_subset[[cutoff_var]], all=TRUE)
          },
          error = function(e) {
            message(paste("Error en el año", year, "trimestre", qtr, ":", e$message))
            return(NULL)
          }
        )
        
        if (!is.null(est)) {
          # Extraer valores correspondientes a la estimación robusta
          coef_robust <- est$coef["Robust", ]
          se_robust <- est$se["Robust", ]
          pv_robust <- est$pv["Robust", ]
          ci_lower_robust <- est$ci["Robust", "CI Lower"]
          ci_upper_robust <- est$ci["Robust", "CI Upper"]
          
          # Obtener tamaños de muestra
          N1 <- est$N[1]
          N2 <- est$N[2]
          
          # Crear un dataframe temporal con los resultados actuales
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
          
          # Añadir los resultados actuales al dataframe de resultados
          results_df <- rbind(results_df, temp_df)
        }
      } else {
        message(paste("Insuficientes datos para el año", year, "trimestre", qtr))
      }
    }
  }
  
  # Asignar el dataframe resultante al nombre especificado
  assign(df_name, results_df, envir = .GlobalEnv)
}




# ----
# 2. Initial databases ------------------------------------------------------------

# Databases - SISBEN and MASTER
all_sisben_bgta <- open_dataset(sprintf('%s/%s', project_folder, 'data/sisben3_bgta_demog.parquet'))
master          <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))

# Databases - PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
all_pila          <- open_dataset(PILA_history_file) 
main_pila         <- open_dataset(PILA_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  filter(!is.na(puntaje))
  
#  filter(!is.na(puntaje))

#  rename(puntaje = puntaje.x) %>% 
#  filter(!is.na(puntaje))

# ----

# 3. Modified databases to estimations -----------------------------------------

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
    estrato_6 = if_else(estrato == 6, 1, 0)    
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
    tipo_cotizante  = max(tipo_cotizante, na.rm = TRUE),
    n_registros = n(),
    d_registros = if_else(n() > 0, 1, 0),
    ibc_salud     = max(ibc_salud, na.rm = TRUE),
    sal_dias_cot  = sum(sal_dias_cot, na.rm = TRUE)
  )

pila_pbid <- pila_pbid %>% 
  filter(year>=2012 & year<=2019) %>%   
  mutate(
    asalariado  = if_else(tipo_cotizante == 1, 1, 0)
  )

pila_pbid <- pila_pbid %>% 
  collect

#set.seed(123)
#pila_pbid <- pila_pbid %>% 
#  sample_frac(0.001)

gc()

pila_pbid <- pila_pbid %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode,
  ) %>% 
  categorize_age_pila()

# ----

# 4. Balancing panel------------------------------------------------------------

pila_est <- pila_pbid %>%
  mutate(quarterly_date = as.Date(quarterly_date)) %>% 
  centrar_puntaje()

rm(pila_pbid)
gc()

# Skeleton with all individuals observed in all periods
aux_time <- seq(as.Date("2012-01-01"), as.Date("2019-12-01"), by = "quarter")
aux_pbid <- unique(pila_est$personabasicaid)
skeleton <- expand.grid(quarterly_date = aux_time, personabasicaid = aux_pbid)

# Balanced dataframe with the original
pila_est_balanced <- merge(skeleton, pila_est, by = c("quarterly_date", "personabasicaid"), all.x = TRUE)

# Cleaning memory
rm(pila_est, skeleton)
gc()

# Replacing NA's to zero in balance variables
pila_est_balanced <- pila_est_balanced %>%
  group_by(personabasicaid) %>%
  fill(puntaje, genero, estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, cutoff30, cutoff40, asalariado, .direction = "downup") %>%
  ungroup()

# Replacing NA's to zero in main variables
pila_est_balanced <- pila_est_balanced %>%
  mutate(across(c(n_registros, d_registros, ibc_salud, sal_dias_cot),
                ~ replace_na(., 0)))

# 5. Estimations ---------------------------------------------------------------

# Using the cutoff=30
run_estimations(pila_est_balanced, "n_registros",    "cutoff30", "pila30_registros")
run_estimations(pila_est_balanced, "d_registros",    "cutoff30", "pila30_d_registros")
run_estimations(pila_est_balanced, "ibc_salud" ,     "cutoff30", "pila30_ibc_salud")
run_estimations(pila_est_balanced, "sal_dias_cot",   "cutoff30", "pila30_sal_dias_cot")
run_estimations(pila_est_balanced, "asalariado",     "cutoff30", "pila30_asalariado")

# Using the cutoff=40
run_estimations(pila_est_balanced, "n_registros",    "cutoff40", "pila40_registros")
run_estimations(pila_est_balanced, "d_registros",    "cutoff40", "pila40_d_registros")
run_estimations(pila_est_balanced, "ibc_salud" ,     "cutoff40", "pila40_ibc_salud")
run_estimations(pila_est_balanced, "sal_dias_cot",   "cutoff40", "pila40_sal_dias_cot")
run_estimations(pila_est_balanced, "asalariado",     "cutoff40", "pila40_asalariado")

# Crear una lista de todos los dataframes
list_est_pila30 <- list(
  pila30_registros, pila30_d_registros,
  pila30_ibc_salud, pila30_sal_dias_cot, pila30_asalariado
)
list_est_pila40 <- list(
  pila40_registros, pila40_d_registros,
  pila40_ibc_salud, pila40_sal_dias_cot, pila40_asalariado
)

# Combinar todos los dataframes en uno solo
est_pila30 <- bind_rows(list_est_pila30)
est_pila40 <- bind_rows(list_est_pila40)

# Guardar el dataframe combinado en formato .parquet
write_parquet(est_pila30, file.path(output_folder, "parquet/20240620-est_pila30.parquet"))
write_parquet(est_pila40, file.path(output_folder, "parquet/20240620-est_pila40.parquet"))


estimaciones_list_objects <- c(
  "pila30_registros", "pila30_d_registros", "pila30_ibc_salud", "pila30_sal_dias_cot","pila30_asalariado",
  "pila40_registros", "pila40_d_registros", "pila40_ibc_salud", "pila40_sal_dias_cot","pila40_asalariado"
)

# Eliminar los objetos del entorno global
rm(list = estimaciones_list_objects, envir = .GlobalEnv)














