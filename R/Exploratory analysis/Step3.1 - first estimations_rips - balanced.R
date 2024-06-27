paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "tictoc")
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

#%>% 
#  filter(!is.na(puntaje))

#%>% 
#  rename(COD_MPIO = COD_MPIO.x, COD_DPTO = COD_DPTO.x, puntaje = puntaje.x, sexo = sexo.x, estrato = estrato.x, nivel=nivel.x, grado=grado.x) %>% 
#  filter(!is.na(puntaje))

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
  filter(year>=2012 & year<=2019) %>% 
  mutate(
    genero    = if_else(sexo == 1, 1, 0),
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
  ) %>% 
  group_by(quarterly_date, personabasicaid) %>%  
  summarise(
    puntaje   = max(puntaje, na.rm = TRUE),    
    genero    = max(genero,  na.rm = TRUE),
    fechanto  = max(fechanto,  na.rm = TRUE),    
    estrato_1 = max(estrato_1, na.rm = TRUE),
    estrato_2 = max(estrato_2, na.rm = TRUE),
    estrato_3 = max(estrato_3, na.rm = TRUE),
    estrato_4 = max(estrato_4, na.rm = TRUE),
    estrato_5 = max(estrato_5, na.rm = TRUE),
    estrato_6 = max(estrato_6, na.rm = TRUE),
    activi_sin = max(activi_sin, na.rm = TRUE),
    activi_tra = max(activi_tra, na.rm = TRUE),
    activi_bus = max(activi_bus, na.rm = TRUE),    
    activi_est = max(activi_est, na.rm = TRUE),
    activi_hog = max(activi_hog, na.rm = TRUE),
    estcivil_unlibr = max(estcivil_unlibr, na.rm = TRUE),
    estcivil_casado = max(estcivil_casado, na.rm = TRUE),    
    estcivil_solter = max(estcivil_solter, na.rm = TRUE),
    anios_educacion = max(anios_educacion, na.rm = TRUE),  
    ingresos = max(ingresos, na.rm = TRUE),      
    edad_RIPS = max(edad_RIPS, na.rm = TRUE),
    n_visitas = n(),
    d_visitas = if_else(n() > 0, 1, 0),
    n_consultas = sum(MODULE == "c", na.rm = TRUE),
    d_consultas = if_else(sum(MODULE == "c", na.rm = TRUE) > 0, 1, 0),
    n_hospitalizaciones = sum(MODULE == "h", na.rm = TRUE),
    d_hospitalizaciones = if_else(sum(MODULE == "h", na.rm = TRUE) > 0, 1, 0),
    n_procedimientos = sum(MODULE == "p", na.rm = TRUE),
    d_procedimientos = if_else(sum(MODULE == "p", na.rm = TRUE) > 0, 1, 0),
    n_urgencias = sum(MODULE == "u", na.rm = TRUE),
    d_urgencias = if_else(sum(MODULE == "u", na.rm = TRUE) > 0, 1, 0),
    c_cancer = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "C", 1, 0), na.rm = TRUE),
    d_cancer = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "C", 1, 0), na.rm = TRUE) > 0, 1, 0),
    c_cardio = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "I", 1, 0), na.rm = TRUE),
    d_cardio = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "I", 1, 0), na.rm = TRUE) > 0, 1, 0),
    c_preven = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 0:139), 1, 0), na.rm = TRUE),
    d_preven = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 0:139), 1, 0), na.rm = TRUE) > 0, 1, 0),
    c_prenat = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE),
    d_prenat = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE) > 0, 1, 0),
    c_respir = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "J" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 401:459), 1, 0), na.rm = TRUE),
    d_respir = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "J" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 401:459), 1, 0), na.rm = TRUE) > 0, 1, 0)
  )


# ----


# 4. Balancing panel------------------------------------------------------------

rips_est <- rips_pbid %>%
  mutate(quarterly_date = as.Date(quarterly_date)) %>% 
  centrar_puntaje() %>% 
  collect

rips_est <- rips_est %>% 
  sample_frac(0.005)

rm(rips_pbid)
gc()

# -----------

# PROVISIONAL SAMPLE ------------------------------------------------------------

#write_parquet(rips_est, file.path(data_dir, "panel_prov_rips.parquet"))

rips_est <- open_dataset(sprintf('%s/%s', data_dir, "panel_prov_rips.parquet")) %>%  collect

# -----------

# Skeleton with all individuals observed in all periods
aux_time <- seq(as.Date("2012-01-01"), as.Date("2019-12-01"), by = "quarter")
aux_pbid <- unique(rips_est$personabasicaid)
skeleton <- expand.grid(quarterly_date = aux_time, personabasicaid = aux_pbid)

# Balanced dataframe with the original
rips_est_balanced <- merge(skeleton, rips_est, by = c("quarterly_date", "personabasicaid"), all.x = TRUE)

# Cleaning memory
rm(rips_est, skeleton)
gc()


# Replacing NA's to zero in balance variables
rips_est_balanced <- rips_est_balanced %>%
  group_by(personabasicaid) %>%
  fill(puntaje, genero, 
       estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
       activi_sin, activi_tra, activi_bus, activi_est, activi_hog, 
       estcivil_unlibr, estcivil_casado, estcivil_solter, anios_educacion,
       cutoff30, cutoff40, fechanto, .direction = "downup") %>%
  ungroup() %>%
  mutate(across(c(puntaje, genero, 
                  estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
                  activi_sin, activi_tra, activi_bus, activi_est, activi_hog, 
                  estcivil_unlibr, estcivil_casado, estcivil_solter, anios_educacion,
                  cutoff30, cutoff40), ~replace_na(., 0)))

# Replacing NA's to zero in main variables
rips_est_balanced <- rips_est_balanced %>%
  mutate(across(c(n_visitas, n_consultas, n_hospitalizaciones,
                  n_procedimientos, n_urgencias, c_cancer,
                  c_cardio, c_preven, c_prenat, c_respir,
                  d_visitas, d_consultas, d_hospitalizaciones,
                  d_procedimientos, d_urgencias, d_cancer,
                  d_cardio, d_preven, d_prenat, d_respir),
                ~ replace_na(., 0)))


# Crear la variable edad_sisben
rips_est_balanced <- rips_est_balanced %>%
  mutate(edad_sisben = floor(interval(fechanto, quarterly_date) / years(1)))

# Reordenar las columnas
rips_est_balanced <- rips_est_balanced %>%
  select(personabasicaid, quarterly_date, fechanto, edad_sisben, everything())




# 5. Estimations ---------------------------------------------------------------

# Changing the direction of the running variable | Interpretation
rips_est_balanced <-  rips_est_balanced %>% 
  mutate(cutoff30_origi = cutoff30,
         cutoff40_origi = cutoff40,
         cutoff30 = cutoff30*-1,
         cutoff40 = cutoff40*-1)

rips_est_balanced <- rips_est_balanced %>%
  select(personabasicaid, cutoff30, cutoff30_origi, cutoff40, cutoff40_origi, quarterly_date, fechanto, edad_sisben, everything())


# Using the cutoff=30
run_estimations(rips_est_balanced, "n_visitas", "cutoff30", "rips30_visitas")
run_estimations(rips_est_balanced, "c_preven" , "cutoff30", "rips30_c_prev")
run_estimations(rips_est_balanced, "c_prenat" , "cutoff30", "rips30_c_pren")
run_estimations(rips_est_balanced, "c_cancer" , "cutoff30", "rips30_c_cancer")
run_estimations(rips_est_balanced, "c_cardio" , "cutoff30", "rips30_c_cardio")
run_estimations(rips_est_balanced, "c_respir" , "cutoff30", "rips30_c_respir")
run_estimations(rips_est_balanced, "n_consultas", "cutoff30", "rips30_consultas")
run_estimations(rips_est_balanced, "n_hospitalizaciones", "cutoff30", "rips30_hosp")
run_estimations(rips_est_balanced, "n_procedimientos", "cutoff30", "rips30_proc")
run_estimations(rips_est_balanced, "n_urgencias", "cutoff30", "rips30_urgencias")
run_estimations(rips_est_balanced, "d_visitas", "cutoff30", "rips30_d_visitas")
run_estimations(rips_est_balanced, "d_consultas", "cutoff30", "rips30_d_consultas")
run_estimations(rips_est_balanced, "d_hospitalizaciones", "cutoff30", "rips30_d_hosp")
run_estimations(rips_est_balanced, "d_procedimientos", "cutoff30", "rips30_d_proc")
run_estimations(rips_est_balanced, "d_urgencias", "cutoff30", "rips30_d_urgencias")
run_estimations(rips_est_balanced, "d_cancer", "cutoff30", "rips30_d_cancer")
run_estimations(rips_est_balanced, "d_cardio", "cutoff30", "rips30_d_cardio")
run_estimations(rips_est_balanced, "d_preven", "cutoff30", "rips30_d_preven")
run_estimations(rips_est_balanced, "d_prenat", "cutoff30", "rips30_d_prenat")
run_estimations(rips_est_balanced, "d_respir", "cutoff30", "rips30_d_respir")
# Balance
run_estimations(rips_est_balanced, "genero", "cutoff30", "rips30_genero")
run_estimations(rips_est_balanced, "edad_sisben", "cutoff30", "rips30_edad_sisben")
run_estimations(rips_est_balanced, "estrato_1", "cutoff30", "rips30_estrato_1")
run_estimations(rips_est_balanced, "estrato_2", "cutoff30", "rips30_estrato_2")
run_estimations(rips_est_balanced, "estrato_3", "cutoff30", "rips30_estrato_3")
run_estimations(rips_est_balanced, "estrato_4", "cutoff30", "rips30_estrato_4")
run_estimations(rips_est_balanced, "estrato_5", "cutoff30", "rips30_estrato_5")
run_estimations(rips_est_balanced, "estrato_6", "cutoff30", "rips30_estrato_6")
run_estimations(rips_est_balanced, "activi_sin", "cutoff30", "rips30_activi_sin")
run_estimations(rips_est_balanced, "activi_tra", "cutoff30", "rips30_activi_tra")
run_estimations(rips_est_balanced, "activi_bus", "cutoff30", "rips30_activi_bus")
run_estimations(rips_est_balanced, "activi_est", "cutoff30", "rips30_activi_est")
run_estimations(rips_est_balanced, "activi_hog", "cutoff30", "rips30_activi_hog")
run_estimations(rips_est_balanced, "estcivil_unlibr", "cutoff30", "rips30_estcivil_unlibr")
run_estimations(rips_est_balanced, "estcivil_casado", "cutoff30", "rips30_estcivil_casado")
run_estimations(rips_est_balanced, "estcivil_solter", "cutoff30", "rips30_estcivil_solter")
run_estimations(rips_est_balanced, "anios_educacion", "cutoff30", "rips30_anios_educacion")
run_estimations(rips_est_balanced, "ingresos", "cutoff30", "rips30_ingresos")

# Using the cutoff=40
run_estimations(rips_est_balanced, "n_visitas", "cutoff40", "rips40_visitas")
run_estimations(rips_est_balanced, "c_preven" , "cutoff40", "rips40_c_prev")
run_estimations(rips_est_balanced, "c_prenat" , "cutoff40", "rips40_c_pren")
run_estimations(rips_est_balanced, "c_cancer" , "cutoff40", "rips40_c_cancer")
run_estimations(rips_est_balanced, "c_cardio" , "cutoff40", "rips40_c_cardio")
run_estimations(rips_est_balanced, "c_respir" , "cutoff40", "rips40_c_respir")
run_estimations(rips_est_balanced, "n_consultas", "cutoff40", "rips40_consultas")
run_estimations(rips_est_balanced, "n_hospitalizaciones", "cutoff40", "rips40_hosp")
run_estimations(rips_est_balanced, "n_procedimientos", "cutoff40", "rips40_proc")
run_estimations(rips_est_balanced, "n_urgencias", "cutoff40", "rips40_urgencias")
run_estimations(rips_est_balanced, "d_visitas", "cutoff40", "rips40_d_visitas")
run_estimations(rips_est_balanced, "d_consultas", "cutoff40", "rips40_d_consultas")
run_estimations(rips_est_balanced, "d_hospitalizaciones", "cutoff40", "rips40_d_hosp")
run_estimations(rips_est_balanced, "d_procedimientos", "cutoff40", "rips40_d_proc")
run_estimations(rips_est_balanced, "d_urgencias", "cutoff40", "rips40_d_urgencias")
run_estimations(rips_est_balanced, "d_cancer", "cutoff40", "rips40_d_cancer")
run_estimations(rips_est_balanced, "d_cardio", "cutoff40", "rips40_d_cardio")
run_estimations(rips_est_balanced, "d_preven", "cutoff40", "rips40_d_preven")
run_estimations(rips_est_balanced, "d_prenat", "cutoff40", "rips40_d_prenat")
run_estimations(rips_est_balanced, "d_respir", "cutoff40", "rips40_d_respir")
# Balance
run_estimations(rips_est_balanced, "genero", "cutoff40", "rips40_genero")
run_estimations(rips_est_balanced, "edad_sisben", "cutoff40", "rips40_edad_sisben")
run_estimations(rips_est_balanced, "estrato_1", "cutoff40", "rips40_estrato_1")
run_estimations(rips_est_balanced, "estrato_2", "cutoff40", "rips40_estrato_2")
run_estimations(rips_est_balanced, "estrato_3", "cutoff40", "rips40_estrato_3")
run_estimations(rips_est_balanced, "estrato_4", "cutoff40", "rips40_estrato_4")
run_estimations(rips_est_balanced, "estrato_5", "cutoff40", "rips40_estrato_5")
run_estimations(rips_est_balanced, "estrato_6", "cutoff40", "rips40_estrato_6")
run_estimations(rips_est_balanced, "activi_sin", "cutoff40", "rips40_activi_sin")
run_estimations(rips_est_balanced, "activi_tra", "cutoff40", "rips40_activi_tra")
run_estimations(rips_est_balanced, "activi_bus", "cutoff40", "rips40_activi_bus")
run_estimations(rips_est_balanced, "activi_est", "cutoff40", "rips40_activi_est")
run_estimations(rips_est_balanced, "activi_hog", "cutoff40", "rips40_activi_hog")
run_estimations(rips_est_balanced, "estcivil_unlibr", "cutoff40", "rips40_estcivil_unlibr")
run_estimations(rips_est_balanced, "estcivil_casado", "cutoff40", "rips40_estcivil_casado")
run_estimations(rips_est_balanced, "estcivil_solter", "cutoff40", "rips40_estcivil_solter")
run_estimations(rips_est_balanced, "anios_educacion", "cutoff40", "rips40_anios_educacion")
run_estimations(rips_est_balanced, "ingresos", "cutoff40", "rips40_ingresos")


# Data frame lists
list_est_rips30 <- mget(ls(pattern = "^rips30_"))
list_est_rips40 <- mget(ls(pattern = "^rips40_"))


# All dataframes to one
est_rips30 <- bind_rows(list_est_rips30)
est_rips40 <- bind_rows(list_est_rips40)

# Save dataframe in .parquet format
write_parquet(est_rips30, file.path(output_folder, "parquet/20240627-est_rips30.parquet"))
write_parquet(est_rips40, file.path(output_folder, "parquet/20240627-est_rips40.parquet"))

# Elements list
estimaciones_list_objects <- c(
  ls(pattern = "^rips30_"),
  ls(pattern = "^rips40_")
)


estimaciones_list_objects <- c(
  "rips30_visitas", "rips30_c_prev", "rips30_c_pren", "rips30_c_cancer", "rips30_c_cardio", "rips30_c_respir", 
  "rips30_consultas", "rips30_hosp", "rips30_proc", "rips30_urgencias", "rips30_d_visitas", "rips30_d_consultas",
  "rips30_d_hosp", "rips30_d_proc", "rips30_d_urgencias", "rips30_d_cancer", "rips30_d_cardio", "rips30_d_preven",
  "rips30_d_prenat", "rips30_d_respir", "rips40_visitas", "rips40_c_prev", "rips40_c_pren", "rips40_c_cancer",
  "rips40_c_cardio", "rips40_c_respir", "rips40_consultas", "rips40_hosp", "rips40_proc", "rips40_urgencias",
  "rips40_d_visitas", "rips40_d_consultas", "rips40_d_hosp", "rips40_d_proc", "rips40_d_urgencias",
  "rips40_d_cancer", "rips40_d_cardio", "rips40_d_preven", "rips40_d_prenat", "rips40_d_respir"
)

# Drop some elements in the environment
rm(list = estimaciones_list_objects, envir = .GlobalEnv)

  
rdplot(y=rips_est_balanced$estrato_1, x=rips_est_balanced$cutoff30)












