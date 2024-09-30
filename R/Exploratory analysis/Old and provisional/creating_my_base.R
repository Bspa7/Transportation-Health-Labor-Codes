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

# Define a function to create the running variables centered in zero
centrar_puntaje <- function(df) {
  df <- df %>% 
    mutate(
      cutoff30 = puntaje - 30.56,
      cutoff40 = puntaje - 40      
    )
  return(df)
}

# Define a function to save estimations
run_estimations <- function(data, var_name, cutoff_var, df_name) {
  # Definir ventana de tiempo
  years <- 2012:2019
  quarters <- 1:4
  
  # Empty Dataframe to save results
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
  
  # Loop for each year and quarter
  for (year_val in years) {
    for (quarter_val in quarters) {
      # Filter data for specific year and quarter
      data_subset <- data %>%
        filter(year(quarterly_date) == !!year_val & quarter(quarterly_date) == !!quarter_val) %>%
        drop_na(all_of(c(var_name, cutoff_var)))
      
      # Print some stats to verification
      print(paste("Iterarion with year:", year_val, "and quarter:", quarter_val)) # Depuración
      print(summary(data_subset$quarterly_date))
      print(paste("N-rows in the subset:", nrow(data_subset)))
      
      # Verify if there are information to the estimation
      if (nrow(data_subset) > 20) { 
        # Print the current information
        cat("Variable:", var_name, "Cutoff:", cutoff_var, "Trimestre:", paste0(year_val, "q", quarter_val), "Hora:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
        
        # ESTIMATION!
        est <- tryCatch(
          {
            rdrobust(data_subset[[var_name]], data_subset[[cutoff_var]], all=TRUE)
          },
          error = function(e) {
            message(paste("Error en el año", year_val, "trimestre", quarter_val, ":", e$message))
            return(NULL)
          }
        )
        
        if (!is.null(est)) {
          # Extract the values from robust estimation
          coef_robust <- est$coef["Robust", ]
          se_robust <- est$se["Robust", ]
          pv_robust <- est$pv["Robust", ]
          ci_lower_robust <- est$ci["Robust", "CI Lower"]
          ci_upper_robust <- est$ci["Robust", "CI Upper"]
          
          # Obtain sample size
          N1 <- est$N[1]
          N2 <- est$N[2]
          
          # Create a temporal data frame with the current results
          temp_df <- data.frame(
            coef_robust = coef_robust,
            se_robust = se_robust,
            pv_robust = pv_robust,
            ci_lower_robust = ci_lower_robust,
            ci_upper_robust = ci_upper_robust,
            N1 = N1,
            N2 = N2,
            period = paste0(year_val, "q", quarter_val),
            variable = var_name,
            stringsAsFactors = FALSE
          )
          
          # Add the current results to the final data frame
          results_df <- rbind(results_df, temp_df)
        }
      } else {
        message(paste("Insuficientes datos para el año", year_val, "trimestre", quarter_val))
      }
    }
  }
  
  # Assign a specific name to the data frame
  assign(df_name, results_df, envir = .GlobalEnv)
}

# Function to generate graphs (estimations)
plot_rd <- function(data, outcome_var, cutoff_var, year_val, quarter_val) {
  # Fliter by year and quarter
  sample <- data %>%
    filter(year(quarterly_date) == !!year_val & quarter(quarterly_date) == !!quarter_val)
  
  # Verify if the estimation is possible with the available data
  if (nrow(sample) > 20) {
    # Print the current information
    cat("Variable:", outcome_var, "Cutoff:", cutoff_var, "Trimestre:", paste0(year_val, "q", quarter_val), "Hora:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    # ESTIMATION!!
    rd_result <- tryCatch(
      {
        rdrobust(sample[[outcome_var]], sample[[cutoff_var]], all=TRUE)
      },
      error = function(e) {
        message(paste("Error in the year", year_val, "quarter", quarter_val, ":", e$message))
        return(NULL)
      }
    )
    
    # Verify if the estimation was successful 
    if (!is.null(rd_result)) {
      coef_robust <- rd_result$coef["Robust", ]
      pv_robust <- rd_result$pv["Robust", ]
      
      # Generate the rdplot graph
      rd_plot <- rdplot(y = sample[[outcome_var]], x = sample[[cutoff_var]], binselect = "es")
      plot <- rd_plot$rdplot
      
      # Graph design
      plot <- plot + theme_classic() +
        labs(
          title = "",
          subtitle = "",
          caption = paste0("period ", year_val, "q", quarter_val, "\n", outcome_var),
          x = paste0("Distance to SISBEN cutoff (", gsub("[^0-9]", "", cutoff_var), "pts)\n",
                     "Running variable adjusted to elegibles individuals at the right side"),
          y = "Mean of the outcome"
        ) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
        theme(
          legend.position = "none",
          plot.caption = element_text(hjust = 0),
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = 10)),
          axis.title.x.bottom = element_text(size = 10, margin = margin(t = 5))
        ) +
        annotate("text", x = Inf, y = Inf, label = paste0("coef=", round(coef_robust, 4), "\np_value=", round(pv_robust, 4)),
                 hjust = 1.1, vjust = 2, size = 3)
      
      # Return to the graph
      return(plot)
    } else {
      return(NULL)  # If the estimation fail, return to NULL
    }
  } else {
    message(paste("There is not enough data in the yea", year_val, "quarter", quarter_val))
    return(NULL)  # If there is not enough data, return NULL
  }
}


# ----

# 2. Initial databases ---------------------------------------------------------

# Databases: MASTER
#master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))
master            <- sprintf('%s/%s', project_folder, 'data/master.parquet') 
# Databases: PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')  
# Databases: RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')

# Leer los archivos Parquet y tomar una submuestra aleatoria del 5%

master <- arrow::read_parquet(master) %>%
  as_tibble() %>%
  slice_sample(prop = 0.01)

PILA_history_file <- arrow::read_parquet(PILA_history_file) %>%
  as_tibble() %>%
  slice_sample(prop = 0.02)

RIPS_history_file <- arrow::read_parquet(RIPS_history_file) %>%
  as_tibble() %>%
  slice_sample(prop = 0.02)

# ----

# 3. First modifications to initial databases ----------------------------------
# Modifications to PILA: filter to Bogota into 2012-2019
all_pila          <- PILA_history_file %>% 
  rename(fechanto_pila = fechanto, date_pila = DATE) %>% 
  filter(depto_cod == 11 & ciudad_cod == 1) %>%
  mutate(
    date_pila = as.Date(date_pila),
    quarterly_date = floor_date(date_pila, "quarter"),    
    year = year(date_pila)
  ) %>% 
  filter(year>=2012 & year<=2019)

# Modifications to RIPS: filter to Bogota into 2012-2019
all_rips          <- RIPS_history_file %>%
  rename(date_rips = DATE) %>% 
  filter(COD_DPTO == 11 & COD_MPIO == 1)  %>%
  mutate(
    date_rips      = as.Date(date_rips),
    year           = year(date_rips),
    quarterly_date = floor_date(date_rips, "quarter")
  ) %>% 
  filter(year>=2012 & year<=2019)

# Modifications to MASTER: Creating balance outcomes (info directly from sisben)
master <- master %>% 
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
    )) 

# ----

# 4. Creating the database (balanced) of RIPS-SISBEN ---------------------------
merged_rips <- master %>% 
  left_join(all_rips, by="personabasicaid")

rips_npbid <- merged_rips %>% 
  select(personabasicaid) %>% 
  unique() %>% 
  collect

# Seleccionar todas las variables con .x y sin el sufijo .y
merged_rips <- merged_rips %>%
  select(personabasicaid, quarterly_date, documen, ingresos, tipodoc, fechanto, estcivil, genero,
         estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, activi_sin, activi_tra,
         activi_bus, activi_est, activi_hog, estcivil_unlibr, estcivil_casado, estcivil_solter,
         anios_educacion, string_pb, date_rips, DIAG_PRIN, CAUSA_EXTERNA, TIPO_USUARIO, edad_RIPS,
         YEAR, MODULE, COD_DPTO.x, COD_MPIO.x, sexo_RIPS, puntaje.x, edad.x, sexo.x, estrato.x, nivel.x, grado.x, year)

# Renombrar variables para quitar el sufijo .x
merged_rips <- merged_rips %>%
  rename(
    COD_DPTO = COD_DPTO.x,
    COD_MPIO = COD_MPIO.x,
    puntaje = puntaje.x,
    edad = edad.x,
    sexo = sexo.x,
    estrato = estrato.x,
    nivel = nivel.x,
    grado = grado.x
  )



merged_rips <- merged_rips %>%
  categorize_age() %>% 
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

rips_est <- merged_rips %>%
  mutate(quarterly_date = as.Date(quarterly_date)) %>% 
  centrar_puntaje() %>% 
  collect

rm(merged_rips)
gc()


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

# Changing the direction of the running variable | Interpretation
rips_est_balanced <-  rips_est_balanced %>% 
  mutate(cutoff30_origi = cutoff30,
         cutoff40_origi = cutoff40,
         cutoff30 = cutoff30*-1,
         cutoff40 = cutoff40*-1)

# ----

# 5. Creating the database (balanced) of PILA-SISBEN ---------------------------

#all_pila_c <- PILA_history_file
all_pila_c <- all_pila

ID_pila <- master %>% 
  select(personabasicaid) %>% collect

merged_pila <- ID_pila %>% 
  left_join(all_pila_c, by="personabasicaid")  

pila_npbid <- merged_pila %>% 
  select(personabasicaid) %>% 
  unique() %>% 
  collect

rm(all_pila_c, pila_npbid, ID_pila)
gc()

merged_pila <- merged_pila %>%
  mutate(
    pila_indep = tipo_cotiz %in% c(3, 16, 41, 42, 2, 59, 57, 66),
    pila_depen = !pila_indep
  ) %>% 
  group_by(quarterly_date, personabasicaid) %>%  
  summarise(
    n_registros = n(),
    d_registros = if_else(n() > 0, 1, 0),
    ibc_salud     = max(ibc_salud, na.rm = TRUE),
    sal_dias_cot  = sum(sal_dias_cot, na.rm = TRUE),
    pila_indep = max(pila_indep, na.rm = TRUE),
    pila_depen = max(pila_depen, na.rm = TRUE)
  )


gc()

pila_est <- merged_pila %>%
  mutate(quarterly_date = as.Date(quarterly_date))

rm(merged_pila)
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

# Replacing NA's to zero in main variables
pila_est_balanced <- pila_est_balanced %>%
  mutate(across(c(n_registros, d_registros, ibc_salud, sal_dias_cot, pila_indep, pila_depen),
                ~ replace_na(., 0)))

# ----

# 6. Creating the final database (balanced) of SISBEN-PILA-RIPS ----------------
main_base <- rips_est_balanced %>% 
  left_join(pila_est_balanced, by=c("personabasicaid", "quarterly_date"))

write_parquet(main_base, file.path(data_dir, "base_pila_rips_pbid.parquet"))

# ----

# 7. Replacing stats respect to real information -------------------------------

# Cargar el dataset
main_base <- open_dataset(sprintf('%s/%s', data_dir, 'base_pila_rips_pbid.parquet')) %>% collect()
summary(main_base)

# Función para ajustar una columna a estadísticas específicas
adjust_column <- function(df, col, stats) {
  # Ordenar los datos y obtener su longitud
  sorted_data <- sort(df[[col]], na.last = NA)
  n <- length(sorted_data)
  
  # Posiciones de los percentiles
  pos <- list(
    min = 1,
    q1 = floor(n * 0.25),
    median = floor(n * 0.5),
    mean = floor(n * 0.5), # Aproximación
    q3 = floor(n * 0.75),
    max = n
  )
  
  # Asignar los valores específicos a las posiciones correspondientes
  sorted_data[pos$min] <- stats["Min"]
  sorted_data[pos$q1] <- stats["1st Qu."]
  sorted_data[pos$median] <- stats["Median"]
  sorted_data[pos$mean] <- stats["Mean"]
  sorted_data[pos$q3] <- stats["3rd Qu."]
  sorted_data[pos$max] <- stats["Max"]
  
  # Ajustar el resto de los valores linealmente
  interp <- approxfun(c(1, pos$q1, pos$median, pos$q3, n), 
                      c(stats["Min"], stats["1st Qu."], stats["Median"], stats["3rd Qu."], stats["Max"]))
  sorted_data <- interp(seq_len(n))
  
  df[[col]] <- sorted_data
  return(df)
}

# Definir las estadísticas objetivo para cada columna
target_stats <- list(
  puntaje    =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),
  genero     =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),  
  estrato_1  =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),
  estrato_2  =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),
  estrato_3  =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),
  estrato_4  =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),
  estrato_5  =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),
  estrato_6  =          c(Min = 0, "1st Qu." = 33.69, Median = 55.29, Mean = 48.90, "3rd Qu." = 66.80, Max = 87.13),  
  activi_sin =          c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),
  activi_tra =          c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),
  activi_bus =          c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),
  activi_est =          c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),
  activi_hog =          c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),
  estcivil_unlibr =     c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  estcivil_casado =     c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  estcivil_solter =     c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  anios_educacion =     c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),     
  ingresos =            c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  edad_RIPS =           c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  n_visitas =           c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  d_visitas =           c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  n_consultas =         c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_consultas =         c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  n_hospitalizaciones = c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_hospitalizaciones = c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  n_procedimientos =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  d_procedimientos =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  n_urgencias =         c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_urgencias =         c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  c_cancer =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_cancer =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  c_cardio =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  d_cardio =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  c_preven =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_preven =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  c_prenat =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_prenat =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  c_respir =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_respir =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  cutoff30 =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  cutoff40 =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  cutoff30_origi = c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  cutoff40_origi = c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  n_registros =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  d_registros =    c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  ibc_salud =      c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),     
  sal_dias_cot =   c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000),   
  pila_indep =     c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
  pila_depen =     c(Min = 0, "1st Qu." = 0.000, Median = 1.000, Mean = 0.543, "3rd Qu." = 1.000, Max = 1.000), 
)

# Ajustar las columnas en el dataframe
for (col in names(target_stats)) {
  main_base <- adjust_column(main_base, col, target_stats[[col]])
}

# Asegurarse de que no haya NA en el dataframe
main_base <- main_base %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Ver el resumen del dataframe ajustado
summary(main_base)

# ----

summary(main_base)














master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>% collect
# Databases: PILA
PILA_history_file <- open_dataset(sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')) 
# Databases: RIPS
RIPS_history_file <- open_dataset(sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet'))



# 1. Abrir y tomar muestra del 1% para el dataset 'master'
master_path <- sprintf('%s/%s', project_folder, 'data/master.parquet')
master <- open_dataset(master_path) %>% collect()
master_sample <- master %>% sample_frac(0.01)
write_parquet(master_sample, master_path)
rm(master)
gc()

# 2. Abrir y tomar muestra del 1% para el dataset 'history_PILA'
pila_path <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
pila <- open_dataset(pila_path) %>% collect()
pila_sample <- pila %>% sample_frac(0.001)
write_parquet(pila_sample, pila_path)
rm(pila)
gc()

# 3. Abrir y tomar muestra del 1% para el dataset 'history_RIPS'
rips_path <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')
rips <- open_dataset(rips_path) %>% collect()
rips_sample <- rips %>% sample_frac(0.001)
write_parquet(rips_sample, rips_path)
rm(rips)
gc()






