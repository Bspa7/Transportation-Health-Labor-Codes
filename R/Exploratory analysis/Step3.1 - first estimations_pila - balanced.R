paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate")
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
      rd_plot <- rdplot(y = sample[[outcome_var]], x = sample[[cutoff_var]])
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
# 2. Initial databases ------------------------------------------------------------

# Databases - SISBEN and MASTER
all_sisben_bgta <- open_dataset(sprintf('%s/%s', project_folder, 'data/sisben3_bgta_demog.parquet'))
master          <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))

# Databases - PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')  
all_pila          <- open_dataset(PILA_history_file)
# Rename a variable if it is necessary
if ("fechanto" %in% names(all_pila)) {
  all_pila <- all_pila %>%
    rename(fechanto_pila = fechanto)
}
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
# PILA: panel at personabasicaid-quarter level outcomes

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
    year      = max(year, na.rm = TRUE),
    yearmode  = max(yearmode, na.rm = TRUE),    
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
#  sample_frac(0.003)

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
#rm(pila_est, skeleton)
gc()

# Replacing NA's to zero in balance variables
pila_est_balanced <- pila_est_balanced %>%
  group_by(personabasicaid) %>%
  fill(puntaje, genero, 
       estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
       activi_sin, activi_tra, activi_bus, activi_est, activi_hog, 
       estcivil_unlibr, estcivil_casado, estcivil_solter, anios_educacion,
       cutoff30, cutoff40, asalariado, .direction = "downup") %>%
  ungroup() %>%
  mutate(across(c(puntaje, genero, 
                  estrato_1, estrato_2, estrato_3, estrato_4, estrato_5, estrato_6, 
                  activi_sin, activi_tra, activi_bus, activi_est, activi_hog, 
                  estcivil_unlibr, estcivil_casado, estcivil_solter, anios_educacion,
                  cutoff30, cutoff40, asalariado), ~replace_na(., 0)))

# Replacing NA's to zero in main variables
pila_est_balanced <- pila_est_balanced %>%
  mutate(across(c(n_registros, d_registros, ibc_salud, sal_dias_cot),
                ~ replace_na(., 0)))


# Crear la variable edad_sisben
pila_est_balanced <- pila_est_balanced %>%
  mutate(edad_sisben = floor(interval(fechanto, quarterly_date) / years(1)))


# Changing the direction of the running variable | Interpretation
pila_est_balanced <-  pila_est_balanced %>% 
  mutate(cutoff30_origi = cutoff30,
         cutoff40_origi = cutoff40,
         cutoff30 = cutoff30*-1,
         cutoff40 = cutoff40*-1)


pila_est_balanced <- pila_est_balanced %>%
  select(quarterly_date, personabasicaid, cutoff30, cutoff40, tipo_cotizante, asalariado, n_registros, d_registros, ibc_salud, everything())
pila_est <- pila_est %>%
  select(quarterly_date, personabasicaid, cutoff30, cutoff40, tipo_cotizante, asalariado, n_registros, d_registros, ibc_salud, everything())

# ----
# 5.1 Estimations on tables for each quarter -----------------------------------

# Using the cutoff=30
run_estimations(pila_est_balanced, "n_registros",    "cutoff30", "pila30_registros")
run_estimations(pila_est_balanced, "d_registros",    "cutoff30", "pila30_d_registros")
run_estimations(pila_est_balanced, "ibc_salud" ,     "cutoff30", "pila30_ibc_salud")
run_estimations(pila_est_balanced, "sal_dias_cot",   "cutoff30", "pila30_sal_dias_cot")
run_estimations(pila_est_balanced, "asalariado",     "cutoff30", "pila30_asalariado")
# Balance
run_estimations(pila_est_balanced, "genero", "cutoff30", "pila30_genero")
run_estimations(pila_est_balanced, "edad_sisben", "cutoff30", "pila30_edad_sisben")
run_estimations(pila_est_balanced, "estrato_1", "cutoff30", "pila30_estrato_1")
run_estimations(pila_est_balanced, "estrato_2", "cutoff30", "pila30_estrato_2")
run_estimations(pila_est_balanced, "estrato_3", "cutoff30", "pila30_estrato_3")
run_estimations(pila_est_balanced, "estrato_4", "cutoff30", "pila30_estrato_4")
run_estimations(pila_est_balanced, "estrato_5", "cutoff30", "pila30_estrato_5")
run_estimations(pila_est_balanced, "estrato_6", "cutoff30", "pila30_estrato_6")
run_estimations(pila_est_balanced, "activi_sin", "cutoff30", "pila30_activi_sin")
run_estimations(pila_est_balanced, "activi_tra", "cutoff30", "pila30_activi_tra")
run_estimations(pila_est_balanced, "activi_bus", "cutoff30", "pila30_activi_bus")
run_estimations(pila_est_balanced, "activi_est", "cutoff30", "pila30_activi_est")
run_estimations(pila_est_balanced, "activi_hog", "cutoff30", "pila30_activi_hog")
run_estimations(pila_est_balanced, "estcivil_unlibr", "cutoff30", "pila30_estcivil_unlibr")
run_estimations(pila_est_balanced, "estcivil_casado", "cutoff30", "pila30_estcivil_casado")
run_estimations(pila_est_balanced, "estcivil_solter", "cutoff30", "pila30_estcivil_solter")
run_estimations(pila_est_balanced, "anios_educacion", "cutoff30", "pila30_anios_educacion")
run_estimations(pila_est_balanced, "ingresos", "cutoff30", "pila30_ingresos")


# Using the cutoff=40
run_estimations(pila_est_balanced, "n_registros",    "cutoff40", "pila40_registros")
run_estimations(pila_est_balanced, "d_registros",    "cutoff40", "pila40_d_registros")
run_estimations(pila_est_balanced, "ibc_salud" ,     "cutoff40", "pila40_ibc_salud")
run_estimations(pila_est_balanced, "sal_dias_cot",   "cutoff40", "pila40_sal_dias_cot")
run_estimations(pila_est_balanced, "asalariado",     "cutoff40", "pila40_asalariado")
# Balance
run_estimations(pila_est_balanced, "genero", "cutoff40", "pila40_genero")
run_estimations(pila_est_balanced, "edad_sisben", "cutoff40", "pila40_edad_sisben")
run_estimations(pila_est_balanced, "estrato_1", "cutoff40", "pila40_estrato_1")
run_estimations(pila_est_balanced, "estrato_2", "cutoff40", "pila40_estrato_2")
run_estimations(pila_est_balanced, "estrato_3", "cutoff40", "pila40_estrato_3")
run_estimations(pila_est_balanced, "estrato_4", "cutoff40", "pila40_estrato_4")
run_estimations(pila_est_balanced, "estrato_5", "cutoff40", "pila40_estrato_5")
run_estimations(pila_est_balanced, "estrato_6", "cutoff40", "pila40_estrato_6")
run_estimations(pila_est_balanced, "activi_sin", "cutoff40", "pila40_activi_sin")
run_estimations(pila_est_balanced, "activi_tra", "cutoff40", "pila40_activi_tra")
run_estimations(pila_est_balanced, "activi_bus", "cutoff40", "pila40_activi_bus")
run_estimations(pila_est_balanced, "activi_est", "cutoff40", "pila40_activi_est")
run_estimations(pila_est_balanced, "activi_hog", "cutoff40", "pila40_activi_hog")
run_estimations(pila_est_balanced, "estcivil_unlibr", "cutoff40", "pila40_estcivil_unlibr")
run_estimations(pila_est_balanced, "estcivil_casado", "cutoff40", "pila40_estcivil_casado")
run_estimations(pila_est_balanced, "estcivil_solter", "cutoff40", "pila40_estcivil_solter")
run_estimations(pila_est_balanced, "anios_educacion", "cutoff40", "pila40_anios_educacion")
run_estimations(pila_est_balanced, "ingresos", "cutoff40", "pila40_ingresos")


# Data frame lists
list_est_pila30 <- mget(ls(pattern = "^pila30_"))
list_est_pila40 <- mget(ls(pattern = "^pila40_"))


# All dataframes to one
est_pila30 <- bind_rows(list_est_pila30)
est_pila40 <- bind_rows(list_est_pila40)

# Save dataframe in .parquet format
write_parquet(est_pila30, file.path(output_folder, "parquet/20240627-est_pila30.parquet"))
write_parquet(est_pila40, file.path(output_folder, "parquet/20240627-est_pila40.parquet"))

# Elements list
estimaciones_list_objects <- c(
  ls(pattern = "^pila30_"),
  ls(pattern = "^pila40_")
)
rm(list = estimaciones_list_objects, envir = .GlobalEnv)

# ----
# 5.2 Estimations on figures in some outcomes ----------------------------------

# Variables list to estimation
variables <- c("n_registros",         "d_registros", 
               "ibc_salud", "sal_dias_cot",
               "asalariado", "genero","edad_sisben",
               "estrato_1", "estrato_2", "estrato_3", 
               "activi_sin", "activi_tra", "activi_bus", "activi_est", "activi_hog", 
               "estcivil_unlibr", "estcivil_casado", "estcivil_solter", 
               "anios_educacion", "ingresos")

# Periods to the estimation (year and quarter)
periodos <- list(
  c(2012, 2),
  c(2014, 2),
  c(2016, 2),
  c(2017, 2),
  c(2018, 2)
)

# Running variable list
cutoff_vars <- c("cutoff30", "cutoff40")

# Create a list to save the figures names
figure_files <- c()

# Bucle over each variable, period and running variable
for (variable in variables) {
  for (periodo in periodos) {
    for (cutoff_var in cutoff_vars) {
      year <- periodo[1]
      quarter <- periodo[2]
      
      # Graph generation
      plot <- plot_rd(pila_est_balanced, variable, cutoff_var, year, quarter)
      
      if (!is.null(plot)) {
        # Generate the file name
        filename <- file.path(output_folder, paste0("figures/20240627-pila", gsub("[^0-9]", "", cutoff_var), "_", variable, "_", year, "q", quarter, ".png"))
        
        # Save the graph
        ggsave(filename, plot = plot, width = 8, height = 6, dpi = 300)
        
        # Aggregate the file name to the list
        figure_files <- c(figure_files, filename)
      }
    }
  }
}

# Crear archivo comprimido con las figuras
zipfile <- file.path(output_folder, "figures/20240627-estimaciones_pila.zip")

# Cambiar temporalmente el directorio de trabajo
setwd(file.path(output_folder, "figures"))
zip(zipfile, files = basename(figure_files))

# Eliminar los archivos originales de figuras
file.remove(figure_files)


rm(list=ls())

# ----











