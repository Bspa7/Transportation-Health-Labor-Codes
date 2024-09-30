paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

base_pbid <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/datos_originales/personabasicaid_all_projects/Bases insumos"
base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
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

master <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>% 
  select(personabasicaid, puntaje) %>% unique() %>% collect

# Main database
main_base <- open_dataset(sprintf('%s/%s', data_dir, 'base_pila_rips_pbid.parquet')) %>% 
  select(-puntaje, -cutoff30, -cutoff40, -cutoff30_origi, -cutoff40_origi) %>% 
  glimpse %>% collect()

main_base <- main_base %>% 
  left_join(master, by='personabasicaid') %>% 
  centrar_puntaje() %>% 
  mutate(cutoff30_origi = cutoff30,
         cutoff40_origi = cutoff40,
         cutoff30 = cutoff30*-1,
         cutoff40 = cutoff40*-1)
summary(main_base)
rm(master)
gc()
#main_base <- main_base %>% 
#  sample_frac(0.005)

# ----

# 3. Estimations on tables for each quarter ------------------------------------

# Balance using 30pts as the cutoff
run_estimations(main_base, "genero",      "cutoff30", "est30_genero")
run_estimations(main_base, "edad_sisben", "cutoff30", "est30_edad_sisben")
run_estimations(main_base, "estrato_1", "cutoff30", "est30_estrato_1")
run_estimations(main_base, "estrato_2", "cutoff30", "est30_estrato_2")
run_estimations(main_base, "estrato_3", "cutoff30", "est30_estrato_3")
run_estimations(main_base, "estrato_4", "cutoff30", "est30_estrato_4")
run_estimations(main_base, "estrato_5", "cutoff30", "est30_estrato_5")
run_estimations(main_base, "estrato_6", "cutoff30", "est30_estrato_6")
run_estimations(main_base, "activi_sin", "cutoff30", "est30_activi_sin")
run_estimations(main_base, "activi_tra", "cutoff30", "est30_activi_tra")
run_estimations(main_base, "activi_bus", "cutoff30", "est30_activi_bus")
run_estimations(main_base, "activi_est", "cutoff30", "est30_activi_est")
run_estimations(main_base, "activi_hog", "cutoff30", "est30_activi_hog")
run_estimations(main_base, "estcivil_unlibr", "cutoff30", "est30_estcivil_unlibr")
run_estimations(main_base, "estcivil_casado", "cutoff30", "est30_estcivil_casado")
run_estimations(main_base, "estcivil_solter", "cutoff30", "est30_estcivil_solter")
run_estimations(main_base, "anios_educacion", "cutoff30", "est30_anios_educacion")
run_estimations(main_base, "ingresos", "cutoff30", "est30_ingresos")

# Balance using 40pts as the cutoff
run_estimations(main_base, "genero",      "cutoff40", "est40_genero")
run_estimations(main_base, "edad_sisben", "cutoff40", "est40_edad_sisben")
run_estimations(main_base, "estrato_1", "cutoff40", "est40_estrato_1")
run_estimations(main_base, "estrato_2", "cutoff40", "est40_estrato_2")
run_estimations(main_base, "estrato_3", "cutoff40", "est40_estrato_3")
run_estimations(main_base, "estrato_4", "cutoff40", "est40_estrato_4")
run_estimations(main_base, "estrato_5", "cutoff40", "est40_estrato_5")
run_estimations(main_base, "estrato_6", "cutoff40", "est40_estrato_6")
run_estimations(main_base, "activi_sin", "cutoff40", "est40_activi_sin")
run_estimations(main_base, "activi_tra", "cutoff40", "est40_activi_tra")
run_estimations(main_base, "activi_bus", "cutoff40", "est40_activi_bus")
run_estimations(main_base, "activi_est", "cutoff40", "est40_activi_est")
run_estimations(main_base, "activi_hog", "cutoff40", "est40_activi_hog")
run_estimations(main_base, "estcivil_unlibr", "cutoff40", "est40_estcivil_unlibr")
run_estimations(main_base, "estcivil_casado", "cutoff40", "est40_estcivil_casado")
run_estimations(main_base, "estcivil_solter", "cutoff40", "est40_estcivil_solter")
run_estimations(main_base, "anios_educacion", "cutoff40", "est40_anios_educacion")
run_estimations(main_base, "ingresos", "cutoff40", "est40_ingresos")

# Main estimations using 30pts as the cutoff
run_estimations(main_base, "n_registros",    "cutoff30", "est30_registros")
run_estimations(main_base, "d_registros",    "cutoff30", "est30_d_registros")
run_estimations(main_base, "ibc_salud" ,     "cutoff30", "est30_ibc_salud")
run_estimations(main_base, "sal_dias_cot",   "cutoff30", "est30_sal_dias_cot")
run_estimations(main_base, "pila_indep",     "cutoff30", "est30_indep")
run_estimations(main_base, "pila_depen",     "cutoff30", "est30_depen")
run_estimations(main_base, "n_visitas", "cutoff30", "est30_visitas")
run_estimations(main_base, "c_preven" , "cutoff30", "est30_c_prev")
run_estimations(main_base, "c_prenat" , "cutoff30", "est30_c_pren")
run_estimations(main_base, "c_cancer" , "cutoff30", "est30_c_cancer")
run_estimations(main_base, "c_cardio" , "cutoff30", "est30_c_cardio")
run_estimations(main_base, "c_respir" , "cutoff30", "est30_c_respir")
run_estimations(main_base, "n_consultas", "cutoff30", "est30_consultas")
run_estimations(main_base, "n_hospitalizaciones", "cutoff30", "est30_hosp")
run_estimations(main_base, "n_procedimientos", "cutoff30", "est30_proc")
run_estimations(main_base, "n_urgencias", "cutoff30", "est30_urgencias")
run_estimations(main_base, "d_visitas", "cutoff30", "est30_d_visitas")
run_estimations(main_base, "d_consultas", "cutoff30", "est30_d_consultas")
run_estimations(main_base, "d_hospitalizaciones", "cutoff30", "est30_d_hosp")
run_estimations(main_base, "d_procedimientos", "cutoff30", "est30_d_proc")
run_estimations(main_base, "d_urgencias", "cutoff30", "est30_d_urgencias")
run_estimations(main_base, "d_cancer", "cutoff30", "est30_d_cancer")
run_estimations(main_base, "d_cardio", "cutoff30", "est30_d_cardio")
run_estimations(main_base, "d_preven", "cutoff30", "est30_d_preven")
run_estimations(main_base, "d_prenat", "cutoff30", "est30_d_prenat")
run_estimations(main_base, "d_respir", "cutoff30", "est30_d_respir")


# Main estimations using 40pts as the cutoff
run_estimations(main_base, "n_registros",    "cutoff40", "est40_registros")
run_estimations(main_base, "d_registros",    "cutoff40", "est40_d_registros")
run_estimations(main_base, "ibc_salud" ,     "cutoff40", "est40_ibc_salud")
run_estimations(main_base, "sal_dias_cot",   "cutoff40", "est40_sal_dias_cot")
run_estimations(main_base, "pila_indep",     "cutoff40", "est40_indep")
run_estimations(main_base, "pila_depen",     "cutoff40", "est40_depen")
run_estimations(main_base, "n_visitas", "cutoff40", "est40_visitas")
run_estimations(main_base, "c_preven" , "cutoff40", "est40_c_prev")
run_estimations(main_base, "c_prenat" , "cutoff40", "est40_c_pren")
run_estimations(main_base, "c_cancer" , "cutoff40", "est40_c_cancer")
run_estimations(main_base, "c_cardio" , "cutoff40", "est40_c_cardio")
run_estimations(main_base, "c_respir" , "cutoff40", "est40_c_respir")
run_estimations(main_base, "n_consultas", "cutoff40", "est40_consultas")
run_estimations(main_base, "n_hospitalizaciones", "cutoff40", "est40_hosp")
run_estimations(main_base, "n_procedimientos", "cutoff40", "est40_proc")
run_estimations(main_base, "n_urgencias", "cutoff40", "est40_urgencias")
run_estimations(main_base, "d_visitas", "cutoff40", "est40_d_visitas")
run_estimations(main_base, "d_consultas", "cutoff40", "est40_d_consultas")
run_estimations(main_base, "d_hospitalizaciones", "cutoff40", "est40_d_hosp")
run_estimations(main_base, "d_procedimientos", "cutoff40", "est40_d_proc")
run_estimations(main_base, "d_urgencias", "cutoff40", "est40_d_urgencias")
run_estimations(main_base, "d_cancer", "cutoff40", "est40_d_cancer")
run_estimations(main_base, "d_cardio", "cutoff40", "est40_d_cardio")
run_estimations(main_base, "d_preven", "cutoff40", "est40_d_preven")
run_estimations(main_base, "d_prenat", "cutoff40", "est40_d_prenat")
run_estimations(main_base, "d_respir", "cutoff40", "est40_d_respir")

# Data frame lists
list_est30 <- mget(ls(pattern = "^est30_"))
list_est40 <- mget(ls(pattern = "^est40_"))

# All dataframes to one
est_cut30 <- bind_rows(list_est30)
est_cut40 <- bind_rows(list_est40)

# Save dataframe in .parquet format
write_parquet(est_cut30, file.path(output_folder, "parquet/20240715-table_est30.parquet"))
write_parquet(est_cut40, file.path(output_folder, "parquet/20240715-table_est40.parquet"))

estimaciones_list_objects <- c(
  ls(pattern = "^est30_"),
  ls(pattern = "^est40_")
)
rm(list = estimaciones_list_objects, envir = .GlobalEnv)

# ----

# 4. Estimations on figures to balance outcomes --------------------------------
# Variables list to estimation
variables <- c("genero","edad_sisben",
               "estrato_1", "estrato_2", "estrato_3", 
               "activi_sin", "activi_tra", "activi_bus", "activi_est", "activi_hog", 
               "estcivil_unlibr", "estcivil_casado", "estcivil_solter", 
               "anios_educacion", "ingresos")

# Periods to the estimation (year and quarter)
periodos <- list(
  c(2017, 2)
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
      plot <- plot_rd(main_base, variable, cutoff_var, year, quarter)
      
      if (!is.null(plot)) {
        # Generate the file name
        filename <- file.path(output_folder, paste0("figures/20240715-plot", gsub("[^0-9]", "", cutoff_var), "_", variable, "_", year, "q", quarter, ".png"))
        
        # Save the graph
        ggsave(filename, plot = plot, width = 8, height = 6, dpi = 300)
        
        # Aggregate the file name to the list
        figure_files <- c(figure_files, filename)
      }
    }
  }
}

# Crear archivo comprimido con las figuras
zipfile <- file.path(output_folder, "figures/20240715-balance_v1.zip")

# Cambiar temporalmente el directorio de trabajo
setwd(file.path(output_folder, "figures"))
zip(zipfile, files = basename(figure_files))

# Eliminar los archivos originales de figuras
file.remove(figure_files)


rm(list=ls())

# ----











