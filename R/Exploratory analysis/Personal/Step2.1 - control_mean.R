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

# 3. Control mean  -------------------------------------------------------------

# Define los años y trimestres de interés
years <- 2012:2019
quarters <- 1:4

# Lista de variables de interés
variables_of_interest <- c(
  "n_registros", "d_registros", "ibc_salud", "sal_dias_cot", "pila_indep", "pila_depen", 
  "n_visitas", "c_preven", "c_prenat", "c_cancer", "c_cardio", "c_respir", "n_consultas", 
  "n_hospitalizaciones", "n_procedimientos", "n_urgencias", "d_visitas", "d_consultas", 
  "d_hospitalizaciones", "d_procedimientos", "d_urgencias", "d_cancer", "d_cardio", 
  "d_preven", "d_prenat", "d_respir", "genero", "edad_sisben", "estrato_1", "estrato_2", 
  "estrato_3", "estrato_4", "estrato_5", "estrato_6", "activi_sin", "activi_tra", 
  "activi_bus", "activi_est", "activi_hog", "estcivil_unlibr", "estcivil_casado", 
  "estcivil_solter", "anios_educacion", "ingresos"
)

# DataFrame para almacenar los resultados
results_df <- data.frame(
  grupo = character(),
  variable = character(),
  periodo = character(),
  mean_control = numeric(),
  stringsAsFactors = FALSE
)

# Función para calcular y almacenar la media del grupo de control
calculate_mean_control <- function(data, cutoff_var, cutoff_value, var_name) {
  control_group <- data %>%
    filter(!!sym(cutoff_var) < cutoff_value)
  mean_control <- mean(control_group[[var_name]], na.rm = TRUE)
  return(mean_control)
}

# Iterar sobre los años, trimestres, variables de interés y los grupos de corte
cutoffs <- c("cutoff30", "cutoff40")

for (year_val in years) {
  for (quarter_val in quarters) {
    # Filtra los datos para el periodo específico
    data_period <- main_base %>%
      filter(year(quarterly_date) == year_val & quarter(quarterly_date) == quarter_val)
    
    for (cutoff in cutoffs) {
      for (var in variables_of_interest) {
        mean_control <- calculate_mean_control(data_period, cutoff, 0, var)
        temp_df <- data.frame(
          grupo = cutoff,
          variable = var,
          periodo = paste0(year_val, "q", quarter_val),
          mean_control = mean_control,
          stringsAsFactors = FALSE
        )
        results_df <- rbind(results_df, temp_df)
      }
    }
  }
}

# Save results
write_parquet(results_df, file.path(output_folder, "parquet/20240723-control_mean.parquet"))



























