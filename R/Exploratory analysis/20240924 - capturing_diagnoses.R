# Date of creation: 24-Sep-2024
# Created by Brayan Pineda
# Script Objective: This code gives an overview of diagnoses and their registers in the system
# Last modification: 18-Oct-2024
# Last modification by: Brayan Pineda
# Last modification aim: Capture the diagnoses overview for all the modules in RIPS

# Packages and folders ---------------------------------------------------------
paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "/Users/brayanpineda/Documents/Trabajo/2024_DIME/COL Health and Public Transport"
#base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"


project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")
# ----

# Initial databases ------------------------------------------------------------

# MASTER
master <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>%
  filter(puntaje >= 15 & puntaje <= 45) %>%
  select(personabasicaid) %>%
  unique() %>%
  collect()

# RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')

# ----

# Function to process data by module -------------------------------------------

process_rips_by_module <- function(module, output_prefix) {
  # Step 1: Filter and process the data
  all_rips <- open_dataset(RIPS_history_file) %>%
    rename(date_rips = DATE) %>%
    filter(COD_DPTO == 11 & COD_MPIO == 1) %>%
    mutate(
      date_rips = as.Date(date_rips),
      year = year(date_rips),
      monthly_date = floor_date(date_rips, "month")
    ) %>%
    filter(year >= 2017 & year <= 2018) %>%
    filter(MODULE == module) %>%
    collect()
  
  # Join with master data
  all_rips <- master %>%
    left_join(all_rips, by = "personabasicaid")
  
  # Step 2: Diagnóstico principal summary
  diag_prin_summary <- all_rips %>%
    group_by(DIAG_PRIN, monthly_date) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = monthly_date, values_from = n, values_fill = 0)
  
  # Step 3: Causa externa summary
  causa_externa_summary <- all_rips %>%
    group_by(CAUSA_EXTERNA, monthly_date) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = monthly_date, values_from = n, values_fill = 0)
  
  # Step 4: Codigos CUPS summary
  cod_cups_summary <- all_rips %>%
    group_by(COD_CUPS, monthly_date) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = monthly_date, values_from = n, values_fill = 0)
  
  # Step 5: Save results
  write_parquet(diag_prin_summary,     file.path(output_folder, sprintf("parquet/%s_CIEV10.parquet", output_prefix)))
  write_parquet(causa_externa_summary, file.path(output_folder, sprintf("parquet/%s_cau_ext.parquet", output_prefix)))
  write_parquet(cod_cups_summary,      file.path(output_folder, sprintf("parquet/%s_CUPS.parquet", output_prefix)))
  
  # Clean up memory
  rm(all_rips, diag_prin_summary, causa_externa_summary, cod_cups_summary)
  gc()
}

# ----

# Process each module and save results -----------------------------------------

process_rips_by_module("c", "20241018-diagnoses_c")  # Consultations
process_rips_by_module("p", "20241018-diagnoses_p")  # Procedures
process_rips_by_module("h", "20241018-diagnoses_h")  # Hospitalizations
process_rips_by_module("u", "20241018-diagnoses_u")  # Emergencies

# ----







