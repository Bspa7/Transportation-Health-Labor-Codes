# Date of creation: 24-Sep-2024
# Created by Brayan Pineda
# Objective: This code gives an overview of diagnoses and their registers in the system

# Packages and folders ---------------------------------------------------------
paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
#base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")
# ----

# 1. Initial databases ---------------------------------------------------------

# MASTER
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>%
  filter(puntaje>=20 & puntaje<=45) %>% 
  select(personabasicaid) %>% 
  unique() %>% 
  collect()

# RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet') %>% glimpse

# ----

# 2. First modifications to initial databases ----------------------------------

# Modifications to RIPS: filter to Bogota into 2015-2019
all_rips          <- open_dataset(RIPS_history_file) %>%
  rename(date_rips = DATE) %>% 
  filter(COD_DPTO == 11 & COD_MPIO == 1)  %>%
  mutate(
    date_rips      = as.Date(date_rips),
    year           = year(date_rips),
    monthly_date = floor_date(date_rips, "month"),
  )  %>%
  filter(year>=2017 & year<=2018) %>% 
  filter(MODULE == "c") %>% 
  collect() %>% 
  glimpse()

  all_rips <- master %>% 
    left_join(all_rips, by="personabasicaid") 
  gc()
    
  
# 3. Diagnostico principal -----------------------------------------------------  
diag_prin_summary <- all_rips %>%
  group_by(DIAG_PRIN, monthly_date) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = monthly_date, values_from = n, values_fill = 0) %>%
  glimpse()

# ----  

# 4. Diagnostico principal -----------------------------------------------------  
  causa_externa_summary <- all_rips %>%
    group_by(CAUSA_EXTERNA, monthly_date) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = monthly_date, values_from = n, values_fill = 0) %>%
    glimpse()  
  
# ----  
# 5. Codigos CUPS --------------------------------------------------------------
  cod_cups_summary <- all_rips %>%
    group_by(COD_CUPS, monthly_date) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = monthly_date, values_from = n, values_fill = 0) %>%
    glimpse()

# ----

write_parquet(diag_prin_summary,     file.path(output_folder, "parquet/20240924-diagnoses_CIEV10.parquet"))
write_parquet(causa_externa_summary, file.path(output_folder, "parquet/20240924-diagnoses_cau_ext.parquet"))
write_parquet(cod_cups_summary,      file.path(output_folder, "parquet/20240924-diagnoses_CUPS.parquet"))  
  
  
  
  
  
  
  
  
  