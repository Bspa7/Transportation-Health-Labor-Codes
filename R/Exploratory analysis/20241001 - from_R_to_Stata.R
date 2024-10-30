# Date of creation: 30-Sep-2024
# Created by Brayan Pineda
# Objective: This code generates the info to update the total of registers between 2010-2019 by score.
#            This information allows to update the first table of all the project

paquetes <- c("rddensity", "haven", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
#base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"
base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"


project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")

# Initial databases ---------------------------------------------------------

main_base     <- open_dataset(sprintf('%s/%s', data_dir, "panel_total_sample1045_t1019.parquet")) %>% 
  mutate(
    score_1015 = if_else(puntaje>10    & puntaje<=15, 1, 0),      
    score_1520 = if_else(puntaje>15    & puntaje<=20, 1, 0),
    score_2025 = if_else(puntaje>20    & puntaje<=25, 1, 0),
    score_2530 = if_else(puntaje>25    & puntaje<=30.56, 1, 0),
    score_3035 = if_else(puntaje>30.56 & puntaje<=35, 1, 0),
    score_3540 = if_else(puntaje>35    & puntaje<=40, 1, 0),
    score_4045 = if_else(puntaje>40    & puntaje<=45, 1, 0)
  ) %>% 
  collect()


# File name to the dta
output_file <- sprintf('%s/%s', data_dir, "panel_sample1045_t1019.dta")

# Save my database on Stata format
write_dta(main_base, output_file)
























