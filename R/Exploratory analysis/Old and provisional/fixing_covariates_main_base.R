paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
#base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")

# 1. Initial databases ---------------------------------------------------------
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))
main_base <- open_dataset(sprintf('%s/%s', data_dir, "base_pila_rips_pbid_monthly.parquet")) %>% collect()
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')

# Main base without controls (personal covariates)
main_base <- main_base %>%   
  select(-documen, -puntaje, -edad, -estrato, -activi, -nivel, -grado, -sexo, -ingresos, -tipodoc,
       -fechanto, -estcivil, -cutoff30, -cutoff40, -treatment1, -treatment2, 
       -control1, -genero, -estrato_1, -estrato_2, -estrato_3, -estrato_4, -estrato_5,
       -estrato_6, -activi_sin, -activi_tra, -activi_bus, -activi_est, -activi_hog,
       -estcivil_unlibr, -estcivil_casado, -estcivil_solter, -anios_educacion)

# Verify that main base includes 744,904 unique individuals
unique_pbid <- main_base %>% select(personabasicaid) %>% unique() # verify: 744,904

# Capturing covariates from RIPS database
rips_controls <- RIPS_history_file %>% 
  select(personabasicaid, sexo_RIPS) %>% 
  unique() %>% 
  collect() %>% 
  mutate(rips_gender = if_else(sexo_RIPS == "M", 1, 0))

# Capturing covariates from PILA database
pila_controls <- PILA_history_file %>% 
  select(personabasicaid, fechanto) %>% 
  unique() %>% 
  collect() %>% 
  mutate(rips_gender = if_else(sexo_RIPS == "M", 1, 0))

# Capturing personal characteristics (covariates) from master
master_controls <- master %>% 
  select(personabasicaid, puntaje, edad, genero, estrato, activi, nivel, 
         grado, sezo, ingresos, tipodoc, fechanto, estcivil) %>% 
  unique() %>% collect

# All basic information
master_controls <- master_controls %>% 
  left_join(rips_controls, by="personabasicaid") %>% 
  left_join(pila_controls, by="personabasicaid") %>%   

# Creating some variables of interest in master controls
master_controls <- master_controls %>% 
  mutate(
#    genero = if_else(sexo == 1, 1, 0),
    #rips_gender = if_else(sexo_RIPS == "M", 1, 0),    
    #male    =  if_else(genero ==1, 1, 0),
    #female  =  if_else(genero ==0, 1, 0),
    genero    =
    score_m15 =  if_else(puntaje <= 15, 1, 0),    
    score_1520 = if_else(puntaje > 15 & puntaje <= 20, 1, 0),
    score_2025 = if_else(puntaje > 20 & puntaje <= 25, 1, 0),
    score_2530 = if_else(puntaje > 25 & puntaje <= 30.56, 1, 0),
    score_3035 = if_else(puntaje > 30.56 & puntaje <= 35, 1, 0),
    score_3540 = if_else(puntaje > 35 & puntaje <= 40, 1, 0),
    score_4245 = if_else(puntaje > 40 & puntaje <= 45, 1, 0),
    score_45m = if_else(puntaje > 45, 1, 0),  
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
  ) 

main_base <- main_base %>% 
  left_join(master_controls, by="personabasicaid")

write_parquet(main_base, file.path(data_dir, "base_pila_rips_pbid_monthly.parquet"))

















