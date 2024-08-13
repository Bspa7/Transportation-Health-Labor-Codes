# Date of creation: 12-Aug-2024
# Created by Brayan Pineda
# Objective: This code creates the main database at monthly level


paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_pbid <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/datos_originales/personabasicaid_all_projects/Bases insumos"
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

# Define a function to create the running variables centered in zero and treatments
centrar_puntaje <- function(df) {
  df <- df %>%
    mutate(
      cutoff30 = puntaje - 30.56,
      cutoff40 = puntaje - 40,
      treatment1 = ifelse(puntaje <= 30.56, 1, 0),
      treatment2 = ifelse(puntaje > 30.56 & puntaje <= 40, 1, 0),
      control1 = ifelse(puntaje > 40, 1, 0)
    )
  return(df)
}

# ----

# 2. Initial databases ---------------------------------------------------------

# Databases: SISBEN
#all_sisben_bgta <- open_dataset(sprintf('%s/%s', project_folder, 'data/sisben3_bgta_demog.parquet')) %>% glimpse
# Databases: PERSONABASICAID2010 
#base_pbid2010   <- open_dataset(sprintf('%s/%s', project_folder, 'data/personabasicaid_pila2010.parquet')) %>% glimpse
# Databases: MASTER
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>% glimpse
# Databases: PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')  %>% glimpse
# Databases: RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet') %>% glimpse


# ----

# 3. First modifications to initial databases ----------------------------------

# Modifications to PILA: filter to Bogota into 2015-2019
all_pila          <- open_dataset(PILA_history_file) %>% 
  rename(fechanto_pila = fechanto, date_pila = DATE) %>% 
  filter(depto_cod == 11 & ciudad_cod == 1) %>%
  mutate(
    date_pila = as.Date(date_pila),
    monthly_date = floor_date(date_pila, "month"),
    year = year(date_pila)
  ) %>% 
  filter(year>=2015 & year<=2019) %>% glimpse()

# Modifications to RIPS: filter to Bogota into 2015-2019
all_rips          <- open_dataset(RIPS_history_file) %>%
  rename(date_rips = DATE) %>% 
  filter(COD_DPTO == 11 & COD_MPIO == 1)  %>%
  mutate(
    date_rips      = as.Date(date_rips),
    year           = year(date_rips),
    monthly_date = floor_date(date_rips, "month"),
  ) %>% 
  filter(year>=2015 & year<=2019) %>% glimpse()

# Unique personabasicaid - 744,904
unique_pbid <- master %>% 
  select(personabasicaid) %>% 
  unique()

aux_pbid <- master %>% 
  select(personabasicaid) %>% 
  unique() %>% collect()

# Skeleton with all individuals observed in all periods
aux_time <- seq(as.Date("2015-01-01"), as.Date("2019-12-01"), by = "month")
aux_pbid <- unique(aux_pbid$personabasicaid)
skeleton <- expand.grid(monthly_date = aux_time, personabasicaid = aux_pbid)
rm(aux_pbid)

# ----

# 4. Creating the database (balanced) of RIPS-SISBEN ---------------------------
merged_rips <- unique_pbid %>% 
  left_join(all_rips, by="personabasicaid") 
rm(all_rips)
gc()
merged_rips <- merged_rips %>%
  group_by(monthly_date, personabasicaid) %>%  
  summarise(
    n_visitas_rips = n(),
    d_visitas_rips = if_else(n() > 0, 1, 0),
    n_consultas = sum(MODULE == "c", na.rm = TRUE),
    d_consultas = if_else(sum(MODULE == "c", na.rm = TRUE) > 0, 1, 0),
    n_hospitalizaciones = sum(MODULE == "h", na.rm = TRUE),
    d_hospitalizaciones = if_else(sum(MODULE == "h", na.rm = TRUE) > 0, 1, 0),
    n_procedimientos = sum(MODULE == "p", na.rm = TRUE),
    d_procedimientos = if_else(sum(MODULE == "p", na.rm = TRUE) > 0, 1, 0),
    n_urgencias = sum(MODULE == "u", na.rm = TRUE),
    d_urgencias = if_else(sum(MODULE == "u", na.rm = TRUE) > 0, 1, 0),
    c_preven = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 0:139), 1, 0), na.rm = TRUE),
    d_preven = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 0:139), 1, 0), na.rm = TRUE) > 0, 1, 0),
    c_prenat = sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE),
    d_prenat = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE) > 0, 1, 0)
  )

rips_est <- merged_rips %>%
  mutate(monthly_date = as.Date(monthly_date)) %>% 
  collect

rm(merged_rips)
gc()

# Balanced dataframe with the original
rips_est_balanced <- skeleton %>%
  left_join(rips_est, by = c("monthly_date", "personabasicaid"))

# Cleaning memory
rm(rips_est)
gc()

# Replacing NA's to zero in main variables
rips_est_balanced <- rips_est_balanced %>%
  mutate(across(c(n_visitas_rips, n_consultas, n_hospitalizaciones,
                  n_procedimientos, n_urgencias,c_preven, c_prenat,
                  d_visitas_rips, d_consultas, d_hospitalizaciones,
                  d_procedimientos, d_urgencias, d_preven, d_prenat),
                ~ replace_na(., 0)))

# ----

# 5. Creating the database (balanced) of PILA-SISBEN ---------------------------

all_pila_c <- all_pila %>% collect
unique_pbid <- unique_pbid %>% collect
merged_pila <- unique_pbid %>% 
  left_join(all_pila_c, by="personabasicaid") 

rm(all_pila_c, all_pila)
gc()

merged_pila <- merged_pila %>%
  mutate(
    pila_indep = tipo_cotiz %in% c(3, 16, 41, 42, 2, 59, 57, 66),
    pila_depen = !pila_indep
  ) %>% 
  group_by(monthly_date, personabasicaid) %>%  
  summarise(
    n_registros_pila  = n(),
    d_registros_pila  = if_else(n() > 0, 1, 0),
    ibc_salud         = max(ibc_salud,    na.rm = TRUE),
    sal_dias_cot_sum  = sum(sal_dias_cot, na.rm = TRUE),
    sal_dias_cot_max  = max(sal_dias_cot, na.rm = TRUE),    
    pila_indep = max(pila_indep, na.rm = TRUE),
    pila_depen = max(pila_depen, na.rm = TRUE)
  )

pila_est <- merged_pila %>%
  mutate(monthly_date = as.Date(monthly_date))

rm(merged_pila)
gc()

# Balanced dataframe with the original
pila_est_balanced <- skeleton %>%
  left_join(pila_est, by = c("monthly_date", "personabasicaid"))

# Cleaning memory
rm(pila_est)
gc()

# Replacing NA's to zero in main variables
pila_est_balanced <- pila_est_balanced %>%
  mutate(across(c(n_registros_pila, d_registros_pila, ibc_salud, sal_dias_cot_sum, sal_dias_cot_max, pila_indep, pila_depen),
                ~ replace_na(., 0)))

# ----

# 6. Creating the final database (balanced) of SISBEN-PILA-RIPS ----------------
master <- master %>% collect()
main_base <- rips_est_balanced %>% 
  left_join(pila_est_balanced, by = c("personabasicaid", "monthly_date")) %>% 
  left_join(master, by = "personabasicaid") %>% 
  centrar_puntaje() %>% 
  mutate(
    genero = if_else(sexo == 1, 1, 0),
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

rm(master, pila_est_balanced, rips_est_balanced, skeleton, unique_pbid)
gc()
write_parquet(main_base, file.path(data_dir, "base_pila_rips_pbid_monthly.parquet"))

unique_pbid <- main_base %>% 
  select(personabasicaid, puntaje) %>% 
  unique() %>% 
  collect()

# Imprimir texto en colores
cat(green("---------- PLEASE SEND A PICTURE OF THIS RESULTS: ----------\n"))
hist(main_base$puntaje)
summary(main_base)
summary(unique_pbid)




# ----








