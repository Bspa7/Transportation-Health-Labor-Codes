# Date of creation: 30-Sept-2024
# Created by Brayan Pineda
# Last modification: 27-Oct-2024 by Brayan Pineda
# Last modification: Create new outcomes disaggregating consultations into 4 sub outcomes.
# Objective: This code creates the main database at monthly level for sample with scores between 10-45 from 2010 to 2019.

################################################################################
#  Information from 2010 to 2014
################################################################################

# Packages and folders ---------------------------------------------------------
paquetes <- c("rddensity", "readxl", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")

# ----

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

# MASTER
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>%
  filter(puntaje>=10 & puntaje<=45) %>% glimpse()
# PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')  %>% glimpse
# RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet') %>% glimpse 

# Opening the reference table of CUPS codes
# Define path to read the reference file for CUPS codes
excel_file <- file.path(data_dir, "Data_health/20240924 - general_diagnoses.xlsx")
# Read the reference table of CUPS codes
ministry_CUPS_data <- read_excel(excel_file, sheet = "ministry_CUPS_reference_table")  %>% 
  rename(COD_CUPS = Codigo) %>% 
  select(COD_CUPS, Nombre, Descripcion)


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
  filter(year>=2010 & year<=2014) %>% glimpse()

# Modifications to RIPS: filter to Bogota into 2015-2019
all_rips          <- open_dataset(RIPS_history_file) %>%
  rename(date_rips = DATE) %>% 
  filter(COD_DPTO == 11 & COD_MPIO == 1)  %>%
  mutate(
    date_rips      = as.Date(date_rips),
    year           = year(date_rips),
    monthly_date = floor_date(date_rips, "month"),
  ) %>% 
  filter(year>=2010 & year<=2014) %>% glimpse()

# Unique personabasicaid
unique_pbid <- master %>% 
  select(personabasicaid) %>% 
  unique()

aux_pbid <- master %>% 
  select(personabasicaid) %>% 
  unique() %>% collect()

# Skeleton with all individuals observed in all periods
aux_time <- seq(as.Date("2010-01-01"), as.Date("2014-12-01"), by = "month")
aux_pbid <- unique(aux_pbid$personabasicaid)
skeleton <- expand.grid(monthly_date = aux_time, personabasicaid = aux_pbid)
rm(aux_pbid, aux_time)

# ----

all_rips <- all_rips %>% 
  left_join(ministry_CUPS_data, by="COD_CUPS") 

# 4. Creating the database (balanced) of RIPS-SISBEN ---------------------------
merged_rips <- unique_pbid %>% 
  left_join(all_rips, by="personabasicaid") 
rm(all_rips)
gc()

rips_est <- merged_rips %>% 
  group_by(monthly_date, personabasicaid) %>%  
  summarise(
    # Informacion relacionada con conteos y dummies de RIPS en sí mismo  
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
    d_prenat = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE) > 0, 1, 0),
    
    # Agregar las dummies para TIPO_USUARIO (contributivo, subsidiado, otros)
    reg_contributivo            = if_else(sum(TIPO_USUARIO == 1, na.rm = TRUE) > 0, 1, 0),
    reg_subsidiado              = if_else(sum(TIPO_USUARIO == 2, na.rm = TRUE) > 0, 1, 0),
    reg_vinculado               = if_else(sum(TIPO_USUARIO == 3, na.rm = TRUE) > 0, 1, 0),
    reg_particular              = if_else(sum(TIPO_USUARIO == 4, na.rm = TRUE) > 0, 1, 0),
    reg_otro                    = if_else(sum(TIPO_USUARIO == 5, na.rm = TRUE) > 0, 1, 0),
    reg_desp_contributivo       = if_else(sum(TIPO_USUARIO == 6, na.rm = TRUE) > 0, 1, 0),
    reg_desp_subsidiado         = if_else(sum(TIPO_USUARIO == 7, na.rm = TRUE) > 0, 1, 0),
    reg_desp_no_asegurado       = if_else(sum(TIPO_USUARIO == 8, na.rm = TRUE) > 0, 1, 0),
    
    # Variables para genero
    rips_gender = if_else(sum(sexo_RIPS == "M", na.rm = TRUE) > 0, 1, 0),
    rips_female = if_else(sum(sexo_RIPS == "F", na.rm = TRUE) > 0, 1, 0),
    rips_male   = if_else(sum(sexo_RIPS == "M", na.rm = TRUE) > 0, 1, 0),
    
    # Clasificacion de las consultas usando los CUPS
    d_cons_gral = if_else(sum(MODULE == "c" & str_detect(Nombre, "GENERAL"), na.rm = TRUE) > 0, 1, 0),
    n_cons_gral = sum(MODULE == "c" & str_detect(Nombre, "GENERAL"), na.rm = TRUE),
    
    d_cons_esp = if_else(sum(MODULE == "c" & str_detect(Nombre, "ESPECIAL"), na.rm = TRUE) > 0, 1, 0),
    n_cons_esp = sum(MODULE == "c" & str_detect(Nombre, "ESPECIAL"), na.rm = TRUE),
    
    d_cons_primera = if_else(sum(MODULE == "c" & str_detect(Nombre, "PRIMERA"), na.rm = TRUE) > 0, 1, 0),
    n_cons_primera = sum(MODULE == "c" & str_detect(Nombre, "PRIMERA"), na.rm = TRUE),
    
    d_cons_control = if_else(sum(MODULE == "c" & str_detect(Nombre, "SEGUIMIENTO|CONTROL"), na.rm = TRUE) > 0, 1, 0),
    n_cons_control = sum(MODULE == "c" & str_detect(Nombre, "SEGUIMIENTO|CONTROL"), na.rm = TRUE)    
    
  )
gc()
rips_est <- rips_est %>%
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
                  d_procedimientos, d_urgencias, d_preven, d_prenat,
                  reg_contributivo, reg_subsidiado, reg_vinculado, 
                  reg_particular, reg_otro, reg_desp_contributivo, 
                  reg_desp_subsidiado, reg_desp_no_asegurado, 
                  d_cons_gral, n_cons_gral, d_cons_esp, n_cons_esp, 
                  d_cons_primera, n_cons_primera, d_cons_control, n_cons_control),
                ~ replace_na(., 0)))

write_parquet(rips_est_balanced, file.path(data_dir, "panel_rips_s1045_t1014.parquet"))

# ----

# 5. Creating the database (balanced) of PILA-SISBEN ---------------------------

all_pila <- all_pila %>% collect()
unique_pbid <- unique_pbid %>% collect()

merged_pila <- unique_pbid %>% 
  left_join(all_pila, by="personabasicaid") 

rm(all_pila)
gc()

merged_pila_2 <- merged_pila %>% 
  mutate(
    pila_indep = tipo_cotiz %in% c(3, 16, 41, 42, 2, 59, 57, 66),
    pila_depen = !pila_indep
  ) %>% 
  group_by(monthly_date, personabasicaid) %>%  
  summarise(
    # Numero de registros totales en PILA y dummy de registro por mes
    n_registros_pila  = n(),
    d_registros_pila  = if_else(n() > 0, 1, 0),
    
    # IBC variables
    ibc_salud_max     = max(ibc_salud, na.rm = TRUE),
    ibc_ccf_max       = max(ibc_ccf,   na.rm = TRUE),
    ibc_pens_max      = max(ibc_pens,  na.rm = TRUE),
    ibc_rprof_max     = max(ibc_rprof, na.rm = TRUE),
    
    # Income 
    income_base_max  = max(salario_bas,    na.rm = TRUE),
    income_base_sum  = sum(salario_bas,    na.rm = TRUE),    
    
    # Number of contributed days
    sal_dias_cot_max  = max(sal_dias_cot, na.rm = TRUE),    
    sal_dias_cot_sum  = sum(sal_dias_cot, na.rm = TRUE),    
    
    # Type of contributor
    pila_indep = max(pila_indep, na.rm = TRUE),
    pila_depen = max(pila_depen, na.rm = TRUE)
  )  %>% glimpse()

pila_est <- merged_pila_2 %>%
  mutate(monthly_date = as.Date(monthly_date)) 

rm(merged_pila, merged_pila_2)
gc()

# Balanced dataframe with the original
pila_est_balanced <- skeleton %>%
  left_join(pila_est, by = c("monthly_date", "personabasicaid"))

# Cleaning memory
rm(pila_est, skeleton)
gc()

# Replacing NA's to zero in main variables
pila_est_balanced <- pila_est_balanced %>%
  mutate(across(c(n_registros_pila, d_registros_pila, ibc_salud_max, ibc_ccf_max, ibc_pens_max, ibc_rprof_max,
                  income_base_max, income_base_sum, sal_dias_cot_sum, sal_dias_cot_max, pila_indep, pila_depen),
                ~ replace_na(., 0)))
write_parquet(pila_est_balanced, file.path(data_dir, "panel_pila_s1045_t1014.parquet"))

# ----

# 6. Creating the final database (balanced) of SISBEN-PILA-RIPS ----------------

master <- master %>% collect()
main_base <- rips_est_balanced %>% 
  left_join(pila_est_balanced, by = c("personabasicaid", "monthly_date")) %>% 
  left_join(master, by = "personabasicaid") %>% 
  centrar_puntaje() %>% 
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
    
    # Calculando la edad por mes y por año
    date_fechanto  = as.Date(fechanto),
    year_fechanto  = year(date_fechanto),
    month_fechanto = floor_date(date_fechanto, "month"),
    # Edad en cada mes
    edad_mensual = floor(interval(fechanto, monthly_date) / years(1)),
    # Edad que cumple en el año
    year_current = year(monthly_date),
    edad_anual   = year_current - year_fechanto,        
    
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

rm(master, pila_est_balanced, rips_est_balanced, unique_pbid)
gc()
write_parquet(main_base, file.path(data_dir, "panel_total_sample1045_t1014.parquet"))

# -----

# 7. Clean memory -----------------------------------------------------------------

rm(list=ls(all=TRUE))
gc()
# ----



################################################################################
#  Information from 2015 to 2019
################################################################################

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
#base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")
# ----

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

# MASTER
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>%
  filter(puntaje>=10 & puntaje<=45)
# PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')  %>% glimpse
# RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet') %>% glimpse

# Opening the reference table of CUPS codes
# Define path to read the reference file for CUPS codes
excel_file <- file.path(data_dir, "Data_health/20240924 - general_diagnoses.xlsx")
# Read the reference table of CUPS codes
ministry_CUPS_data <- read_excel(excel_file, sheet = "ministry_CUPS_reference_table")  %>% 
  rename(COD_CUPS = Codigo) %>% 
  select(COD_CUPS, Nombre, Descripcion)

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

all_rips <- all_rips %>% collect()

# Unique personabasicaid
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
rm(aux_pbid, aux_time)

# ----

all_rips <- all_rips %>% 
  left_join(ministry_CUPS_data, by="COD_CUPS") 

# 4. Creating the database (balanced) of RIPS-SISBEN ---------------------------
merged_rips <- unique_pbid %>% 
  left_join(all_rips, by="personabasicaid") 
rm(all_rips)
gc()

rips_est <- merged_rips %>% 
  group_by(monthly_date, personabasicaid) %>%  
  summarise(
    # Informacion relacionada con conteos y dummies de RIPS en sí mismo  
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
    d_prenat = if_else(sum(if_else(MODULE == "c" & substr(DIAG_PRIN, 1, 1) == "Z" & substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0), na.rm = TRUE) > 0, 1, 0),
    
    # Agregar las dummies para TIPO_USUARIO (contributivo, subsidiado, otros)
    reg_contributivo            = if_else(sum(TIPO_USUARIO == 1, na.rm = TRUE) > 0, 1, 0),
    reg_subsidiado              = if_else(sum(TIPO_USUARIO == 2, na.rm = TRUE) > 0, 1, 0),
    reg_vinculado               = if_else(sum(TIPO_USUARIO == 3, na.rm = TRUE) > 0, 1, 0),
    reg_particular              = if_else(sum(TIPO_USUARIO == 4, na.rm = TRUE) > 0, 1, 0),
    reg_otro                    = if_else(sum(TIPO_USUARIO == 5, na.rm = TRUE) > 0, 1, 0),
    reg_desp_contributivo       = if_else(sum(TIPO_USUARIO == 6, na.rm = TRUE) > 0, 1, 0),
    reg_desp_subsidiado         = if_else(sum(TIPO_USUARIO == 7, na.rm = TRUE) > 0, 1, 0),
    reg_desp_no_asegurado       = if_else(sum(TIPO_USUARIO == 8, na.rm = TRUE) > 0, 1, 0),
    
    # Variables para genero
    rips_gender = if_else(sum(sexo_RIPS == "M", na.rm = TRUE) > 0, 1, 0),
    rips_female = if_else(sum(sexo_RIPS == "F", na.rm = TRUE) > 0, 1, 0),
    rips_male   = if_else(sum(sexo_RIPS == "M", na.rm = TRUE) > 0, 1, 0),
    
    # Clasificacion de las consultas usando los CUPS
    d_cons_gral = if_else(sum(MODULE == "c" & str_detect(Nombre, "GENERAL"), na.rm = TRUE) > 0, 1, 0),
    n_cons_gral = sum(MODULE == "c" & str_detect(Nombre, "GENERAL"), na.rm = TRUE),
    
    d_cons_esp = if_else(sum(MODULE == "c" & str_detect(Nombre, "ESPECIAL"), na.rm = TRUE) > 0, 1, 0),
    n_cons_esp = sum(MODULE == "c" & str_detect(Nombre, "ESPECIAL"), na.rm = TRUE),
    
    d_cons_primera = if_else(sum(MODULE == "c" & str_detect(Nombre, "PRIMERA"), na.rm = TRUE) > 0, 1, 0),
    n_cons_primera = sum(MODULE == "c" & str_detect(Nombre, "PRIMERA"), na.rm = TRUE),
    
    d_cons_control = if_else(sum(MODULE == "c" & str_detect(Nombre, "SEGUIMIENTO|CONTROL"), na.rm = TRUE) > 0, 1, 0),
    n_cons_control = sum(MODULE == "c" & str_detect(Nombre, "SEGUIMIENTO|CONTROL"), na.rm = TRUE)    
    
    
  )
gc()
rips_est <- rips_est %>%
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
                  d_procedimientos, d_urgencias, d_preven, d_prenat,
                  reg_contributivo, reg_subsidiado, reg_vinculado, 
                  reg_particular, reg_otro, reg_desp_contributivo, 
                  reg_desp_subsidiado, reg_desp_no_asegurado, 
                  d_cons_gral, n_cons_gral, d_cons_esp, n_cons_esp, 
                  d_cons_primera, n_cons_primera, d_cons_control, n_cons_control),
                ~ replace_na(., 0)))

write_parquet(rips_est_balanced, file.path(data_dir, "panel_rips_s1045_t1519.parquet"))

# ----

# 5. Creating the database (balanced) of PILA-SISBEN ---------------------------

all_pila <- all_pila %>% collect()
unique_pbid <- unique_pbid %>% collect()

merged_pila <- unique_pbid %>% 
  left_join(all_pila, by="personabasicaid") 

rm(all_pila)
gc()

merged_pila_2 <- merged_pila %>% 
  mutate(
    pila_indep = tipo_cotiz %in% c(3, 16, 41, 42, 2, 59, 57, 66),
    pila_depen = !pila_indep
  ) %>% 
  group_by(monthly_date, personabasicaid) %>%  
  summarise(
    # Numero de registros totales en PILA y dummy de registro por mes
    n_registros_pila  = n(),
    d_registros_pila  = if_else(n() > 0, 1, 0),
    
    # IBC variables
    ibc_salud_max     = max(ibc_salud, na.rm = TRUE),
    ibc_ccf_max       = max(ibc_ccf,   na.rm = TRUE),
    ibc_pens_max      = max(ibc_pens,  na.rm = TRUE),
    ibc_rprof_max     = max(ibc_rprof, na.rm = TRUE),
    
    # Income 
    income_base_max  = max(salario_bas,    na.rm = TRUE),
    income_base_sum  = sum(salario_bas,    na.rm = TRUE),    
    
    # Number of contributed days
    sal_dias_cot_max  = max(sal_dias_cot, na.rm = TRUE),    
    sal_dias_cot_sum  = sum(sal_dias_cot, na.rm = TRUE),    
    
    # Type of contributor
    pila_indep = max(pila_indep, na.rm = TRUE),
    pila_depen = max(pila_depen, na.rm = TRUE)
  )  %>% glimpse()

pila_est <- merged_pila_2 %>%
  mutate(monthly_date = as.Date(monthly_date)) 

rm(merged_pila, merged_pila_2)
gc()

# Balanced dataframe with the original
pila_est_balanced <- skeleton %>%
  left_join(pila_est, by = c("monthly_date", "personabasicaid"))

# Cleaning memory
rm(pila_est, skeleton)
gc()

# Replacing NA's to zero in main variables
pila_est_balanced <- pila_est_balanced %>%
  mutate(across(c(n_registros_pila, d_registros_pila, ibc_salud_max, ibc_ccf_max, ibc_pens_max, ibc_rprof_max,
                  income_base_max, income_base_sum, sal_dias_cot_sum, sal_dias_cot_max, pila_indep, pila_depen),
                ~ replace_na(., 0)))
write_parquet(pila_est_balanced, file.path(data_dir, "panel_pila_s1045_t1519.parquet"))

# ----

# 6. Creating the final database (balanced) of SISBEN-PILA-RIPS ----------------

master <- master %>% collect()
main_base <- rips_est_balanced %>% 
  left_join(pila_est_balanced, by = c("personabasicaid", "monthly_date")) %>% 
  left_join(master, by = "personabasicaid") %>% 
  centrar_puntaje() %>% 
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
    
    # Calculando la edad por mes y por año
    date_fechanto  = as.Date(fechanto),
    year_fechanto  = year(date_fechanto),
    month_fechanto = floor_date(date_fechanto, "month"),
    # Edad en cada mes
    edad_mensual = floor(interval(fechanto, monthly_date) / years(1)),
    # Edad que cumple en el año
    year_current = year(monthly_date),
    edad_anual   = year_current - year_fechanto,        
    
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

rm(master, pila_est_balanced, rips_est_balanced, unique_pbid)
gc()
write_parquet(main_base, file.path(data_dir, "panel_total_sample1045_t1519.parquet"))

# -----

# 7. Clean memory -----------------------------------------------------------------

rm(list=ls(all=TRUE))

# ----

################################################################################
#  Append to obtain dataset from 2010 to 2019
################################################################################

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
#base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")

sample_t1014 <- open_dataset(sprintf('%s/%s', data_dir, 'panel_total_sample1045_t1014.parquet')) %>% collect
sample_t1520 <- open_dataset(sprintf('%s/%s', data_dir, 'panel_total_sample1045_t1519.parquet')) %>% collect

sample_t1019 <- bind_rows(sample_t1014, sample_t1520)

write_parquet(sample_t1019, file.path(data_dir, "panel_total_sample1045_t1019.parquet"))



cat(green("---------- PLEASE SEND A PICTURE OF THIS RESULTS: ----------\n"))
# Calcular y mostrar el número de personas únicas en la base de datos
num_personas <- sample_t1019 %>%
  summarise(n_personas = n_distinct(personabasicaid)) %>%
  pull(n_personas)
cat("Número de personas únicas (personabasicaid):", num_personas, "\n")


# Imprimir texto en colores
hist(sample_t1019$puntaje)
summary(sample_t1019)


rm(list=ls(all=TRUE))


# Saving the dataset in DTA format (Stata)

paquetes <- c("rddensity", "haven", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"


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

rm(list=ls(all=TRUE))









