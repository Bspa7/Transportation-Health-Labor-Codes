paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


# This code creates the main database at monthly level to run estimations at this level

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

# ----

# 2. Initial databases ---------------------------------------------------------

# PILA filtered in 2010
#base_pbid2010 <- open_dataset(sprintf('%s/%s', base_pbid, 'personabasicaid_pila2010.parquet'))
#write_parquet(base_pbid2010, file.path(data_dir, "personabasicaid_pila2010.parquet"))

# Databases: SISBEN
all_sisben_bgta <- open_dataset(sprintf('%s/%s', project_folder, 'data/sisben3_bgta_demog.parquet')) %>% glimpse
# Databases: MASTER
master          <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>% glimpse
# Databases: PERSONABASICAID2010 
base_pbid2010   <- open_dataset(sprintf('%s/%s', project_folder, 'data/personabasicaid_pila2010.parquet')) %>% glimpse
# Databases: PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')  
# Databases: RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet') 

# ----

# 3. First modifications to initial databases ----------------------------------

# Modifications to PILA: filter to Bogota into 2012-2019
all_pila          <- open_dataset(PILA_history_file) %>% 
  rename(fechanto_pila = fechanto, date_pila = DATE) %>% 
  filter(depto_cod == 11 & ciudad_cod == 1) %>%
  mutate(
    date_pila = as.Date(date_pila),
    monthly_date = floor_date(date_pila, "month"),
    year = year(date_pila)
  ) %>% 
  filter(year>=2012 & year<=2019) %>% glimpse

# Modifications to RIPS: filter to Bogota into 2012-2019
all_rips          <- open_dataset(RIPS_history_file) %>%
  rename(date_rips = DATE) %>% 
  filter(COD_DPTO == 11 & COD_MPIO == 1)  %>%
  mutate(
    date_rips      = as.Date(date_rips),
    year           = year(date_rips),
    monthly_date = floor_date(date_pila, "month"),
  ) %>% 
  filter(year>=2012 & year<=2019) %>% glimpse

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


# Creating a dummy to identify personabasicaid in 2010
master <- master %>% 
  left_join(base_pbid2010 %>%  mutate(pbid2010 = 1), by='personabasicaid')
master <- master %>% 
  mutate(pbid2010 = ifelse(is.na(pbid2010), 0, pbid2010))


# ----

# 4. Creating the database (balanced) of RIPS-SISBEN ---------------------------
merged_rips <- master %>% 
  left_join(all_rips, by="personabasicaid")  

rips_npbid <- merged_rips %>% 
  select(personabasicaid) %>% 
  unique() %>% 
  collect

merged_rips <- merged_rips %>%
  categorize_age() %>% 
  group_by(monthly_date, personabasicaid) %>%  
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
  mutate(monthly_date = as.Date(monthly_date)) %>% 
  centrar_puntaje() %>% 
  collect

rm(merged_rips)
gc()


# Skeleton with all individuals observed in all periods
aux_time <- seq(as.Date("2012-01-01"), as.Date("2019-12-01"), by = "month")
aux_pbid <- unique(rips_est$personabasicaid)
skeleton <- expand.grid(monthly_date = aux_time, personabasicaid = aux_pbid)

# Balanced dataframe with the original
rips_est_balanced <- merge(skeleton, rips_est, by = c("monthly_date", "personabasicaid"), all.x = TRUE)

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
  mutate(edad_sisben = floor(interval(fechanto, monthly_date) / years(1)))

# Changing the direction of the running variable | Interpretation
rips_est_balanced <-  rips_est_balanced %>% 
  mutate(cutoff30_origi = cutoff30,
         cutoff40_origi = cutoff40,
         cutoff30 = cutoff30*-1,
         cutoff40 = cutoff40*-1)

# ----

# 5. Creating the database (balanced) of PILA-SISBEN ---------------------------

all_pila_c <- all_pila %>% collect

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
  group_by(monthly_date, personabasicaid) %>%  
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
  mutate(monthly_date = as.Date(monthly_date))

rm(merged_pila)
gc()

# Skeleton with all individuals observed in all periods
aux_time <- seq(as.Date("2012-01-01"), as.Date("2019-12-01"), by = "month")
aux_pbid <- unique(pila_est$personabasicaid)
skeleton <- expand.grid(monthly_date = aux_time, personabasicaid = aux_pbid)

# Balanced dataframe with the original
pila_est_balanced <- merge(skeleton, pila_est, by = c("monthly_date", "personabasicaid"), all.x = TRUE)

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
  left_join(pila_est_balanced, by=c("personabasicaid", "monthly_date"))

write_parquet(main_base, file.path(data_dir, "base_pila_rips_pbid_monthy.parquet"))

#merged_data

#merged_data <- merged_data %>% collect

#write_parquet(merged_data, file.path(data_dir, "base_pila_rips_pbid.parquet"))














# ----



