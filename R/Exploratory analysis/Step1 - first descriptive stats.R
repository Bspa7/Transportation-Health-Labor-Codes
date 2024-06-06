paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

# Directory and creating a new folder to save all the results ------------------

base_dir <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
excel_rips     <- file.path(output_folder, "20240604-try2_descriptives_rips.xlsx")
excel_pila     <- file.path(output_folder, "20240604-try2_descriptives_pila.xlsx")

# ----

# 1. Definitions and functions -----------------------------------------------------

create_variables <- function(df, modulo) {
  df <- df %>%
    mutate(
      !!paste0(modulo, "_cron_cancer")         := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "C", 1, 0),      
      !!paste0(modulo, "_cron_cardiovascular") := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "I", 1, 0),      
      !!paste0(modulo, "_preventivas")         := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "Z" & 
                                                            substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 0:139), 1, 0),  
      !!paste0(modulo, "_prenatales") := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "Z" & 
                                                   substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 300:392), 1, 0),
      !!paste0(modulo, "_tuberculosis") := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "A" & 
                                                     substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 150:199), 1, 0),      
      !!paste0(modulo, "_cron_renal") := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "N" & 
                                                   substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 170:189), 1, 0),
      !!paste0(modulo, "_cron_respiratoria") := if_else(MODULE == modulo & substr(DIAG_PRIN, 1, 1) == "J" & 
                                                          substr(DIAG_PRIN, 2, 4) %in% sprintf("%03d", 401:459), 1, 0)
    )
  return(df)
}


# Define a variable to categorize age into group
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

# Define a variable to categorize age into group
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


# ----

# 2. Initial databases ------------------------------------------------------------

RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')
all_rips          <- open_dataset(RIPS_history_file)
main_rips         <- open_dataset(RIPS_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  filter(!is.na(puntaje))


PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
all_pila          <- open_dataset(PILA_history_file) 
main_pila         <- open_dataset(PILA_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  filter(!is.na(puntaje))

# ----

# 3.1. DESCRIPTIVES: RIPS Bogota -------------------------------------------------

rips_bogota <- all_rips %>%
  filter(COD_DPTO == 11 & COD_MPIO == 1) %>%
  create_variables("c") %>%
  create_variables("p") %>%
  create_variables("h") %>%
  create_variables("u") %>%
  categorize_age() %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE)
  )

# collapse the data
all_bgta_collapse_rips <- rips_bogota %>% 
  group_by(year, monthly_date, sexo_RIPS, grupo_edad, TIPO_USUARIO) %>% 
  summarise(
    across(starts_with("c_"), sum, .names = "sum_{col}"),
    across(starts_with("h_"), sum, .names = "sum_{col}"),
    across(starts_with("p_"), sum, .names = "sum_{col}"),
    across(starts_with("u_"), sum, .names = "sum_{col}")
  ) 

all_bgta_month_rips_module <- rips_bogota %>%
  group_by(monthly_date, MODULE) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

all_bgta_month_rips <- rips_bogota %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))


# ----

# 3.2. DESCRIPTIVES: RIPS (main sample) -----------------------------------------------------------
# Opening dataset RIPS

rips_bogota_main <- main_rips %>%
  filter(COD_DPTO == 11 & COD_MPIO == 1) %>%
  create_variables("c") %>%
  create_variables("p") %>%
  create_variables("h") %>%
  create_variables("u") %>%
  categorize_age() %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE)
  )

# collapse the data
main_bgta_collapse_rips <- rips_bogota_main %>% 
  group_by(year, monthly_date, sexo_RIPS, grupo_edad, TIPO_USUARIO) %>% 
  summarise(
    across(starts_with("c_"), sum, .names = "sum_{col}"),
    across(starts_with("h_"), sum, .names = "sum_{col}"),
    across(starts_with("p_"), sum, .names = "sum_{col}"),
    across(starts_with("u_"), sum, .names = "sum_{col}")
  )

main_bgta_month_rips_module <- rips_bogota_main %>%
  group_by(monthly_date, MODULE) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

main_bgta_month_rips <- rips_bogota_main %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

# ----

# 3.3. DESCRIPTIVES: RIPS-Sisben with score <= 30.56 ---------------------------

rips_bogota_p30 <- main_rips %>%
  filter(COD_DPTO == 11 & COD_MPIO == 1 & puntaje<=30.56) %>%
  create_variables("c") %>%
  create_variables("p") %>%
  create_variables("h") %>%
  create_variables("u") %>%
  categorize_age() %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE)
  )

# collapse the data
p30_bgta_collapse_rips <- rips_bogota_p30 %>% 
  group_by(year, monthly_date, sexo_RIPS, grupo_edad, TIPO_USUARIO) %>% 
  summarise(
    across(starts_with("c_"), sum, .names = "sum_{col}"),
    across(starts_with("h_"), sum, .names = "sum_{col}"),
    across(starts_with("p_"), sum, .names = "sum_{col}"),
    across(starts_with("u_"), sum, .names = "sum_{col}")
  )

p30_bgta_month_rips_module <- rips_bogota_p30 %>%
  group_by(monthly_date, MODULE) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

p30_bgta_month_rips <- rips_bogota_p30 %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))





# ----

# 3.4. DESCRIPTIVES: RIPS-Sisben with score <= 40 ------------------------------

rips_bogota_P40 <- main_rips %>%
  filter(COD_DPTO == 11 & COD_MPIO == 1 & puntaje<=40) %>%
  create_variables("c") %>%
  create_variables("p") %>%
  create_variables("h") %>%
  create_variables("u") %>%
  categorize_age() %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE)
  )

# collapse the data
p40_bgta_collapse_rips <- rips_bogota_P40 %>% 
  group_by(year, monthly_date, sexo_RIPS, grupo_edad, TIPO_USUARIO) %>% 
  summarise(
    across(starts_with("c_"), sum, .names = "sum_{col}"),
    across(starts_with("h_"), sum, .names = "sum_{col}"),
    across(starts_with("p_"), sum, .names = "sum_{col}"),
    across(starts_with("u_"), sum, .names = "sum_{col}")
  )

p40_bgta_month_rips_module <- rips_bogota_P40 %>%
  group_by(monthly_date, MODULE) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

p40_bgta_month_rips <- rips_bogota_P40 %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))


# ----

# 3.5. DESCRIPTIVES: RIPS-Sisben with score between 25 and 45 ------------------

rips_bogota_p2545 <- main_rips %>%
  filter(COD_DPTO == 11 & COD_MPIO == 1 & puntaje<=25 & puntaje<=45) %>%
  create_variables("c") %>%
  create_variables("p") %>%
  create_variables("h") %>%
  create_variables("u") %>%
  categorize_age() %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE)
  )

# collapse the data
p2545_bgta_collapse_rips <- rips_bogota_p2545 %>% 
  group_by(year, monthly_date, sexo_RIPS, grupo_edad, TIPO_USUARIO) %>% 
  summarise(
    across(starts_with("c_"), sum, .names = "sum_{col}"),
    across(starts_with("h_"), sum, .names = "sum_{col}"),
    across(starts_with("p_"), sum, .names = "sum_{col}"),
    across(starts_with("u_"), sum, .names = "sum_{col}")
  )

p2545_bgta_month_rips_module <- rips_bogota_p2545 %>%
  group_by(monthly_date, MODULE) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

p2545_bgta_month_rips <- rips_bogota_p2545 %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))


# ----

# 3.6. SAVING RESULTS: RIPS ----------------------------------------------------
all_bgta_collapse_rips     <- all_bgta_collapse_rips     %>%  collect
all_bgta_month_rips_module <- all_bgta_month_rips_module %>%  collect
all_bgta_month_rips        <- all_bgta_month_rips        %>%  collect

main_bgta_collapse_rips     <- main_bgta_collapse_rips     %>%  collect
main_bgta_month_rips_module <- main_bgta_month_rips_module %>%  collect
main_bgta_month_rips        <- main_bgta_month_rips        %>%  collect

p30_bgta_collapse_rips     <- p30_bgta_collapse_rips     %>%  collect
p30_bgta_month_rips_module <- p30_bgta_month_rips_module %>%  collect
p30_bgta_month_rips        <- p30_bgta_month_rips        %>%  collect

p40_bgta_collapse_rips     <- p40_bgta_collapse_rips     %>%  collect
p40_bgta_month_rips_module <- p40_bgta_month_rips_module %>%  collect
p40_bgta_month_rips        <- p40_bgta_month_rips        %>%  collect

p2545_bgta_collapse_rips     <- p2545_bgta_collapse_rips     %>%  collect
p2545_bgta_month_rips_module <- p2545_bgta_month_rips_module %>%  collect
p2545_bgta_month_rips        <- p2545_bgta_month_rips        %>%  collect

rips_dataframes <- list(all_collapse        = all_bgta_collapse_rips,    
                        all_id_mes_modulo   = all_bgta_month_rips_module,
                        all_id_mes          = all_bgta_month_rips,
                        main_collapse       = main_bgta_collapse_rips,
                        main_id_mes_modulo  = main_bgta_month_rips_module,
                        main_id_mes         = main_bgta_month_rips,
                        p30_collapse        = p30_bgta_collapse_rips,     
                        p30_id_mes_modulo   = p30_bgta_month_rips_module, 
                        p30_id_mes          = p30_bgta_month_rips,        
                        p40_collapse        = p40_bgta_collapse_rips,     
                        p40_id_mes_modulo   = p40_bgta_month_rips_module, 
                        p40_id_mes          = p40_bgta_month_rips,       
                        p2545_collapse      = p2545_bgta_collapse_rips,    
                        p2545_id_mes_modulo = p2545_bgta_month_rips_module,
                        p2545_id_mes        = p2545_bgta_month_rips)
write.xlsx(rips_dataframes, file = excel_rips)

rips_dataframes <- c("all_bgta_collapse_rips",    
                     "all_bgta_month_rips_module",
                     "all_bgta_month_rips",
                     "main_bgta_collapse_rips",
                     "main_bgta_month_rips_module",
                     "main_bgta_month_rips",
                     "p30_bgta_collapse_rips",     
                     "p30_bgta_month_rips_module", 
                     "p30_bgta_month_rips",        
                     "p40_bgta_collapse_rips",     
                     "p40_bgta_month_rips_module", 
                     "p40_bgta_month_rips",       
                     "p2545_bgta_collapse_rips",    
                     "p2545_bgta_month_rips_module",
                     "p2545_bgta_month_rips")

# Eliminar los dataframes
rm(list = rips_dataframes)

# ----

# 4.1. DESCRIPTIVES: PILA Bogotá -----------------------------------------------------------

pila_bogota <- all_pila %>%
  filter(depto_cod == 11 & ciudad_cod == 1) %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE),
    tipo_cotizante = case_when(
      tipo_cotiz == 1 ~ 1,
      tipo_cotiz == 3 ~ 2,
      (tipo_cotiz != 1 & tipo_cotiz != 3) ~ 3,
      is.na(tipo_cotiz) ~ 3
    )
  ) %>% 
  select(personabasicaid, monthly_date, sexomode, year, yearmode, tipo_cotizante) %>% 
  distinct()
pila_bogota <- pila_bogota %>% 
  collect
gc()

pila_bogota <- pila_bogota %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode
  ) %>% 
  categorize_age_pila()

all_pila_by_vars <- pila_bogota %>%
  group_by(monthly_date, sexomode, grupo_edad, tipo_cotizante) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid)) 

all_pila_by_mes <- pila_bogota %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

# ----

# 4.2. DESCRIPTIVES: PILA (main sample) ----------------------------------------

pila_bogota_main <- main_pila %>%
  filter(depto_cod == 11 & ciudad_cod == 1) %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE),
    tipo_cotizante = case_when(
      tipo_cotiz == 1 ~ 1,
      tipo_cotiz == 3 ~ 2,
      (tipo_cotiz != 1 & tipo_cotiz != 3) ~ 3,
      is.na(tipo_cotiz) ~ 3
    )
  ) %>% 
  select(personabasicaid, monthly_date, sexomode, year, yearmode, tipo_cotizante) %>% 
  distinct()
pila_bogota_main <- pila_bogota_main %>% 
  collect
gc()

pila_bogota_main <- pila_bogota_main %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode
  ) %>% 
  categorize_age_pila()

main_pila_by_vars <- pila_bogota_main %>%
  group_by(monthly_date, sexomode, grupo_edad, tipo_cotizante) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

main_pila_by_mes <- pila_bogota_main %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

# ----

# 4.3. DESCRIPTIVES: PILA-Sisben with score <= 30.56 ---------------------------

pila_bogota_p30 <- main_pila %>%
  filter(depto_cod == 11 & ciudad_cod == 1 & puntaje<=30.56) %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE),
    tipo_cotizante = case_when(
      tipo_cotiz == 1 ~ 1,
      tipo_cotiz == 3 ~ 2,
      (tipo_cotiz != 1 & tipo_cotiz != 3) ~ 3,
      is.na(tipo_cotiz) ~ 3
    )
  ) %>% 
  select(personabasicaid, monthly_date, sexomode, year, yearmode, tipo_cotizante) %>% 
  distinct()
pila_bogota_p30 <- pila_bogota_p30 %>% 
  collect
gc()

pila_bogota_p30 <- pila_bogota_p30 %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode
  ) %>% 
  categorize_age_pila()

p30_pila_by_vars <- pila_bogota_p30 %>%
  group_by(monthly_date, sexomode, grupo_edad, tipo_cotizante) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

p30_pila_by_mes <- pila_bogota_p30 %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))


# ----

# 4.4. DESCRIPTIVES: PILA-Sisben with score <= 40 ------------------------------

pila_bogota_p40 <- main_pila %>%
  filter(depto_cod == 11 & ciudad_cod == 1 & puntaje<=40) %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE),
    tipo_cotizante = case_when(
      tipo_cotiz == 1 ~ 1,
      tipo_cotiz == 3 ~ 2,
      (tipo_cotiz != 1 & tipo_cotiz != 3) ~ 3,
      is.na(tipo_cotiz) ~ 3
    )
  ) %>% 
  select(personabasicaid, monthly_date, sexomode, year, yearmode, tipo_cotizante) %>% 
  distinct()
pila_bogota_p40 <- pila_bogota_p40 %>% 
  collect
gc()

pila_bogota_p40 <- pila_bogota_p40 %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode
  ) %>% 
  categorize_age_pila()


p40_pila_by_vars <- pila_bogota_p40 %>%
  group_by(monthly_date, sexomode, grupo_edad, tipo_cotizante) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

p40_pila_by_mes <- pila_bogota_p40 %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))


# ----

# 4.5. DESCRIPTIVES: PILA-Sisben with score between 25 and 45-------------------

pila_bogota_p2545 <- main_pila %>%
  filter(depto_cod == 11 & ciudad_cod == 1  & puntaje>=25 & puntaje<=45) %>%
  mutate(
    DATE = as.Date(DATE),
    monthly_date = floor_date(DATE, "month"),
    year = year(DATE),
    tipo_cotizante = case_when(
      tipo_cotiz == 1 ~ 1,
      tipo_cotiz == 3 ~ 2,
      (tipo_cotiz != 1 & tipo_cotiz != 3) ~ 3,
      is.na(tipo_cotiz) ~ 3
    )
  ) %>% 
  select(personabasicaid, monthly_date, sexomode, year, yearmode, tipo_cotizante) %>% 
  distinct()
pila_bogota_p2545 <- pila_bogota_p2545 %>% 
  collect
gc()

pila_bogota_p2545 <- pila_bogota_p2545 %>% 
  mutate(
    year = as.numeric(year),  
    yearmode = as.numeric(yearmode),
    edad_PILA = year - yearmode
  ) %>% 
  categorize_age_pila()

p2545_pila_by_vars <- pila_bogota_p2545 %>%
  group_by(monthly_date, sexomode, grupo_edad, tipo_cotizante) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))

p2545_pila_by_mes <- pila_bogota_p2545 %>%
  group_by(monthly_date) %>% 
  summarise(total_personabasicaid = n_distinct(personabasicaid))



# ----

# 4.6. SAVING RESULTS ---------------------------------------------------------

pila_dataframes <- list(all_pila_by_vars   = all_pila_by_vars,  
                        all_pila_by_mes    = all_pila_by_mes,   
                        main_pila_by_vars  = main_pila_by_vars, 
                        main_pila_by_mes   = main_pila_by_mes,  
                        p30_pila_by_vars   = p30_pila_by_vars,  
                        p30_pila_by_mes    = p30_pila_by_mes,   
                        p40_pila_by_vars   = p40_pila_by_vars,  
                        p40_pila_by_mes    = p40_pila_by_mes,   
                        p2545_pila_by_vars = p2545_pila_by_vars,
                        p2545_pila_by_mes  = p2545_pila_by_mes)
write.xlsx(pila_dataframes, file = excel_pila)


pila_dataframes <- c("all_pila_by_vars",  
                     "all_pila_by_mes",   
                     "main_pila_by_vars", 
                     "main_pila_by_mes",  
                     "p30_pila_by_vars",  
                     "p30_pila_by_mes",   
                     "p40_pila_by_vars",  
                     "p40_pila_by_mes",   
                     "p2545_pila_by_vars",
                     "p2545_pila_by_mes") 

# Eliminar los dataframes
rm(list = pila_dataframes)





# ----


