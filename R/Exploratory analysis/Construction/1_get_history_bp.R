# HISTORIAL PILA y RIPS. SELECTED_COLUMNS (TIPOS DE DATOS) Y ARCHIVOS.
# Limpieza de la muestra ids_finales. REGLAS DE LIMPIEZA.
# colapsa PILA y RIPS en la periodicidad deseada. CREATED_COLUMNS.POR CADA VARIABLE TENER UNA REGLA DE DECISION Y PERIODICIDAD.
# Crea la master. CUALES VARIABLES
# Crea un panel pegando todas las bases. CUALES VARIABLES.
library("arrow"); library("tidyverse"); library("haven"); library("dplyr") 

project_folder <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"


ids <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet')) %>% 
  distinct(personabasicaid) %>% collect %>% unlist %>% unname

RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
# Get RIPS history --------------------------------------------------------

folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/_RIPS'
files <- list.files(folder, pattern = 'proc|urg|hos|cons', 
                    recursive = T)
dict_path <- '../Data/RIPS_dictionary.xlsx'
dict <- read_excel(dict_path, sheet = 'colnames')
names(dict) <- sapply(str_split(names(dict), '\\.'),
                      function(x) paste(tail(x, 2), collapse = '.'))

# MODIFY: SELECT THE DESIRED COLUMNS (USE THE UNINAME OF THE DICTIONARY).
selected_columns <- c('PERSONABASICAID', 'SEXO', 'EDAD','TIPO_USUARIO', 
                      'CAUSA_EXTERNA', 'DATE_JUAN', 'DIAG_PRIN', 'DIAG_R1',
                      'COD_DIAG_R2', 'COD_DIAG_R3', 'COD_CUPS',
                      'COD_DPTO', 'COD_MPIO')

df <- NULL
tic()
for (file in files) {
  cat(paste('Began', file))
  tic()
  # Auxiliary variables to select proper names and get the desired types. 
  (df_selected <- filter(dict, uniname %in% selected_columns) %>% 
      select(uniname, uniclass, file) %>% drop_na(file) %>% 
      replace(is.na(.), ''))
  (selected_columns_file <- df_selected[[file]])
  (selected_columns1 <- df_selected$uniname)
  (desired_classes <- df_selected$uniclass)
  
  module <- str_sub(str_match(file, 'cons|hosp|urg|proc'), end = 1L)
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    # Select variables and rename.
    select(all_of(selected_columns_file)) %>%
    rename_at(vars(selected_columns_file), function(x) selected_columns1) %>%
    # MODIFY: PROCESS THE DATA AS NEEDED. 
    filter(PERSONABASICAID %in% ids) %>%
    mutate(MODULE = module) %>% 
    # Unify column types.
    mutate(
      across(all_of(selected_columns1[desired_classes == 'numeric']), as.numeric),
      across(all_of(selected_columns1[desired_classes == 'character']), as.character)
    ) %>% collect
  
  if (module %in% c('c', 'p')) {
    df0 %>% mutate(year_file = str_extract(file, '\\d{4}') %>% as.integer)
  }
  
  df <- bind_rows(df, df0)
  
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}
# MODIFY: PROCESS AND SAVE THE REQUIRED DATA.
df %>% 
  mutate(
    YEAR = str_sub(DATE_JUAN, 1L, 4L) %>% as.integer(),
    DATE = ymd(str_sub(DATE_JUAN, 1L, 10L))) %>% 
  select(-DATE_JUAN) %>% 
  mutate(across(
    all_of(selected_columns[str_detect(selected_columns, 'DIAG')]), ~toupper(.)
  )) %>%
  rename(sexo_RIPS = SEXO, edad_RIPS = EDAD,
         personabasicaid = PERSONABASICAID) %>% 
  write_parquet(RIPS_history_file)
sprintf('\n\t RIPS history retrieved in %f mins\n', get_values_tic_msg('min')) %>% cat


# Get PILA history --------------------------------------------------------

tic()
folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/_PILA/'
files <- list.files(folder, pattern = '2009|20[12][0-9]')

dict_path <- '../Data/PILA_dictionary.xlsx'
dict <- read_excel(dict_path, sheet = 'colnames')
names(dict) <- sapply(str_split(names(dict), '\\.'),
                      function(x) paste(tail(x, 2), collapse = '.'))

# MODIFY: SELECT THE DESIRED COLUMNS (USE THE UNINAME OF THE DICTIONARY).
selected_columns <- c(
  'personabasicaid', 'fecha_cobertura', 'fechanto', 'sexo',
  'ibc_ccf', 'ibc_pens', 'ibc_rprof', 'ibc_salud', 'salario_bas',
  'tipo_ap', 'tipo_cotiz', 'tipo_per',
  'sal_dias_cot', 'tipo_cotiz', 'depto_cod', 'ciudad_cod', 
  'licen_mat', 'incap_gral', 'incap_trab', 'ciiu', 'sexomode', 'yearmode')


df <- NULL
tic()
for (file in files) {
  cat(paste('Began', file))
  tic()
  # Auxiliary variables to select proper names and get the desired types. 
  (df_selected <- filter(dict, uniname %in% selected_columns) %>% 
      select(uniname, uniclass, file) %>% drop_na(file) %>% 
      replace(is.na(.), ''))
  (selected_columns_file <- df_selected[[file]])
  (selected_columns1 <- df_selected$uniname)
  (desired_classes <- df_selected$uniclass)
  
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    # Select variables and rename.
    select(all_of(selected_columns_file)) %>%
    rename_at(vars(selected_columns_file), function(x) selected_columns1) %>%
    filter(personabasicaid %in% ids) %>%
    # Unify column types.
    mutate(
      across(all_of(selected_columns1[desired_classes == 'numeric']), as.numeric),
      across(all_of(selected_columns1[desired_classes == 'character']), as.character)
    ) %>% collect
  
  df <- bind_rows(df, df0)
  
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}
# MODIFY: PROCESS AND SAVE THE REQUIRED DATA.
df %>% 
  mutate(DATE = ymd(str_sub(fecha_cobertura, 1L, 10L))) %>% 
  select(-fecha_cobertura) %>% 
  rename(sexo_PILA = sexo) %>% 
  write_parquet(PILA_history_file)
sprintf('\n\t PILA history retrieved  in %f mins\n', get_values_tic_msg('min')) %>% cat


# Preliminar panels -------------------------------------------------------
# open_dataset(RIPS_history_file) %>% 
#   left_join(
#     by = 'personabasicaid'
#   ) %>% collect %>% write_dta(
#     sprintf("%s/%s", project_folder,"data/Data_health/initial_sample_RIPS.dta"))
#   
# open_dataset(PILA_history_file) %>% 
#   left_join(
#     open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
#     by = 'personabasicaid'
#   ) %>% collect %>% write_dta(
#     sprintf("%s/%s", project_folder,"data/Data_labor/initial_sample_PILA.dta"))

# Working on RIPS --------------------------------------------------------------

rips <- open_dataset(RIPS_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  collect 

# Define a function to create the new variables based on conditions
diagnosticos <- unique(rips$DIAG_PRIN) 
df_diagnosticos <- data.frame(diagnosticos = unique(rips$DIAG_PRIN))

create_variables <- function(df, modulo)  { 
  df <- df %>% 
    mutate(
      !!paste0(modulo, "_preventivas")         := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "Z" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 0  & as.numeric(substr(DIAG_PRIN, 2,4)) <= 13, 1, 0),  
      !!paste0(modulo, "_prenatales")          := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "Z" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 30 & as.numeric(substr(DIAG_PRIN, 2,4)) <= 39, 1, 0),
      !!paste0(modulo, "_cron_cancer")         := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "C" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 0  & as.numeric(substr(DIAG_PRIN, 2,4)) <= 97, 1, 0),
      !!paste0(modulo, "_cron_diabetes")       := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "E" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 10 & as.numeric(substr(DIAG_PRIN, 2,4)) <= 14, 1, 0),      
      !!paste0(modulo, "_cron_renal")          := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "N" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 17 & as.numeric(substr(DIAG_PRIN, 2,4)) <= 18, 1, 0),
      !!paste0(modulo, "_cron_cardiovascular") := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "I" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 0  & as.numeric(substr(DIAG_PRIN, 2,4)) <= 99, 1, 0),
      !!paste0(modulo, "_cron_respiratoria")   := if_else(is.na(MODULE) & is.na(DIAG_PRIN) & MODULE == modulo & substr(DIAG_PRIN, 1,1) == "J" & as.numeric(substr(DIAG_PRIN, 2,4)) >= 40 & as.numeric(substr(DIAG_PRIN, 2,4)) <= 47, 1, 0)
    )
  return(df)
  }


# Apply the function to the datafrane fir each module
newlist_rips <- create_variables(rips, "c")
newlist_rips <- create_variables(newlist_rips, "h") 
newlist_rips <- create_variables(newlist_rips, "p") 
newlist_rips <- create_variables(newlist_rips, "u") 

# Define a variable to categorize age into group

categorize_age <- function(df) {
  mutate(
    grupo_edad = case_when(
      between(edad_RIPS , 18, 26) ~ 1,
      between(edad_RIPS , 27, 59) ~ 2,
      edad_RIPS >= 60 ~ 3      
    )
  )
  return(df)
}

# Apply the function to the dataframe
newlist_rips <- categorize_age(newlist_rips)
summary(newlist_rips$edad_RIPS)

# creating a monthly variable
newlist_rips$monthly_date <- format(newlist_rips$DATE, "%Y-%m")

# collapse the data
collapsed_rips <- newlist_rips %>% 
  group_by(YEAR, monthly_date, sexo_RIPS, grupo_edad) %>% 
  summarise(
    across(starts_with(c("c_", "h_", "p_", "u_")), sum)
  )

# collapse the data
collapsed_rips <- newlist_rips %>% 
  group_by(YEAR, monthly_date, sexo_RIPS) %>% 
  summarise(
    across(starts_with(c("c_", "h_", "p_", "u_")), sum)
  )



# Working on PILA --------------------------------------------------------------

pila <- open_dataset(PILA_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% collect 




# Doing things about RDD--------------------------------------------------------
install.packages(rddensity)
library(rddensity)

centrar_puntaje <- function(df) {
  df <- df %>% 
    mutate(
      cutuff30 = puntaje - 30.56,
      cutuff40 = puntaje - 40      
    )
  return(df)
}
rips <- centrar_puntaje(rips)
pila <- centrar_puntaje(pila)

rdd_rips30 <- rddensity(X=rips$cutuff30, p=2, vce = "jackknife")
rdd_rips40 <- rddensity(X=rips$cutuff40, p=2, vce = "jackknife")
rdd_pila30 <- rddensity(X=pila$cutuff30, p=2, vce = "jackknife")
rdd_pila40 <- rddensity(X=pila$cutuff40, p=2, vce = "jackknife")
summary(rdd_rips30)
summary(rdd_rips40)
summary(rdd_pila30)
summary(rdd_pila40)

plot1 <- rdplotdensity(rdd, rips$cutuff30, plotRange=c(-2,2), plotN=25)
plot1

score_ind_pila <- data.frame(
  personabasicaid = pila$personabasicaid,
  puntaje = pila$puntaje
) %>% 
  distinct()

score_ind_rips <- data.frame(
  personabasicaid = rips$PERSONABASICAID,
  puntaje = rips$puntaje
) %>% 
  distinct()

score_ind_rips <- centrar_puntaje(score_ind_rips)
score_ind_pila <- centrar_puntaje(score_ind_pila)

rdd_rips30 <- rddensity(X=score_ind_rips$cutuff30, p=2, vce = "jackknife")
rdd_rips40 <- rddensity(X=score_ind_rips$cutuff40, p=2, vce = "jackknife")
rdd_pila30 <- rddensity(X=score_ind_pila$cutuff30, p=2, vce = "jackknife")
rdd_pila40 <- rddensity(X=score_ind_pila$cutuff40, p=2, vce = "jackknife")
summary(rdd_rips30)
summary(rdd_rips40)
summary(rdd_pila30)
summary(rdd_pila40)


rdplot_rips30 <- rdplotdensity(rdd, score_ind_rips$cutuff30, plotN=10)
rdplot_rips30

rdplot_rips40 <- rdplotdensity(rdd, score_ind_rips$cutuff40, plotN=10)
rdplot_rips40

rdplot_pila30 <- rdplotdensity(rdd, score_ind_pila$cutuff30, plotN=10)
rdplot_pila30

rdplot_pila40 <- rdplotdensity(rdd, score_ind_pila$cutuff40, plotN=10)
rdplot_pila40

plot2 <- rdplotdensity(rdd, score_ind_rips$cutuff40, plotN=10)
plot2

hist(score_ind_pila$puntaje, breaks= 200)
abline(v= c(30.56, 40), col="red")

hist(score_ind_rips$puntaje, breaks= 200)
abline(v= c(30.56, 40), col="red")



rm(pila) # para eliminar elementos del DATA en el entorno
summary(rips)
summary(pila)


# submuestras
submuestra_rips <- rips %>% 
  sample_n(100, replace = FALSE)
submuestra_rips <- submuestra_rips %>% 
  select(-PERSONABASICAID, -documen)
  
submuestra_pila <- pila %>% 
  sample_n(100, replace = FALSE)
submuestra_pila <- submuestra_pila %>% 
  select(-personabasicaid, -documen)

setwd('//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health/data/Data_health/')
saveRDS(submuestra_rips, file='submuestra_rips')
setwd('//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health/data/Data_labor/')
saveRDS(submuestra_pila, file='submuestra_labor')
rm(submuestra_pila, submuestra_rips)


setwd('//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health/data/Data_health/')
submuestra_rips <- readRDS('submuestra_rips')
setwd('//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health/data/Data_labor/')
submuestra_pila <- readRDS('submuestra_labor')




