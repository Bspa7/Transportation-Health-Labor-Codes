# HISTORIAL PILA y RIPS. SELECTED_COLUMNS (TIPOS DE DATOS) Y ARCHIVOS.
# Limpieza de la muestra ids_finales. REGLAS DE LIMPIEZA.
# colapsa PILA y RIPS en la periodicidad deseada. CREATED_COLUMNS.POR CADA VARIABLE TENER UNA REGLA DE DECISION Y PERIODICIDAD.
# Crea la master. CUALES VARIABLES
# Crea un panel pegando todas las bases. CUALES VARIABLES.
project_folder <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"
ids <- open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% distinct(personabasicaid) %>% 
  collect %>% unlist %>% unname
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
  write_parquet(RIPS_history_file)
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat


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
  write_parquet(PILA_history_file)
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat


# Expand basic SISBEN -----------------------------------------------------
ids <- open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% distinct(documen) %>% 
  collect %>% unlist %>% unname

select_columns <- c('documen', 'estrato', 'thogar', 'ingresos', 'nivel', 'sexo')


df_aux <- open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/SISBEN/SISBEN III/Sisben III_Nacional.parquet') %>%
  filter(tipodoc == 1, documen %in% ids, estrato != 0) %>% select(all_of(select_columns)) %>% 
  distinct(documen, estrato, thogar, nivel, sexo)

df_aux <- df_aux %>% 
  group_by(documen) %>% summarise(n_ = n()) %>% filter(n_ == 1) %>% select(-n_) %>% 
  left_join(df_aux, by = 'documen') %>% 
  left_join(
    open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/SISBEN/SISBEN III/Sisben III_Nacional.parquet') %>%
      filter(tipodoc == 1, documen %in% ids, estrato != 0) %>% select(all_of(select_columns)) %>% 
      group_by(documen) %>% 
      summarise(ingresos = max(ingresos, na.rm = T)), by = 'documen'
  )

open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% 
  left_join(df_aux, by = 'documen') %>% 
  write_parquet(sprintf("%s/%s", project_folder, "data/master.parquet"))


# Preliminar panels -------------------------------------------------------
open_dataset(RIPS_history_file) %>% 
  rename(sexo_RIPS = SEXO, edad_RIPS = EDAD) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = join_by(PERSONABASICAID == personabasicaid)
  ) %>% collect %>% write_dta(
    sprintf("%s/%s", project_folder,"data/Data_health/initial_sample_RIPS.dta"))
  
open_dataset(PILA_history_file) %>% 
  rename(sexo_PILA = sexo) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% collect %>% write_dta(
    sprintf("%s/%s", project_folder,"data/Data_labor/initial_sample_PILA.dta"))
