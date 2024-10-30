# Date of creation: 23-Oct-2024
# Created by: Brayan Pineda
# Last modification: 24-Oct-2024 by Brayan Pineda
# Objective: This code creates the initial datasets for PILA and RIPS (history files)
# Using selected columns and the Christian's files, this script clean the smaple of
# final ID's, establish some cleaning rules, AND colapse each dataset to obtain a panel.

# Libraries
library("arrow"); library("tidyverse"); library("haven"); library("dplyr"); library("readxl"); library("tictoc")

# Set the project folder path
project_folder  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"

# Load the initial dataset to filter unique 'personabasicaid' IDs based on the 'pareo' condition
ids <- open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% # Filter for matching records
  distinct(personabasicaid) %>% # Keep only distinct IDs
  collect %>% unlist %>% unname  # Convert to a simple vector for later use

# Define the folder containing RIPS files and the pattern to match specific datasets (proc, urg, hosp, cons)
folder <- '//wmedesrv/GAMMA/Christian Posso/_banrep_research/datos_originales/_RIPS'
files <- list.files(folder, pattern = 'proc|urg|hos|cons', recursive = T)

# Load the dictionary that maps column names for each file
dict_path <- '//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health/Data/RIPS_dictionary.xlsx'
dict <- read_excel(dict_path, sheet = 'colnames')

# Ensure consistent column naming by cleaning up the dictionary's column names
names(dict) <- sapply(str_split(names(dict), '\\.'), function(x) paste(tail(x, 2), collapse = '.'))

# Specify the columns of interest that will be selected and processed from each dataset
selected_columns <- c('PERSONABASICAID', 'SEXO', 'EDAD', 'TIPO_USUARIO', 
                      'CAUSA_EXTERNA', 'DATE_JUAN', 'DIAG_PRIN', 'DIAG_R1',
                      'COD_DIAG_R2', 'COD_DIAG_R3', 'COD_CUPS',
                      'COD_DPTO', 'COD_MPIO')

# Initialize an empty dataframe to store the final result
df <- NULL

# Iterate over each file that matches the specified patterns ('proc', 'urg', 'hos', 'cons')
for (file in files) {
  cat(paste('Began', file))  # Display the file being processed
  
  # Filter the dictionary for the selected columns and the current file, and handle missing values
  df_selected <- filter(dict, uniname %in% selected_columns) %>% 
    select(uniname, uniclass, file) %>% drop_na(file) %>% 
    replace(is.na(.), '')
  
  # Extract the file-specific columns and their corresponding names and types
  selected_columns_file <- df_selected[[file]]
  selected_columns1 <- df_selected$uniname
  desired_classes <- df_selected$uniclass
  
  # Extract the dataset type (module) from the file name (e.g., consultation, procedure)
  module <- str_sub(str_match(file, 'cons|hosp|urg|proc'), end = 1L)
  
  # Open the dataset and select the relevant columns, renaming them to unified names
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    select(all_of(selected_columns_file)) %>%
    rename_at(vars(selected_columns_file), function(x) selected_columns1) %>%
    filter(PERSONABASICAID %in% ids) %>%  # Keep only relevant IDs
    mutate(MODULE = module) %>% collect
  
  # Ensure consistent data types: 
  # Convert 'CAUSA_EXTERNA' to numeric (handling empty strings as NA) and unify numeric/character columns
  df0 <- df0 %>% 
    mutate(
      CAUSA_EXTERNA = if("CAUSA_EXTERNA" %in% names(df0)) {
        as.numeric(na_if(as.character(CAUSA_EXTERNA), ""))
      } else {
        NA_real_  # Assign NA if the variable is missing
      },
      across(all_of(selected_columns1[desired_classes == 'numeric']), as.numeric),
      across(all_of(selected_columns1[desired_classes == 'character']), as.character)
    )
  
  # Add 'year_file' variable for certain modules (if applicable)
  if (module %in% c('c', 'p')) {
    df0 <- df0 %>% mutate(year_file = str_extract(file, '\\d{4}') %>% as.integer)
  }
  
  # Append the processed data to the main dataframe
  df <- bind_rows(df, df0)
}

# Final processing: Create 'YEAR' and 'DATE' variables from 'DATE_JUAN', and clean up the dataset
df <- df %>% 
  mutate(
    YEAR = str_sub(DATE_JUAN, 1L, 4L) %>% as.integer(),  # Extract the year from 'DATE_JUAN'
    DATE = ymd(str_sub(DATE_JUAN, 1L, 10L))) %>%         # Extract and convert the date
  select(-DATE_JUAN) %>%                                 # Remove 'DATE_JUAN' after processing
  mutate(across(
    all_of(selected_columns[str_detect(selected_columns, 'DIAG')]), ~toupper(.))) %>%  # Capitalize diagnosis codes
  rename(sexo_RIPS = SEXO, edad_RIPS = EDAD, personabasicaid = PERSONABASICAID)  # Rename some variables for clarity

# Save the final dataset as a Parquet file
write_parquet(df, file.path(project_folder, "data/Data_health/history_RIPS_complete_controls.parquet"))


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

