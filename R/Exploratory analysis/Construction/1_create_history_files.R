
# Set of necessary packages
paquetes <- c("arrow", "tidyverse", "dplyr", "readxl", "stringr", "lubridate", "purrr", "tictoc")

for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


# Set the path for the project folder
project_folder <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

# Load the dataset (Sisben 3 for Bogotá) and filter based on a specific condition (pareo == 1),
# keeping only distinct values of 'personabasicaid' and convert the result into a vector
ids <- open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% 
  distinct(personabasicaid) %>% 
  collect %>% 
  unlist %>% 
  unname

# Define the file paths for the RIPS and PILA historical data
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')

# Get RIPS history --------------------------------------------------------

# Define the folder where the original RIPS data is located
folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/_RIPS'

# Get the list of files in the folder that corresponds to each module in RIPS
files <- list.files(folder, pattern = 'proc|urg|hos|cons', recursive = TRUE)

# Load the dictionary that contains information about the columns in the RIPS files
# This dictionary is stored in an Excel file and specifies column names and data types
dict_path <- '../Data/RIPS_dictionary.xlsx'
dict <- read_excel(dict_path, sheet = 'colnames')

# Clean up the dictionary column names: Extract the last two elements from the names
names(dict) <- sapply(str_split(names(dict), '\\.'), function(x) paste(tail(x, 2), collapse = '.'))

# Select the columns of interest (based on the unified names in the dictionary)
# This is the set of variables that you I'll keep from the RIPS files
selected_columns <- c('PERSONABASICAID', 'SEXO', 'EDAD','TIPO_USUARIO', 
                      'CAUSA_EXTERNA', 'DATE_JUAN', 'DIAG_PRIN', 'DIAG_R1',
                      'COD_DIAG_R2', 'COD_DIAG_R3', 'COD_CUPS',
                      'COD_DPTO', 'COD_MPIO')

# Initialize an empty data frame to hold the final results
df <- NULL

# Start a timer for measuring execution time
tic()

# Loop through each file in the list of RIPS files
for (file in files) {
  cat(paste('Began', file))  # Print a message indicating the current file being processed
  tic()  # Start another timer for this iteration
  
  # Select the dictionary rows relevant for the current file and filter out rows with missing file names
  df_selected <- filter(dict, uniname %in% selected_columns) %>% 
    select(uniname, uniclass, file) %>% 
    drop_na(file) %>% 
    replace(is.na(.), '')
  
  # Extract the relevant columns and types for the current file
  selected_columns_file <- df_selected[[file]]    # Columns specific to this file
  selected_columns1 <- df_selected$uniname        # Standardized column names (unified names)
  desired_classes <- df_selected$uniclass         # Desired data types (numeric, character)
  
  # Extract the module type from the file name (cons, hosp, urg, proc)
  module <- str_sub(str_match(file, 'cons|hosp|urg|proc'), end = 1L)
  
  # Open the dataset and select the relevant columns, renaming them according to the unified names
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    select(all_of(selected_columns_file)) %>%       # Select the required columns
    rename_at(vars(selected_columns_file), function(x) selected_columns1) %>%  # Rename the columns
    filter(PERSONABASICAID %in% ids) %>%            # Keep only rows where PERSONABASICAID is in the list 'ids'
    mutate(MODULE = module) %>%                     # Add a new column indicating the module type
    mutate(
      across(all_of(selected_columns1[desired_classes == 'numeric']), as.numeric),  # Convert numeric columns
      across(all_of(selected_columns1[desired_classes == 'character']), as.character)  # Convert character columns
    ) %>% 
    collect  # Collect the results into a data frame
  
  # If the file corresponds to a consultation ('c') or procedure ('p') module, add a 'year_file' column
  if (module %in% c('c', 'p')) {
    df0 %>% mutate(year_file = str_extract(file, '\\d{4}') %>% as.integer)  # Extract year from file name
  }
  
  # Append the results to the main data frame
  df <- bind_rows(df, df0)
  
  # Print a message indicating the time taken for this iteration
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}

# Final processing of the combined data

# Add YEAR and DATE columns by extracting from 'DATE_JUAN' (YYYY-MM-DD format)
# Also convert diagnostic codes to uppercase
df %>% 
  mutate(
    YEAR = str_sub(DATE_JUAN, 1L, 4L) %>% as.integer(),  # Extract the year from DATE_JUAN
    DATE = ymd(str_sub(DATE_JUAN, 1L, 10L))) %>%        # Convert DATE_JUAN to a proper date format
  select(-DATE_JUAN) %>%                                 # Remove the original 'DATE_JUAN' column
  mutate(across(
    all_of(selected_columns[str_detect(selected_columns, 'DIAG')]), ~toupper(.)
  )) %>% # Convert all diagnoses in upper cases
  # Save the processed data to a parquet file
  write_parquet(RIPS_history_file)

# Print a message indicating the total time taken for the entire process
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat


# Get PILA history --------------------------------------------------------

# Start a timer to measure execution time
tic()

# Define the folder where the original PILA data is located
folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/_PILA/'

# Get a list of files that match the pattern for years 2009 through 2019
files <- list.files(folder, pattern = '2009|20[12][0-9]')

# Load the dictionary that contains metadata about the columns in the PILA files
# This dictionary is stored in an Excel file and specifies column names and data types
dict_path <- '../Data/PILA_dictionary.xlsx'
dict <- read_excel(dict_path, sheet = 'colnames')

# Clean up the dictionary column names: Extract the last two elements from the names (assuming they are split by a period)
names(dict) <- sapply(str_split(names(dict), '\\.'), function(x) paste(tail(x, 2), collapse = '.'))

# Select the desired columns from the PILA data based on the unified names in the dictionary
selected_columns <- c(
  'personabasicaid', 'fecha_cobertura', 'fechanto', 'sexo',
  'ibc_ccf', 'ibc_pens', 'ibc_rprof', 'ibc_salud', 'salario_bas',
  'tipo_ap', 'tipo_cotiz', 'tipo_per',
  'sal_dias_cot', 'tipo_cotiz', 'depto_cod', 'ciudad_cod', 
  'licen_mat', 'incap_gral', 'incap_trab', 'ciiu', 'sexomode', 'yearmode')

# Initialize an empty data frame to hold the final results
df <- NULL

# Start another timer for measuring execution time
tic()

# Loop through each file in the list of PILA files
for (file in files) {
  cat(paste('Began', file))  # Print a message indicating the current file being processed
  tic()  # Start a timer for this iteration
  
  # Select the dictionary rows relevant for the current file and filter out rows with missing file names
  df_selected <- filter(dict, uniname %in% selected_columns) %>% 
    select(uniname, uniclass, file) %>% 
    drop_na(file) %>% 
    replace(is.na(.), '')
  
  # Extract the relevant columns and types for the current file
  selected_columns_file <- df_selected[[file]]    # Columns specific to this file
  selected_columns1 <- df_selected$uniname        # Standardized column names (unified names)
  desired_classes <- df_selected$uniclass         # Desired data types (numeric, character)
  
  # Open the dataset and select the relevant columns, renaming them according to the unified names
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    select(all_of(selected_columns_file)) %>%       # Select the required columns
    rename_at(vars(selected_columns_file), function(x) selected_columns1) %>%  # Rename the columns
    filter(personabasicaid %in% ids) %>%            # Filter rows based on the 'personabasicaid'
    mutate(                                         # Unify column types by converting variables
      across(all_of(selected_columns1[desired_classes == 'numeric']), as.numeric),  # Convert numeric columns
      across(all_of(selected_columns1[desired_classes == 'character']), as.character)  # Convert character columns
    ) %>% 
    collect  # Collect the results into a data frame
  
  # Append the results to the main data frame
  df <- bind_rows(df, df0)
  
  # Print a message indicating the time taken for this iteration
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}

# Final processing of the combined data
df %>% 
  # Add a DATE column by converting the 'fecha_cobertura' (YYYY-MM-DD format)
  mutate(DATE = ymd(str_sub(fecha_cobertura, 1L, 10L))) %>%  
  select(-fecha_cobertura) %>%                        # Remove the original 'fecha_cobertura' column
  write_parquet(PILA_history_file)                    # Save the processed data as a Parquet file

# Print a message indicating the total time taken for the entire process
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat


# Expand basic SISBEN -----------------------------------------------------

# Reload the IDs (now using 'documen' instead of 'personabasicaid')
ids <- open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% 
  distinct(documen) %>% 
  collect %>% unlist %>% unname

# Select the relevant columns from the SISBEN III data
select_columns <- c('documen', 'estrato', 'thogar', 'ingresos', 'nivel', 'sexo')

# Load the SISBEN III dataset and filter it based on certain conditions:
# - 'tipodoc' == 1 (likely a filter for a specific document type)
# - 'documen' must be in the list of IDs
# - 'estrato' should not be zero
df_aux <- open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/SISBEN/SISBEN III/Sisben III_Nacional.parquet') %>%
  filter(tipodoc == 1, documen %in% ids, estrato != 0) %>%
  select(all_of(select_columns)) %>% 
  distinct(documen, estrato, thogar, nivel, sexo)

# Further processing of the SISBEN data: 
# Ensure only individuals with a single observation ('n_ == 1') are retained
df_aux <- df_aux %>% 
  group_by(documen) %>% summarise(n_ = n()) %>% filter(n_ == 1) %>% select(-n_) %>% 
  left_join(df_aux, by = 'documen') %>% 
  # Join with another version of the dataset where we summarize the 'ingresos' column (keeping the maximum value)
  left_join(
    open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/SISBEN/SISBEN III/Sisben III_Nacional.parquet') %>%
      filter(tipodoc == 1, documen %in% ids, estrato != 0) %>% 
      select(all_of(select_columns)) %>% 
      group_by(documen) %>% 
      summarise(ingresos = max(ingresos, na.rm = TRUE)), 
    by = 'documen'
  )

# Merge the SISBEN data with the original dataset and save the results as a Parquet file
open_dataset(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet")) %>% 
  filter(pareo == 1) %>% 
  left_join(df_aux, by = 'documen') %>% 
  write_parquet(sprintf("%s/%s", project_folder, "data/master.parquet"))


# Preliminar panels -------------------------------------------------------

# Merge RIPS data with the master dataset, saving the final result as a .dta file (for Stata)
open_dataset(RIPS_history_file) %>% 
  rename(sexo_RIPS = SEXO, edad_RIPS = EDAD) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = join_by(PERSONABASICAID == personabasicaid)
  ) %>% 
  collect %>% 
  write_dta(sprintf("%s/%s", project_folder,"data/Data_health/initial_sample_RIPS.dta"))

# Merge PILA data with the master dataset, saving the final result as a .dta file (for Stata)
open_dataset(PILA_history_file) %>% 
  rename(sexo_PILA = sexo) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  collect %>% 
  write_dta(sprintf("%s/%s", project_folder,"data/Data_labor/initial_sample_PILA.dta"))



