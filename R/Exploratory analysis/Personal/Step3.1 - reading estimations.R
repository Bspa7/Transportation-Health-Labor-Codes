paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

# Directory and creating a new folder to save all the results
base_dir  <- "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")

# Results from Jun 06, 2024

rips_est30 <- sprintf('%s/%s', output_folder, 'parquet/20240607-est_rips30.parquet')
pila_est30 <- sprintf('%s/%s', output_folder, 'parquet/20240607-est_pila30.parquet')
rips_est40 <- sprintf('%s/%s', output_folder, 'parquet/20240607-est_rips40.parquet')
pila_est40 <- sprintf('%s/%s', output_folder, 'parquet/20240607-est_pila40.parquet')

rips_est30 <- open_dataset(rips_est30) %>%  collect
pila_est30 <- open_dataset(pila_est30) %>%  collect
rips_est40 <- open_dataset(rips_est40) %>%  collect
pila_est40 <- open_dataset(pila_est40) %>%  collect

write.xlsx(rips_est30, file = file.path(output_folder, "20240612-rips_est30.xlsx"))
write.xlsx(pila_est30, file = file.path(output_folder, "20240612-pila_est30.xlsx"))
write.xlsx(rips_est40, file = file.path(output_folder, "20240612-rips_est40.xlsx"))
write.xlsx(pila_est40, file = file.path(output_folder, "20240612-pila_est40.xlsx"))


# Results from Jun 27, 2024 (Estimations using balanced panel)

rips_est30 <- sprintf('%s/%s', output_folder, 'parquet/20240627-est_rips30.parquet')
pila_est30 <- sprintf('%s/%s', output_folder, 'parquet/20240627-est_pila30.parquet')
rips_est40 <- sprintf('%s/%s', output_folder, 'parquet/20240627-est_rips40.parquet')
pila_est40 <- sprintf('%s/%s', output_folder, 'parquet/20240627-est_pila40.parquet')

rips_est30 <- open_dataset(rips_est30) %>%  collect
pila_est30 <- open_dataset(pila_est30) %>%  collect
rips_est40 <- open_dataset(rips_est40) %>%  collect
pila_est40 <- open_dataset(pila_est40) %>%  collect

write.xlsx(rips_est30, file = file.path(output_folder, "20240627-rips_est30.xlsx"))
write.xlsx(pila_est30, file = file.path(output_folder, "20240627-pila_est30.xlsx"))
write.xlsx(rips_est40, file = file.path(output_folder, "20240627-rips_est40.xlsx"))
write.xlsx(pila_est40, file = file.path(output_folder, "20240627-pila_est40.xlsx"))

# Results from Jul 16, 2024 (Estimations using balanced panel and main-base)

est30 <- sprintf('%s/%s', output_folder, 'parquet/20240715-table_est30.parquet')
est40 <- sprintf('%s/%s', output_folder, 'parquet/20240715-table_est40.parquet')


est30 <- open_dataset(est30) %>%  collect
est40 <- open_dataset(est40) %>%  collect


write.xlsx(est30, file = file.path(output_folder, "20240715-est30.xlsx"))
write.xlsx(est40, file = file.path(output_folder, "20240715-est40.xlsx"))


# Reading parquet  with control mean
control_mean <- sprintf('%s/%s', output_folder, 'parquet/20240723-control_mean.parquet')
control_mean <- open_dataset(control_mean) %>%  collect

control_mean$mean_control <- round(control_mean$mean_control, 3)
write.xlsx(control_mean, file = file.path(output_folder, "20240723-control_mean.xlsx"))

# Results from Aug 02, 2024 (Estimations using different groups) 
results <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240801-table_est_v2.parquet')) %>% collect()
results$control_mean <- round(results$control_mean, 3)
write.xlsx(results, file = file.path(output_folder, "20240801-table_est_v2.xlsx"))


# Reading results from 14-aug-2024, results with global descriptives
results <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240814-gnral_stats.parquet')) %>% collect()
write.xlsx(results, file = file.path(output_folder, "20240814-gnral_stats.xlsx"))

# Reading results from 26-aug-2024, results with basic descriptives
results <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240826-gnral_counts.parquet')) %>% collect()
write.xlsx(results, file = file.path(output_folder, "20240826-gnral_counts.xlsx"))
results <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240826-gnral_stats.parquet')) %>% collect()
write.xlsx(results, file = file.path(output_folder, "20240826-gnral_stats.xlsx"))

# Reading results from 05-sep-2024, results with descriptives

# Loop para iterar sobre los números del 1 al 6
for (i in 1:6) {
  # Construir el nombre del archivo .parquet
  parquet_file <- sprintf("20240905-stats_set1_step%d.parquet", i)
  
  # Abrir el archivo .parquet y recolectar los datos
  results <- open_dataset(sprintf('%s/%s', output_folder, paste0('parquet/', parquet_file))) %>% collect()
  
  # Guardar los datos en formato .xlsx
  write.xlsx(results, file = file.path(output_folder, paste0(parquet_file, ".xlsx")))
}


# Reading results from 18-sep-2024, results with descriptives 

# Nombres de los archivos parquet y las hojas que se crearán en el Excel
file_names <- c(
  "20240918-stats_set2_income_y.parquet",
  "20240918-stats_set2_income_m.parquet",
  "20240918-stats_set2_cont_y.parquet",
  "20240918-stats_set2_cont_m.parquet",
  "20240918-stats_set2_count_y.parquet",
  "20240918-stats_set2_count_m.parquet",
  "20240918-stats_set2_unique_m.parquet"
)

# Extraer nombres para las hojas del Excel
sheet_names <- sub(".*set2_(.*)\\.parquet", "\\1", file_names)

# Crear un nuevo archivo Excel
excel_file <- createWorkbook()

# Leer cada archivo .parquet y agregarlo como una hoja al Excel
for (i in seq_along(file_names)) {
  file_path <- sprintf('%s/parquet/%s', output_folder, file_names[i])
  results <- open_dataset(file_path) %>% collect()
  
  # Agregar hoja al archivo Excel
  addWorksheet(excel_file, sheetName = sheet_names[i])
  writeData(excel_file, sheet = sheet_names[i], x = results)
}

# Guardar el archivo Excel
saveWorkbook(excel_file, file = file.path(output_folder, "20240918-all_results.xlsx"), overwrite = TRUE)



# Reading results from 20-sep-2024, results with descriptives

count_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240919-stats_set3_step1.parquet')) %>% collect
count_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240919-stats_set3_step2.parquet')) %>% collect
count_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240919-stats_set3_step3.parquet')) %>% collect
count_step4 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240923-stats_set4_step1.parquet')) %>% collect

# Crear un nuevo workbook
wb <- createWorkbook()

# Añadir hojas al workbook con los dataframes correspondientes
addWorksheet(wb, "Step 1")
writeData(wb, "Step 1", count_step1)

addWorksheet(wb, "Step 2")
writeData(wb, "Step 2", count_step2)

addWorksheet(wb, "Step 3")
writeData(wb, "Step 3", count_step3)

addWorksheet(wb, "Step 4")
writeData(wb, "Step 4", count_step4)

saveWorkbook(wb, file = sprintf('%s/20240923 - general_counters.xlsx', output_folder), overwrite = TRUE)


# Reading results from 24-sep-2024, general info about diagnoses

diag_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240924-diagnoses_CIEV10.parquet')) %>% collect
diag_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240924-diagnoses_cau_ext.parquet')) %>% collect
diag_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240924-diagnoses_CUPS.parquet')) %>% collect

# Crear un nuevo workbook
wb <- createWorkbook()

# Añadir hojas al workbook con los dataframes correspondientes
addWorksheet(wb, "Diag_CIEV10")
writeData(wb, "Diag_CIEV10", diag_step1)

addWorksheet(wb, "Diag_cauext")
writeData(wb, "Diag_cauext", diag_step2)

addWorksheet(wb, "Diag_cups")
writeData(wb, "Diag_cups", diag_step3)


saveWorkbook(wb, file = sprintf('%s/20240924 - general_diagnoses.xlsx', output_folder), overwrite = TRUE)



# Reading results from 30-sep-2024, results with descriptives

count_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step1.parquet')) %>% collect
count_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step2.parquet')) %>% collect
count_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step3.parquet')) %>% collect
count_step4 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step4.parquet')) %>% collect
count_step5 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step5.parquet')) %>% collect
count_step6 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step6.parquet')) %>% collect
count_step7 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20240930-stats_set5_step7.parquet')) %>% collect

# Crear un nuevo workbook
wb <- createWorkbook()

# Añadir hojas al workbook con los dataframes correspondientes
addWorksheet(wb, "Step 1")
writeData(wb, "Step 1", count_step1)

addWorksheet(wb, "Step 2")
writeData(wb, "Step 2", count_step2)

addWorksheet(wb, "Step 3")
writeData(wb, "Step 3", count_step3)

addWorksheet(wb, "Step 4")
writeData(wb, "Step 4", count_step4)

addWorksheet(wb, "Step 5")
writeData(wb, "Step 5", count_step5)

addWorksheet(wb, "Step 6")
writeData(wb, "Step 6", count_step6)

addWorksheet(wb, "Step 7")
writeData(wb, "Step 7", count_step7)

saveWorkbook(wb, file = sprintf('%s/20240930 - general_counters.xlsx', output_folder), overwrite = TRUE)


# Nombres de los archivos parquet y las hojas que se crearán en el Excel
file_names <- c(
  "20240930-stats_set5_income_y.parquet",
  "20240930-stats_set5_income_m.parquet",
  "20240930-stats_set5_cont_y.parquet",
  "20240930-stats_set5_cont_m.parquet",
  "20240930-stats_set5_count_y.parquet",
  "20240930-stats_set5_count_m.parquet",
  "20240930-stats_set5_unique_m.parquet"
)

# Extraer nombres para las hojas del Excel
sheet_names <- sub(".*set5_(.*)\\.parquet", "\\1", file_names)

# Crear un nuevo archivo Excel
excel_file <- createWorkbook()

# Leer cada archivo .parquet y agregarlo como una hoja al Excel
for (i in seq_along(file_names)) {
  file_path <- sprintf('%s/parquet/%s', output_folder, file_names[i])
  results <- open_dataset(file_path) %>% collect()
  
  # Agregar hoja al archivo Excel
  addWorksheet(excel_file, sheetName = sheet_names[i])
  writeData(excel_file, sheet = sheet_names[i], x = results)
}

# Guardar el archivo Excel
saveWorkbook(excel_file, file = file.path(output_folder, "20240930-all_results.xlsx"), overwrite = TRUE)



# Reading results from 18-Oct-2024, general info about diagnoses in the four modules in RIPS

diag_c_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_c_CIEV10.parquet')) %>% collect
diag_c_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_c_cau_ext.parquet')) %>% collect
diag_c_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_c_CUPS.parquet')) %>% collect

diag_p_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_p_CIEV10.parquet')) %>% collect
diag_p_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_p_cau_ext.parquet')) %>% collect
diag_p_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_p_CUPS.parquet')) %>% collect

diag_h_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_h_CIEV10.parquet')) %>% collect
diag_h_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_h_cau_ext.parquet')) %>% collect
diag_h_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_h_CUPS.parquet')) %>% collect

diag_u_step1 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_u_CIEV10.parquet')) %>% collect
diag_u_step2 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_u_cau_ext.parquet')) %>% collect
diag_u_step3 <- open_dataset(sprintf('%s/%s', output_folder, 'parquet/20241018-diagnoses_u_CUPS.parquet')) %>% collect

# Crear un nuevo workbook
wb <- createWorkbook()

# Consultations sheets in the excel file
addWorksheet(wb, "consultations_Diag_CIEV10")
writeData(wb, "consultations_Diag_CIEV10", diag_c_step1)

addWorksheet(wb, "consultations_Diag_cauext")
writeData(wb, "consultations_Diag_cauext", diag_c_step2)

addWorksheet(wb, "consultations_Diag_cups")
writeData(wb, "consultations_Diag_cups", diag_c_step3)

# Procedures sheets in the excel file
addWorksheet(wb, "procedures_Diag_CIEV10")
writeData(wb, "procedures_Diag_CIEV10", diag_p_step1)

addWorksheet(wb, "procedures_Diag_cauext")
writeData(wb, "procedures_Diag_cauext", diag_p_step2)

addWorksheet(wb, "procedures_Diag_cups")
writeData(wb, "procedures_Diag_cups", diag_p_step3)

# Hospitalization sheets in the excel file
addWorksheet(wb, "hospit_Diag_CIEV10")
writeData(wb, "hospit_Diag_CIEV10", diag_h_step1)

addWorksheet(wb, "hospit_Diag_cauext")
writeData(wb, "hospit_Diag_cauext", diag_h_step2)

addWorksheet(wb, "hospit_Diag_cups")
writeData(wb, "hospit_Diag_cups", diag_h_step3)

# Emergencies sheets in the excel file
addWorksheet(wb, "emergency_Diag_CIEV10")
writeData(wb, "emergency_Diag_CIEV10", diag_u_step1)

addWorksheet(wb, "emergency_Diag_cauext")
writeData(wb, "emergency_Diag_cauext", diag_u_step2)

addWorksheet(wb, "emergency_Diag_cups")
writeData(wb, "emergency_Diag_cups", diag_u_step3)


saveWorkbook(wb, file = sprintf('%s/20241018 - general_diagnoses.xlsx', output_folder), overwrite = TRUE)






