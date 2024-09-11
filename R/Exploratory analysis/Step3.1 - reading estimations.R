paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

# Directory and creating a new folder to save all the results
base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
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
























