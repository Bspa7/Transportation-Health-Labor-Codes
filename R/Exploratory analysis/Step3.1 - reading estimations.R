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




