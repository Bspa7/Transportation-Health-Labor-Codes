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


scores <- c(30, 40)
bases <- c("pila", "rips", "sbgta", "spbid")

# Iteration over databases and scores
for (base in bases) {
  for (score in scores) {
    # Assing a name to the file
    file_path <- sprintf('%s/parquet/rddensity_%s%s.parquet', output_folder, base, score)
    
    # Open file and collect info
    rddensity <- open_dataset(file_path) %>% collect()
    
    assign(paste0("rddensity_", base, score), rddensity)
    
    # Saving results on excel
    excel_file <- sprintf('%s/%s_%s.xlsx', output_folder, 'rddensity', paste0(base, score))
    write.xlsx(rddensity, excel_file, row.names = FALSE)
        
  }
}





