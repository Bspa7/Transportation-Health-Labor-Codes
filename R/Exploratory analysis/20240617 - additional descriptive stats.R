
paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

base_dir <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")

# ----

# 1. Definitions and functions -----------------------------------------------------

# Define a function to create the running variables centered in zero
centrar_puntaje <- function(df) {
  df <- df %>% 
    mutate(
      cutoff30 = puntaje - 30.56,
      cutoff40 = puntaje - 40      
    )
  return(df)
}


# Define a function to calculate some basic metrics for global descriptives
calcular_metricas <- function(df, nombre_df, id_col) {
  ind      <- length(unique(df[[id_col]]))
  ind_30   <- length(unique(df[[id_col]][df$puntaje < 30.56]))
  ind_3040 <- length(unique(df[[id_col]][df$puntaje >= 30.56 & df$puntaje <= 40]))
  ind_40   <- length(unique(df[[id_col]][df$puntaje > 40]))
  ind_2530 <- length(unique(df[[id_col]][df$puntaje >= 25.56 & df$puntaje <= 30.56]))
  ind_3035 <- length(unique(df[[id_col]][df$puntaje >= 30.56 & df$puntaje <= 35.56]))
  ind_3540 <- length(unique(df[[id_col]][df$puntaje >= 35 & df$puntaje <= 40]))
  ind_4045 <- length(unique(df[[id_col]][df$puntaje >= 40 & df$puntaje <= 45]))
  ind_2050 <- length(unique(df[[id_col]][df$puntaje >= 20 & df$puntaje <= 50]))
  
  return(data.frame(
    data = nombre_df,
    ind = ind,
    ind_30 = ind_30,
    ind_3040 = ind_3040,
    ind_40 = ind_40,
    ind_2530 = ind_2530,
    ind_3035 = ind_3035,
    ind_3540 = ind_3540,
    ind_4045 = ind_4045,
    ind_2050 = ind_2050
  ))
}

# Define a function to make and edit the density plots
crear_grafico <- function(estimation, cutoff, xlabel) {
  plot <- rdplotdensity(estimation, cutoff,
                        lcol = c("red", "blue"), histFillCol = "white", histLineCol="gray60",
                        title="", 
                        xlabel = xlabel,
                        ylabel = "Density",
                        plotN = 25,
                        hist=TRUE)
  grafico <- plot$Estplot
  grafico <- grafico + theme_classic() +
    labs(
      title = "",
      subtitle = "",
      caption = "",
      x = xlabel,
      y = "Density"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme(
      legend.position = "none"
    )
  return(grafico)
}


# Función para extraer los valores de una estimación
extraer_valores <- function(estimation, base_name) {
  data.frame(
    Base = base_name,
    N_full = estimation$N$full,
    N_left = estimation$N$left,
    N_right = estimation$N$right,
    N_eff_left = estimation$N$eff_left,
    N_eff_right = estimation$N$eff_right,
    p_value = estimation$test$p_jk,
    h_left = estimation$h$left,
    h_right = estimation$h$right
  )
}


# ----

# 2. Initial databases ------------------------------------------------------------

# Databases - SISBEN and MASTER
all_sisben_bgta <- open_dataset(sprintf('%s/%s', project_folder, 'data/sisben3_bgta_demog.parquet'))
master          <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))

# Databases - RIPS
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')
all_rips          <- open_dataset(RIPS_history_file)
main_rips         <- open_dataset(RIPS_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) 

# Databases - PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
all_pila          <- open_dataset(PILA_history_file) 
main_pila         <- open_dataset(PILA_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) 
# ----


# 3. Modified databases to estimations -----------------------------------------

# SISBEN all Bgta
sisben_bgta <- all_sisben_bgta %>%
  group_by(documen) %>%
  summarise(puntaje  = max(puntaje, na.rm = TRUE))

# SISBEN with personab_id (master)
sisben_pbid <- master %>%
  select(personabasicaid, documen, puntaje) %>%
  distinct() 

# RIPS
main_rips <- main_rips %>% 
  select(personabasicaid, puntaje) %>% 
  distinct()

# PILA
main_pila <- main_pila %>% 
  select(personabasicaid, puntaje) %>% 
  distinct()

sisben_pbid <- centrar_puntaje(sisben_pbid)
sisben_bgta <- centrar_puntaje(sisben_bgta)
main_rips   <- centrar_puntaje(main_rips)
main_pila   <- centrar_puntaje(main_pila)


main_pila <- main_pila %>% 
  collect

main_rips <- main_rips %>% 
  collect

sisben_bgta <- sisben_bgta %>% 
  collect

sisben_pbid <- sisben_pbid %>% 
  collect

# ----

# Some additional stats --------------------------------------------------------

# Metrics of each datadrame
res_sisben_bgta <- calcular_metricas(sisben_bgta, "sisben_bgta", "documen")
res_sisben_pbid <- calcular_metricas(sisben_pbid, "sisben_pbid", "personabasicaid")
res_main_pila   <- calcular_metricas(main_pila,   "main_pila", "personabasicaid")
res_main_rips   <- calcular_metricas(main_rips,   "main_rips", "personabasicaid")


# Creating a small dataframe with all information 
resumen <- rbind(res_sisben_bgta, res_sisben_pbid, res_main_pila, res_main_rips)

# Saving descriptive results
write.xlsx(resumen, file.path(output_folder, "tables/20240617-stats.xlsx"))

# ----

# Manipulation density test-----------------------------------------------------

# Estimations
sisben_bgta30 <- rddensity(X=sisben_bgta$cutoff30)
sisben_bgta40 <- rddensity(X=sisben_bgta$cutoff40)
sisben_pbid30 <- rddensity(X=sisben_pbid$cutoff30)
sisben_pbid40 <- rddensity(X=sisben_pbid$cutoff40)
main_rips30 <- rddensity(X=main_rips$cutoff30)
main_rips40 <- rddensity(X=main_rips$cutoff40)
main_pila30 <- rddensity(X=main_pila$cutoff30)
main_pila40 <- rddensity(X=main_pila$cutoff40)

# Generate plots
grafico_sisben_bgta30 <- crear_grafico(sisben_bgta30, sisben_bgta$cutoff30, "Distance to SISBEN cutoff 30pts (centered around 0)")
grafico_sisben_bgta40 <- crear_grafico(sisben_bgta40, sisben_bgta$cutoff40, "Distance to SISBEN cutoff 40pts (centered around 0)")

grafico_sisben_pbid30 <- crear_grafico(sisben_pbid30, sisben_pbid$cutoff30, "Distance to SISBEN cutoff 30pts (centered around 0)")
grafico_sisben_pbid40 <- crear_grafico(sisben_pbid40, sisben_pbid$cutoff40, "Distance to SISBEN cutoff 40pts (centered around 0)")

grafico_main_rips30 <- crear_grafico(main_rips30, main_rips$cutoff30, "Distance to SISBEN cutoff 30pts (centered around 0)")
grafico_main_rips40 <- crear_grafico(main_rips40, main_rips$cutoff40, "Distance to SISBEN cutoff 40pts (centered around 0)")

grafico_main_pila30 <- crear_grafico(main_pila30, main_pila$cutoff30, "Distance to SISBEN cutoff 30pts (centered around 0)")
grafico_main_pila40 <- crear_grafico(main_pila40, main_pila$cutoff40, "Distance to SISBEN cutoff 40pts (centered around 0)")


# Saving results
ggsave(file.path(output_folder, "figures/20240617-plot_sisben_bgta30.png"), plot = grafico_sisben_bgta30, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_folder, "figures/20240617-plot_sisben_bgta40.png"), plot = grafico_sisben_bgta40, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_folder, "figures/20240617-plot_sisben_pbid30.png"), plot = grafico_sisben_pbid30, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_folder, "figures/20240617-plot_sisben_pbid40.png"), plot = grafico_sisben_pbid40, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_folder, "figures/20240617-plot_main_rips30.png"), plot = grafico_main_rips30, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_folder, "figures/20240617-plot_main_rips40.png"), plot = grafico_main_rips40, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_folder, "figures/20240617-plot_main_pila30.png"), plot = grafico_main_pila30, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_folder, "figures/20240617-plot_main_pila40.png"), plot = grafico_main_pila40, width = 8, height = 6, dpi = 300)

# ----

# Extraer valores para cada estimación
est_results <- rbind(
  extraer_valores(sisben_bgta30, "sisben_bgta30"),
  extraer_valores(sisben_bgta40, "sisben_bgta40"),
  extraer_valores(sisben_pbid30, "sisben_pbid30"),
  extraer_valores(sisben_pbid40, "sisben_pbid40"),
  extraer_valores(main_rips30, "main_rips30"),
  extraer_valores(main_rips40, "main_rips40"),
  extraer_valores(main_pila30, "main_pila30"),
  extraer_valores(main_pila40, "main_pila40")
)

# Mostrar el dataframe resultante
print(est_results)

# Saving descriptive results
write.xlsx(est_results, file.path(output_folder, "tables/20240617-rddensity_results.xlsx"))














