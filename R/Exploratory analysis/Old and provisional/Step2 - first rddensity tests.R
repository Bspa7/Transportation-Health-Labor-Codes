paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


#base_dir <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")

# ----

# 1. Definitions and functions -----------------------------------------------------


# Define a variable to categorize age into group - RIPS
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

# Define a variable to categorize age into group - PILA
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

# Define a function to create the running variables centered in zero
centrar_puntaje <- function(df) {
  df <- df %>% 
    mutate(
      cutoff30 = puntaje - 30.56,
      cutoff40 = puntaje - 40      
    )
  return(df)
}


# Create a function to extract coefficients and values of rddensity tests
extraer_valores_jk <- function(rdd_object) {
  # Extraer los valores de N
  N_full      <- rdd_object$N$full
  N_left      <- rdd_object$N$left
  N_right     <- rdd_object$N$right
  N_eff_left  <- rdd_object$N$eff_left
  N_eff_right <- rdd_object$N$eff_right
  
  # Extraer los valores del modelo
  model_fitselect <- rdd_object$opt$fitselect
  model_kernel    <- rdd_object$opt$kernel
  model_bwselect  <- rdd_object$opt$bwselect
  model_vce       <- rdd_object$opt$vce
  model_polyn     <- rdd_object$opt$p
  
  # Extraer los errores estándar
  error_st_left  <- rdd_object$sd_jk$left 
  error_st_right <- rdd_object$sd_jk$right 
  error_st_diff  <- rdd_object$sd_jk$diff
  
  # Extraer el p-value y los bandwidht
  p_value <- rdd_object$test$p_jk
  h_left  <- rdd_object$h$left
  h_right <- rdd_object$h$right  
  
  # Crear un data frame con los valores extraídos
  resultado <- data.frame(model_fitselect, model_kernel, model_bwselect, model_vce,
                          model_polyn, N_full, N_left, N_right, N_eff_left, N_eff_right,
                          h_left, h_right, error_st_left, error_st_right, error_st_diff, p_value )
  
  # Devolver el data frame con los valores
  return(resultado)
}

p_values <- 1:4
fitselect_values <- c("unrestricted", "restricted")
kernel_values <- c("triangular", "uniform")
bwselect_values <- c("each", "diff", "sum", "comb")

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
  ) %>%
  rename(COD_MPIO = COD_MPIO.x, COD_DPTO = COD_DPTO.x, puntaje = puntaje.x)

# Databases - PILA
PILA_history_file <- sprintf('%s/%s', project_folder, 'data/Data_labor/history_PILA.parquet')
all_pila          <- open_dataset(PILA_history_file) 
main_pila         <- open_dataset(PILA_history_file) %>% 
  left_join(
    open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet")),
    by = 'personabasicaid'
  ) %>% 
  rename(puntaje = puntaje.x)

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

sisben_bgta
sisben_pbid
main_pila
main_rips


# ----

main_pila <- main_pila %>% 
  collect

main_rips <- main_rips %>% 
  collect

sisben_bgta <- sisben_bgta %>% 
  collect

sisben_pbid <- sisben_pbid %>% 
  collect


main_pila <- main_pila %>% 
  sample_frac(0.15)

main_rips <- main_rips %>% 
  sample_frac(0.15) 

sisben_bgta <- sisben_bgta %>% 
  sample_frac(0.35) 

sisben_pbid <- sisben_pbid %>% 
  sample_frac(0.35) 

# 3.1. SISBEN Bogota: test over the running variable ---------------------------

# Test en tabla
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_bgta30 <- rddensity(X=sisben_bgta$cutoff30, p = p_value, fitselect = fitselect_value,
                                                kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_bgta30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_bgta30 <- rddensity(X=sisben_bgta$cutoff30, p = p_value, fitselect = fitselect_value,
                                                kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_bgta30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_sisben_bgta30 <- unique(resultados_df)

# sisben_bgta 40 PTS
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_bgta40 <- rddensity(X=sisben_bgta$cutoff40, p = p_value, fitselect = fitselect_value,
                                                kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_bgta40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_bgta40 <- rddensity(X=sisben_bgta$cutoff40, p = p_value, fitselect = fitselect_value,
                                                kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_bgta40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_sisben_bgta40 <- unique(resultados_df)
# ----

# 3.2. SISBEN with personab_id (master): test over the running variable --------
# Test en tabla
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_pbid30 <- rddensity(X=sisben_pbid$cutoff30, p = p_value, fitselect = fitselect_value,
                                       kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_pbid30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_pbid30 <- rddensity(X=sisben_pbid$cutoff30, p = p_value, fitselect = fitselect_value,
                                       kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_pbid30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_sisben_pbid30 <- unique(resultados_df)

# sisben_pbid 40 PTS
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_pbid40 <- rddensity(X=sisben_pbid$cutoff40, p = p_value, fitselect = fitselect_value,
                                       kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_pbid40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            sisben_pbid40 <- rddensity(X=sisben_pbid$cutoff40, p = p_value, fitselect = fitselect_value,
                                       kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(sisben_pbid40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_sisben_pbid40 <- unique(resultados_df)

# ----

# 3.3. RIPS-SISBEN: test over the running variable -----------------------------
# Test en tabla
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            main_rips30 <- rddensity(X=main_rips$cutoff30, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_rips30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            main_rips30 <- rddensity(X=main_rips$cutoff30, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_rips30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_main_rips30 <- unique(resultados_df)

# main_rips 40 PTS
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            main_rips40 <- rddensity(X=main_rips$cutoff40, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_rips40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            main_rips40 <- rddensity(X=main_rips$cutoff40, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_rips40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_main_rips40 <- unique(resultados_df)

# ----

# 3.4. PILA-SISBEN: test over the running variable -----------------------------
# Test en tabla
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            main_pila30 <- rddensity(X=main_pila$cutoff30, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_pila30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            main_pila30 <- rddensity(X=main_pila$cutoff30, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_pila30)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_main_pila30 <- unique(resultados_df)

# main_pila 40 PTS
# Corriendo todas las combinaciones del rddensity con vce=jackknife
vce_values <- c("jackknife")

# Inicializar un data frame para almacenar los resultados
resultados_df <- data.frame()

# Iterar sobre todas las combinaciones de valores
for (p_value in p_values) {
  for (fitselect_value in fitselect_values) {
    for (kernel_value in kernel_values) {
      for (vce_value in vce_values) {
        # Iterar sobre bwselect solo si fitselect es unrestricted
        if (!(fitselect_value == "restricted" && "each" %in% bwselect_values)) {
          for (bwselect_value in c(bwselect_values, "each")) {
            # Ajustar el modelo RDD con los valores actuales
            main_pila40 <- rddensity(X=main_pila$cutoff40, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_pila40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        } else {
          # Si fitselect es restricted y "each" está en bwselect_values, excluye "each" de las iteraciones
          bwselect_values_no_each <- bwselect_values[bwselect_values != "each"]
          for (bwselect_value in bwselect_values_no_each) {
            # Ajustar el modelo RDD con los valores actuales
            main_pila40 <- rddensity(X=main_pila$cutoff40, p = p_value, fitselect = fitselect_value,
                                     kernel = kernel_value, vce = vce_value, bwselect = bwselect_value)
            # Extraer los valores del modelo
            valores_rdd <- extraer_valores_jk(main_pila40)
            # Crear un data frame auxiliar solo con los resultados de esta iteración
            df_aux <- data.frame(valores_rdd)
            # Agregar la columna select_bw
            df_aux$select_bw <- bwselect_value
            # Agregar los resultados al data frame principal
            resultados_df <- rbind(resultados_df, df_aux)
          }
        }
      }
    }
  }
}
resultados_rddensity_main_pila40 <- unique(resultados_df)

# ----

# 4. Saving results on parquet format ----
write_parquet(resultados_rddensity_main_pila30, file.path(output_folder,   "rddensity_pila30.parquet"))
write_parquet(resultados_rddensity_main_pila40, file.path(output_folder,   "rddensity_pila40.parquet"))
write_parquet(resultados_rddensity_main_rips30, file.path(output_folder,   "rddensity_rips30.parquet"))
write_parquet(resultados_rddensity_main_rips40, file.path(output_folder,   "rddensity_rips40.parquet"))
write_parquet(resultados_rddensity_sisben_bgta30, file.path(output_folder, "rddensity_sbgta30.parquet"))
write_parquet(resultados_rddensity_sisben_bgta40, file.path(output_folder, "rddensity_sbgta40.parquet"))
write_parquet(resultados_rddensity_sisben_pbid30, file.path(output_folder, "rddensity_spbid30.parquet"))
write_parquet(resultados_rddensity_sisben_pbid40, file.path(output_folder, "rddensity_spbid40.parquet"))

# ----

