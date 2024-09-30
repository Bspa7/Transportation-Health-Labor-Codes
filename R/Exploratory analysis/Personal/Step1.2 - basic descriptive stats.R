paquetes <- c("rddensity", "writexl", "arrow", "tidyverse", "haven", "dplyr", "openxlsx", "ggplot2", "rdrobust", "cowplot", "ragg", "R.utils", "rlang", "lubridate", "crayon", "purrr")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}

#base_dir  <- "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
#base_dir  <- "D:/Steban Pineda/Documents/DIME/Transportation and health"
base_dir  <- "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"

project_folder <- base_dir
data_dir       <- file.path(base_dir, "data")
output_folder  <- file.path(base_dir, "outputs")
figure_folder  <- file.path(output_folder, "figures")

# 1. Initial databases ---------------------------------------------------------
master            <- open_dataset(sprintf('%s/%s', project_folder, 'data/master.parquet'))
main_base <- open_dataset(sprintf('%s/%s', data_dir, "base_pila_rips_pbid_monthly.parquet")) %>% collect()
RIPS_history_file <- sprintf('%s/%s', project_folder, 'data/Data_health/history_RIPS.parquet')

# Capturing gender from RIPS information to replace NA's in sisben gender
rips_gender <- open_dataset(RIPS_history_file) %>%
  select(personabasicaid, sexo_RIPS) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(rips_gender = if_else(sexo_RIPS == "M", 1, 0))

main_base <- main_base %>% 
  left_join(rips_gender, by = "personabasicaid") %>% 
  mutate(genero = coalesce(genero, rips_gender)) %>% 
  select(personabasicaid, puntaje, genero, estrato, activi, estcivil) %>% 
  unique()

unique_pbid <- master %>% 
  mutate(estrato_master = estrato,
         activi_master = activi,
         estcivil_master = estcivil) %>% 
  select(personabasicaid, estrato_master, activi_master, estcivil_master) %>% 
  unique() %>% 
  collect()

main_base <- main_base %>% 
  left_join(unique_pbid, by = "personabasicaid") %>% 
  mutate(
    estrato  = coalesce(estrato,  estrato_master),
    activi   = coalesce(activi,   activi_master),
    estcivil = coalesce(estcivil, estcivil_master)    
  )  %>% 
  select(-estrato_master, -activi_master, -estcivil_master)  

rm(RIPS_history_file, rips_gender, unique_pbid)
gc()


# Dummy variables
unique_individuals <- main_base %>%
  mutate(
    male    =  if_else(genero ==1, 1, 0),
    female  =  if_else(genero ==0, 1, 0),
    score_m15 =  if_else(puntaje <= 15, 1, 0),    
    score_1520 = if_else(puntaje > 15 & puntaje <= 20, 1, 0),
    score_2025 = if_else(puntaje > 20 & puntaje <= 25, 1, 0),
    score_2530 = if_else(puntaje > 25 & puntaje <= 30.56, 1, 0),
    score_3035 = if_else(puntaje > 30.56 & puntaje <= 35, 1, 0),
    score_3540 = if_else(puntaje > 35 & puntaje <= 40, 1, 0),
    score_4245 = if_else(puntaje > 40 & puntaje <= 45, 1, 0),
    score_45m = if_else(puntaje > 45, 1, 0),
    estrato_1 = if_else(estrato == 1, 1, 0),
    estrato_2 = if_else(estrato == 2, 1, 0),
    estrato_3 = if_else(estrato == 3, 1, 0),
    estrato_4 = if_else(estrato == 4, 1, 0),
    estrato_5 = if_else(estrato == 5, 1, 0),
    estrato_6 = if_else(estrato == 6, 1, 0),
    activi_sin = if_else(activi == 0, 1, 0),
    activi_tra = if_else(activi == 1, 1, 0),
    activi_bus = if_else(activi == 2, 1, 0),
    activi_est = if_else(activi == 3, 1, 0),
    activi_hog = if_else(activi == 4, 1, 0),
    estcivil_unlibr = if_else(estcivil == 1, 1, 0),
    estcivil_casado = if_else(estcivil == 2, 1, 0),
    estcivil_solter = if_else(estcivil == 4, 1, 0),    
  )


# Tables counting individuals
summary_counts <- unique_individuals %>%
  summarise(
    total_individuals = n(),
    total_male = sum(male),
    total_female = sum(female),
    total_score_m15 = sum(score_m15),
    total_score_1520 = sum(score_1520),
    total_score_2025 = sum(score_2025),
    total_score_2530 = sum(score_2530),
    total_score_3035 = sum(score_3035),
    total_score_3540 = sum(score_3540),
    total_score_4245 = sum(score_4245),
    total_score_45m = sum(score_45m),
    total_estrato_1 = sum(estrato_1),
    total_estrato_2 = sum(estrato_2),
    total_estrato_3 = sum(estrato_3),
    total_estrato_4 = sum(estrato_4),
    total_estrato_5 = sum(estrato_5),
    total_estrato_6 = sum(estrato_6),
    total_activi_sin = sum(activi_sin),
    total_activi_tra = sum(activi_tra),
    total_activi_bus = sum(activi_bus),
    total_activi_est = sum(activi_est),
    total_activi_hog = sum(activi_hog),
    total_estcivil_unlibr = sum(estcivil_unlibr),
    total_estcivil_casado = sum(estcivil_casado),
    total_estcivil_solter = sum(estcivil_solter)
  )

# Calculate some descriptive statistics (min, mean, max) for each dummy  
summary_stats <- unique_individuals %>%
  summarise(
    male_min = min(male), male_mean = mean(male), male_max = max(male),
    female_min = min(female), female_mean = mean(female), female_max = max(female),
    score_m15_min = min(score_m15), score_m15_mean = mean(score_m15), score_m15_max = max(score_m15),
    score_1520_min = min(score_1520), score_1520_mean = mean(score_1520), score_1520_max = max(score_1520),
    score_2025_min = min(score_2025), score_2025_mean = mean(score_2025), score_2025_max = max(score_2025),
    score_2530_min = min(score_2530), score_2530_mean = mean(score_2530), score_2530_max = max(score_2530),
    score_3035_min = min(score_3035), score_3035_mean = mean(score_3035), score_3035_max = max(score_3035),
    score_3540_min = min(score_3540), score_3540_mean = mean(score_3540), score_3540_max = max(score_3540),
    score_4245_min = min(score_4245), score_4245_mean = mean(score_4245), score_4245_max = max(score_4245),
    score_45m_min = min(score_45m), score_45m_mean = mean(score_45m), score_45m_max = max(score_45m),
    estrato_1_min = min(estrato_1), estrato_1_mean = mean(estrato_1), estrato_1_max = max(estrato_1),
    estrato_2_min = min(estrato_2), estrato_2_mean = mean(estrato_2), estrato_2_max = max(estrato_2),
    estrato_3_min = min(estrato_3), estrato_3_mean = mean(estrato_3), estrato_3_max = max(estrato_3),
    estrato_4_min = min(estrato_4), estrato_4_mean = mean(estrato_4), estrato_4_max = max(estrato_4),
    estrato_5_min = min(estrato_5), estrato_5_mean = mean(estrato_5), estrato_5_max = max(estrato_5),
    estrato_6_min = min(estrato_6), estrato_6_mean = mean(estrato_6), estrato_6_max = max(estrato_6),
    activi_sin_min = min(activi_sin), activi_sin_mean = mean(activi_sin), activi_sin_max = max(activi_sin),
    activi_tra_min = min(activi_tra), activi_tra_mean = mean(activi_tra), activi_tra_max = max(activi_tra),
    activi_bus_min = min(activi_bus), activi_bus_mean = mean(activi_bus), activi_bus_max = max(activi_bus),
    activi_est_min = min(activi_est), activi_est_mean = mean(activi_est), activi_est_max = max(activi_est),
    activi_hog_min = min(activi_hog), activi_hog_mean = mean(activi_hog), activi_hog_max = max(activi_hog),
    estcivil_unlibr_min = min(estcivil_unlibr), estcivil_unlibr_mean = mean(estcivil_unlibr), estcivil_unlibr_max = max(estcivil_unlibr),
    estcivil_casado_min = min(estcivil_casado), estcivil_casado_mean = mean(estcivil_casado), estcivil_casado_max = max(estcivil_casado),
    estcivil_solter_min = min(estcivil_solter), estcivil_solter_mean = mean(estcivil_solter), estcivil_solter_max = max(estcivil_solter)
  )

write_parquet(summary_counts, file.path(output_folder, "parquet/20240826-gnral_counts.parquet"))
write_parquet(summary_stats,  file.path(output_folder, "parquet/20240826-gnral_stats.parquet"))

# Figures (frequency and density plots)
fem <- unique_individuals %>% filter(genero==0)
mal <- unique_individuals %>% filter(genero==1)
scores1545 <- unique_individuals %>% filter(puntaje > 15 & puntaje <= 45)
scores1545_fem <- unique_individuals %>% filter(puntaje > 15 & puntaje <= 45 & genero==0)
scores1545_mal <- unique_individuals %>% filter(puntaje > 15 & puntaje <= 45 & genero==1)

# All sample
ggplot(unique_individuals, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(unique_individuals)), color = "red") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_population.png"), width = 8, height = 6)

# Female population
ggplot(fem, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(fem)), color = "purple") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_females.png"), width = 8, height = 6)

# Male population
ggplot(mal, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(mal)), color = "blue") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_males.png"), width = 8, height = 6)

# All sample: 15-45 scores
ggplot(scores1545, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(scores1545)), color = "red") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_scores1545.png"), width = 8, height = 6)

# Female sample: 15-45 scores
ggplot(scores1545_fem, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(scores1545_fem)), color = "purple") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_scores1545_fem.png"), width = 8, height = 6)

# Male sample: 15-45 scores
ggplot(scores1545_mal, aes(x = puntaje)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, fill = "gray", color = "black") +
  geom_density(aes(y = ..density.. * nrow(scores1545_mal)), color = "blue") +
  labs(title = "",
       x = "Score", y = "Frequency / Density")
ggsave(file.path(figure_folder, "20240826-histogram_scores1545_mal.png"), width = 8, height = 6)

# Cleaning environment 
rm(list=ls())
