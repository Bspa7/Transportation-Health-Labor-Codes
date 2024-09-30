source('_requirements.R')
source('auxiliar_functions.R')
tic()
project_folder <- '//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health'

selected_columns <- c('id_s3', 'ficha', 'zona', 'sector', 'comuna', 'documen',
                      'puntaje', 'edad')


# Identify population -----------------------------------------------------



#' Hay `documen` repetidos, nos quedamos con el mayor menor de 60.
#' Que no elimine todo
#' Que tome el mayor menor de 60.
#' Si tienen la misma edad, quedarme con cualquiera.
df_aux <- open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/SISBEN/SISBEN III/Sisben III_Nacional.parquet') %>% 
  filter(depto == 11, munic == 1, tipodoc == 1, documen != '') %>% 
  select(all_of(selected_columns)) %>%
  mutate(id_s3 = as.numeric(id_s3))

df_aux %>% 
  left_join(
    df_aux %>% 
      group_by(documen) %>%
      summarise(
        n_ = n(),
        all_mayor_60 = all(edad >= 60),
        min_edad = min(edad, na.rm = T)
        ),
    by = 'documen'
    ) %>% 
  #' Si hay repetidos y hay alguno menor de 60, nos quedamos con con todos los
  #' menores de 60. Si por el contrario todos  han cumplido 60 nos quedamos con
  #' el menor.
  mutate(delete = (n_ > 1 & !all_mayor_60 & edad >= 60) |
           (n_ > 1 & all_mayor_60 & min_edad != edad)) %>% 
  filter(!delete) %>% 
  select(-all_mayor_60, -n_, -min_edad, -delete) %>% 
  write_parquet('temp.parquet')

# Finalmente como pueden haber varios menores de 60, nos quedamos con el mayor.
open_dataset('temp.parquet') %>% 
  left_join(
    x = open_dataset('temp.parquet') %>% 
      group_by(documen) %>%
      summarise(edad = max(edad, na.rm = T)),
    by = c('documen', 'edad')
  ) %>% 
  write_parquet('temp.parquet')
# Particionamos los datos en los unicos y los repetidos.
open_dataset('temp.parquet') %>% 
  left_join(
    open_dataset('temp.parquet') %>%
      group_by(documen) %>% 
      summarise(duplicates = n() > 1)
    ) %>% 
  write_dataset('temp', partitioning = 'duplicates')

#' Dado que pueden haber varios con la misma edad, nos quedamos con el primero
#' de manera arbitraria.
open_dataset('temp') %>% 
  filter(duplicates == 'true') %>%
  collect %>% 
  group_by(documen) %>% 
  filter(1 == row_number()) %>% 
  write_parquet('temp/duplicates=true/part-0.parquet')
# Guardamos todo en el mismo lugar
open_dataset('temp') %>% 
  select(-duplicates) %>% 
  write_parquet(
    sprintf('%s/data/sisben3_bogota_duplicates_free.parquet', project_folder)
    )
# Release memory.
rm(df_aux)
unlink(c('temp.parquet', 'temp'), recursive = T)


# Pareo con `personabasicaid` ---------------------------------------------

df_pareo <- open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/personabasicaid_all_projects/Bases insumos/personabasicaid_pila2010.parquet')

open_dataset(sprintf('%s/data/sisben3_bogota_duplicates_free.parquet', project_folder)) %>% 
  left_join(df_pareo %>% filter(identif  != ''), by = join_by(documen == identif)) %>%
  mutate(pareo = !is.na(personabasicaid), corte_puntaje = between(puntaje, 30.56, 40)) %>% 
  mutate(id_s3 = as.numeric(id_s3)) %>% 
  write_parquet(sprintf('%s/data/sisben3_bogota_raw.parquet', project_folder))
# Realease storage.
unlink(sprintf('%s/data/sisben3_bogota_duplicates_free.parquet', project_folder))
# WARNING! There are duplicates in df_pareo.
# open_dataset(sprintf('%s/data/sisben3_bogota_raw.parquet', project_folder)) %>% 
#   group_by(documen) %>% 
#   summarise(duplicate_personabasicaid = n()) %>% 
#   filter(duplicate_personabasicaid > 1) %>% 
#   collect %>% View

read_parquet(sprintf('%s/data/sisben3_bogota_raw.parquet', project_folder)) %>% 
  write_dta(sprintf('%s/data/sisben3_bogota_raw.dta', project_folder))

# Compress in stata and then.
read_dta(sprintf('%s/data/sisben3_bogota_raw.dta', project_folder)) %>% 
  write_parquet(sprintf('%s/data/sisben3_bogota_raw.parquet', project_folder))
unlink(sprintf('%s/data/sisben3_bogota_raw/part-0.parquet', project_folder))

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
  filter(pareo == 1) %>% select(-pareo) %>% 
  left_join(df_aux, by = 'documen') %>% 
  write_parquet(sprintf("%s/%s", project_folder, "data/master.parquet"))

get_values_tic_msg('min')
# Release storage.
unlink(sprintf("%s/%s", project_folder, "data/sisben3_bogota_raw.parquet"))



# Add controls ------------------------------------------------------------
new_controls <- c('documen', 'grado','fechanto', 'estcivil', 'activi')
selected_keys <- c(
  'edad', 'id_s3', 'ficha', 'zona', 'sector', 'comuna', 'puntaje',
  'estrato', 'thogar', 'nivel', 'sexo', 'ingresos'
)

df_aux <- open_dataset('Z:/Christian Posso/_banrep_research/datos_originales/SISBEN/SISBEN III/Sisben III_Nacional.parquet') %>% 
  filter(depto == 11, munic == 1, tipodoc == 1, documen != '') %>% 
  select(all_of(c(new_controls, selected_keys))) %>% 
  mutate(id_s3 = as.numeric(id_s3)) %>% 
  left_join(
    x = open_dataset(sprintf("%s/%s", project_folder, "data/master.parquet"))
    ) %>% distinct %>% 
  write_parquet(file.path(project_folder, "data/master.parquet"))
open_dataset(file.path(project_folder, "data/master.parquet")) %>% 
  select(-documen, -personabasicaid, -id_s3, -ficha) %>% glimpse

