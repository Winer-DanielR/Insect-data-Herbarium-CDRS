################ Datos de insectos de CDRS #######################

### Data exploration of insect dataset and environmental variables
pest_data <- read_csv("~/R/CDRS Herbarium insects/Insect-data-Herbarium-CDRS/Data/Raw/datos_trampas_env_2017-20_oct.csv")
pest_data <- as_tibble(pest_data)

