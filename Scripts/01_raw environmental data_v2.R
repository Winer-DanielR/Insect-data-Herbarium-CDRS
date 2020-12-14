################ Datos de insectos de CDRS #######################

### Data exploration of insect dataset and environmental variables

### Clean the data, check for NAs and get the data ready for analysis
### Summarized data to estimate means of temperature and humidity given the time intervals of the traps
### See insect dataset.
### Incorporar promedios de temperatura y humedad dentro de los datos de insectos correspondiente a los intervalos de monitoreo


#### Preparacion de las bases de datos ambientales y de insectos ####

# Temperature dataset
env_data <- read_csv("~/R/CDRS Herbarium insects/Insect-data-Herbarium-CDRS/Data/Raw/Datos temperatura y humedad 2012-2020 Herbario CDS .csv")
env_data <- as_tibble(env_data)

# Insect trap dataset
monitor_data <- read_csv("~/R/CDRS Herbarium insects/Insect-data-Herbarium-CDRS/Data/Processed/datos_trampas_2017_20_V3.csv", 
                                     col_types = cols(fecha_puesta = col_date(format = "%m/%d/%Y"), 
                                                      fecha_recogida = col_date(format = "%m/%d/%Y"), 
                                                      dias_monitoreo = col_number(), abundancia_trampa = col_number(), 
                                                      especies_monitoreo = col_number(), 
                                                      num_especies_monitoreo = col_number(), 
                                                      abundancia_monitoreo = col_number()))
monitor_data <- as_tibble(monitor_data)

# Check datasets
names(env_data)
head(env_data)
str(env_data)

names(monitor_data)
head(monitor_data)
str(monitor_data)

# Changed variables to factors
env_data <- env_data %>% mutate_at(vars(meses, meses_nombre, year), list(factor))
monitor_data <- monitor_data %>% mutate_at(vars(periodo_monitoreo,
                                                monitoreo,
                                                tipo_trampa,
                                                marca_trampa,
                                                trampa_ID_unico,
                                                ubicacion,
                                                orden,
                                                familia,
                                                especie), list(factor))

# num_trampa_monitoreo es un trampa ID por monitoreo.


##### Estimate summary statistics per month similar to the intervals in the insect data ####

#1st interval 2017_May - 2017_Dic
interval1 <- filter(env_data, year == "2017") #Select year
interval_1 <- interval1[86:260,] #Select the interval from May to Dic
interval_1 <- interval_1[!is.na(interval_1$Temp_entrada), ]

interval1_mean <- summarise(interval_1, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

#2nd interval 2017_Dic - 2018_Jul
interval_2 <- env_data[1546:1717,]
interval_2 <- interval_2[!is.na(interval_2$Temp_entrada), ]

interval2_mean <- summarise(interval_2, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

#3rd interval 2018_Jul - 2019_Feb
interval_3 <- env_data[1696:1867,]
interval_3 <- interval_3[!is.na(interval_3$Temp_entrada), ]

interval3_mean <- summarise(interval_3, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

#4th interval 2019_Feb - 2019_Sept
interval_4 <- env_data[1848:2019,]
interval_4 <- interval_4[!is.na(interval_4$Temp_entrada), ]

interval4_mean <- summarise(interval_4, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

#5th interval 2019_Feb - 2020_Ene
interval_5 <- env_data[1848:2115,]
interval_5 <- interval_5[!is.na(interval_5$Temp_entrada), ]

interval5_mean <- summarise(interval_5, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

#6th interval 2020_Feb - 2020_Abr
interval_6 <- env_data[2116:2205,]
interval_6 <- interval_6[!is.na(interval_6$Temp_entrada), ]
interval_6 <- interval_6[!is.na(interval_6$Humedad_entrada), ]

interval6_mean <- summarise(interval_6, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

#7th interval 2020_Ene - 2020_Abr #### ESPERAR DATOS ACTUALIZADOS
interval_6 <- env_data[2116:2205,]
interval_6 <- interval_6[!is.na(interval_6$Temp_entrada), ]
interval_6 <- interval_6[!is.na(interval_6$Humedad_entrada), ]

interval6_mean <- summarise(interval_6, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

###### Crear nuevas columnas para los datos de temperatura y humedad promedio por intervalos en los datos de insectos ####

# Intervalo 1
monitor_1 <- filter(monitor_data, monitoreo == 1)
monitor_1 <- mutate(monitor_1, temp_mean = interval1_mean$mean_temp, hum_mean = interval1_mean$mean_hum)

# Intervalo 2
monitor_2 <- filter(monitor_data, monitoreo == 2)
monitor_2 <- mutate(monitor_2, temp_mean = interval2_mean$mean_temp, hum_mean = interval2_mean$mean_hum)

# Intervalo 3
monitor_3 <- filter(monitor_data, monitoreo == 3)
monitor_3 <- mutate(monitor_3, temp_mean = interval3_mean$mean_temp, hum_mean = interval3_mean$mean_hum)

# Intervalo 4
monitor_4 <- filter(monitor_data, monitoreo == 4)
monitor_4 <- mutate(monitor_4, temp_mean = interval4_mean$mean_temp, hum_mean = interval4_mean$mean_hum)

# Intervalo 5
monitor_5 <- filter(monitor_data, monitoreo == 5)
monitor_5 <- mutate(monitor_5, temp_mean = interval5_mean$mean_temp, hum_mean = interval5_mean$mean_hum)

# Intervalo 6
monitor_6 <- filter(monitor_data, monitoreo == 6)
monitor_6 <- mutate(monitor_6, temp_mean = interval6_mean$mean_temp, hum_mean = interval6_mean$mean_hum)




#### Unir los intervalos con sus promedios de temperatura y humedad en un solo dataframe ####

insect_env_full_data <- bind_rows(monitor_1,
                                  monitor_2,
                                  monitor_3,
                                  monitor_4,
                                  monitor_5,
                                  monitor_6)

#### Exportar la base de datos con los promedios. Processed dataset ####
write_csv(insect_env_full_data, "~/R/CDRS Herbarium insects/Insect-data-Herbarium-CDRS/Data/Processed/datos de insectos y temperatura final.csv")



