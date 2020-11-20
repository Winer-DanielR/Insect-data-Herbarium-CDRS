################ Datos de insectos de CDRS #######################

### Data exploration of insect dataset and environmental variables

### Clean the data, check for NAs and get the data ready for analysis
### Summarized data to estimate means of temperature and humidity given the time intervals of the traps
### See insect dataset.

env_data <- read_csv("~/R/CDRS Herbarium insects/Insect-data-Herbarium-CDRS/Data/Raw/Datos temperatura y humedad 2012-2020 Herbario CDS .csv")
env_data <- as_tibble(env_data)

names(env_data)
head(env_data)
str(env_data)


# Changed variables to factors
env_data <- env_data %>% mutate_at(vars(meses, meses_nombre, year), list(factor))

#Estimate summary statistics per month similar to the intervals in the insect data

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

#5th interval 2019_Feb - 2020_Feb
interval_5 <- env_data[1848:2144,]
interval_5 <- interval_5[!is.na(interval_5$Temp_entrada), ]

interval5_mean <- summarise(interval_5, mean_temp = mean(Temp_entrada), mean_hum = mean(Humedad_entrada))

# I copied the summaries of each interval of temperature and humidity per interval and paste them in the insect database
# There is potential to summarise the environmental data between two seasons to see diferences.


