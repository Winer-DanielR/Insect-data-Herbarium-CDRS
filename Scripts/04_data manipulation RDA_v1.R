##### Manipulación de los datos para ajustarlos a los análisis que sugerió Nico #####
##### Mediciones inividuales por TRAMPA y ABUNDANCIA de especies
# TRAMPA sería cada fila y especies por columna con su abundancia

# Por ahora seleccionaré TRAMPA ID, ESPECIE y ABUNDANCIA para armar una matriz

all_data <- read_csv("Data/Processed/datos de insectos y temperatura final.csv")

### Filtrar columnas ####

abundance_species <- select(all_data, trampa_ID_unico, especie, abundancia_trampa)
abundance_order <- select(all_data, trampa_ID_unico, orden, abundancia_trampa)
# Cambiar especies a una serie de columnas con su abundancia respectiva por trampa

# Para especies
species <- abundance_species %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(especie, abundancia_trampa) %>% select(-i1)

species[is.na(species)] = 0

# Para orden
orden <- abundance_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
orden[is.na(orden)] = 0

# Remover columna de NA en orden
orden <- select(orden, c(1:14))
# Sumar abundancias por trampa
orden_sum <- orden %>% group_by(trampa_ID_unico) %>%
  summarize(across(c(1:13), sum))

# Exportar matriz de orden
write_csv(orden_sum, "Data/Processed/Matrix orden.csv")

# Combinar tablas y filtrar columnas. REMOVER: 
species_all <- bind_cols(species, all_data)
species_all <- rename(species_all, trampa_ID_unico = trampa_ID_unico...1)
species_col <- species_all[-c(57:59,61:73)]

str(species_col)
species_col <- species_col %>% mutate_at(vars(c(52:57)), list(factor))

# Sumar el numero de abundancia por cada trampa ID:
species_sum <- species_col %>% group_by(trampa_ID_unico) %>%
  summarize(across(c(1:48), sum))

species_info <- select(species_col, (c(1,50:57)))
species_info <- species_info %>% group_by(trampa_ID_unico) %>% filter(row_number()==1) 

species_final <- bind_cols(species_sum, species_info)
species_final <- rename(species_final, trampa_ID_unico = trampa_ID_unico...1)
species_final <- species_final[-c(50)]

# Marices finales, creadas para la RDA. Matriz de especies:

write_csv(species_final, "Data/Processed/Matrix especies.csv")

# Hacer una matriz de temperatura y humedad por trampa

temp_hum_col <- select(all_data, trampa_ID_unico, temp_mean, temp_sd, temp_se, hum_mean, hum_sd, hum_se, fecha_puesta, 
                   fecha_recogida, dias_monitoreo, periodo_monitoreo,
                   monitoreo, tipo_trampa, marca_trampa, ubicacion)
temp_hum_col <- temp_hum_col %>% group_by(trampa_ID_unico) %>% filter(row_number()==1)

temp_hum_col$year <- year(temp_hum_col$fecha_recogida)


# Matrices finales, para la RDA. Matriz de parametros ambientales:

write_csv(temp_hum_col, "Data/Processed/Matrix temperatura humedad.csv")

# ====== Datos por monitoreo ======

# Hacer una matriz de abundancia por orden por monitoreo

# Suma de abundancia por orden por monitoreo


# Monitoreo 1
monitoreo1 <- filter(all_data, monitoreo == "1")
monitoreo1_order <- select(monitoreo1, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon1_orden <- monitoreo1_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon1_orden[is.na(mon1_orden)] = 0
mon1_orden_sum <- mon1_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:6), sum))

# Monitoreo 2
monitoreo2 <- filter(all_data, monitoreo == "2")
monitoreo2_order <- select(monitoreo2, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon2_orden <- monitoreo2_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon2_orden[is.na(mon2_orden)] = 0
mon2_orden_sum <- mon2_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:7), sum))

# Monitoreo 3
monitoreo3 <- filter(all_data, monitoreo == "3")
monitoreo3_order <- select(monitoreo3, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon3_orden <- monitoreo3_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon3_orden[is.na(mon3_orden)] = 0
mon3_orden_sum <- mon3_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:8), sum))

# Monitoreo 4
monitoreo4 <- filter(all_data, monitoreo == "4")
monitoreo4_order <- select(monitoreo4, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon4_orden <- monitoreo4_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon4_orden[is.na(mon4_orden)] = 0
mon4_orden_sum <- mon4_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:6), sum))

# Monitoreo 5
monitoreo5 <- filter(all_data, monitoreo == "5")
monitoreo5_order <- select(monitoreo5, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon5_orden <- monitoreo5_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon5_orden[is.na(mon5_orden)] = 0
mon5_orden_sum <- mon5_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:10), sum))

# Monitoreo 6
monitoreo6 <- filter(all_data, monitoreo == "6")
monitoreo6_order <- select(monitoreo6, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon6_orden <- monitoreo6_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon6_orden[is.na(mon6_orden)] = 0
mon6_orden_sum <- mon6_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:8), sum))

# Monitoreo 7
monitoreo7 <- filter(all_data, monitoreo == "7")
monitoreo7_order <- select(monitoreo7, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon7_orden <- monitoreo7_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon7_orden[is.na(mon7_orden)] = 0
mon7_orden_sum <- mon7_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:9), sum))

# Monitoreo 8
monitoreo8 <- filter(all_data, monitoreo == "8")
monitoreo8_order <- select(monitoreo8, monitoreo, trampa_ID_unico, orden, abundancia_trampa)
mon8_orden <- monitoreo8_order %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(orden, abundancia_trampa) %>% select(-i1)
mon8_orden[is.na(mon8_orden)] = 0
mon8_orden_sum <- mon8_orden %>% group_by(monitoreo) %>%
  summarize(across(c(2:8), sum))

# Unir todas las sumas de orden por monitoreo

sum_monitoreo_all <- bind_rows(mon1_orden_sum,
                               mon2_orden_sum,
                               mon3_orden_sum,
                               mon4_orden_sum,
                               mon5_orden_sum,
                               mon6_orden_sum,
                               mon7_orden_sum,
                               mon8_orden_sum)
sum_monitoreo_all[is.na(sum_monitoreo_all)] = 0

# Matriz final de abundancia de ordenes por monitoreo
write_csv(sum_monitoreo_all, "Data/Processed/Matrix abundancia orden por monitoreo.csv")

