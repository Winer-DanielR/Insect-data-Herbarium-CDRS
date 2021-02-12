##### Manipulación de los datos para ajustarlos a los análisis que sugerió Nico #####
##### Mediciones inividuales por TRAMPA y ABUNDANCIA de especies
# TRAMPA sería cada fila y especies por columna con su abundancia

# Por ahora seleccionaré TRAMPA ID, ESPECIE y ABUNDANCIA para armar una matriz

all_data <- read_csv("Data/Processed/datos de insectos y temperatura final.csv")

# Filtrar columnas

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

temp_hum_col <- select(all_data, trampa_ID_unico, temp_mean, hum_mean, fecha_puesta, 
                   fecha_recogida, dias_monitoreo, periodo_monitoreo,
                   monitoreo, tipo_trampa, marca_trampa, ubicacion)
temp_hum_col <- temp_hum_col %>% group_by(trampa_ID_unico) %>% filter(row_number()==1)

# Matrices finales, para la RDA. Matriz de parametros ambientales:

write_csv(temp_hum_col, "Data/Processed/Matrix temperatura humedad.csv")
