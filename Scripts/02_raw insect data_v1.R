################ Datos de insectos de CDRS #######################

### Data exploration of insect dataset and environmental variables

pest_data <- read_csv("Data/Processed/datos de insectos y temperatura final.csv")

# Ajustar variables como factores
pest_data <- pest_data %>% mutate_at(vars(periodo_monitoreo,
                                                monitoreo,
                                                tipo_trampa,
                                                marca_trampa,
                                                trampa_ID_unico,
                                                ubicacion,
                                                orden,
                                                familia,
                                                especie), list(factor))

names(monitor_data)
head(monitor_data)
str(monitor_data)

####### Efecto de humedad sobre cantidad de insectos ####

full_model <- glmer(abundancia_trampa ~ hum_mean + temp_mean
                    + (1|orden)
                    + (1|trampa_ID_unico)
                    + (1|ubicacion), family=poisson, 
                    data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#Diagnostic models
plot(fitted(full_model_scaled),resid(full_model_scaled)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(full_model_scaled))
qqline(resid(full_model_scaled), col="red")
hist(resid(full_model_scaled))

##### Anova comparando cada response variable (humedad y temeperatura media)
Anova(full_model_scaled)
emmeans::emmeans(full_model_scaled, specs = "temp_mean")
emmeans::ref_grid(full_model)
