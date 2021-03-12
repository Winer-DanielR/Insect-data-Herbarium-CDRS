################ Datos de insectos de CDRS #######################

### Data exploration of insect dataset and environmental variables

pest_data <- read_csv("Data/Processed/datos de insectos y temperatura final.csv")
pest_data <- as_tibble(pest_data)

# Add variable column  "year"
pest_data$year <- year(pest_data$fecha_recogida)
select(pest_data, fecha_puesta:fecha_recogida, year, everything())


# Changed variables to factors and scaled temperature and humidity
pest_data <- pest_data %>% mutate_at(vars(monitoreo, 
                                          tipo_trampa, marca_trampa,
                                          ubicacion), list(factor))

diagnostic(sqrt(pest_data$abundancia_trampa))




# pest_data$humedad_scaled <- scale(pest_data$hum_mean)
# pest_data$temperatura_scaled <-scale(pest_data$temp_mean)
# pest_data$log_abundancia_trampa <- log(pest_data$abundancia_trampa + 1)
# hist(pest_data$abundancia_trampa)
# hist(pest_data$log_abundancia_trampa)

#Exploring the data
ggplot(pest_data) +
  aes(x = year, y = abundancia_trampa, fill = familia) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(tipo_trampa))

head(pest_data)
str(pest_data)

# FITTING A GLMM 

####### Modelo general considerando la humedad, la temperatura y la interaccion de humedad y temperatura ####
full_model <- glmer(abundancia_trampa ~ log(hum_mean) + log(temp_mean) + log(temp_mean*hum_mean) + 
                      (1|tipo_trampa) + (1|monitoreo) + (1|ubicacion), family=poisson, 
                    data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
model <- glmmTMB::glmmTMB(abundancia_trampa ~ temp_mean + hum_mean 
                          + temp_mean*hum_mean
                          + monitoreo
                          + (1|trampa_ID_unico)
                          + (1|ubicacion), data = pest_data,
                          ziformula = ~.,
                          family = poisson)

summary(model)
Anova(full_model)
hist(resid(full_model))

plotResiduals(model)
plotQQunif(model)
#Diagnostic models
plot(fitted(full_model),resid(full_model)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(full_model))
qqline(resid(full_model), col="red")
hist(resid(full_model))


##### Anova comparando cada response variable (humedad y temeperatura media)
Anova(full_model) # Anova of the full model using the package cars. Summary shows significance per explanatory
emmeans::emmeans(full_model, specs = "tipo_trampa")
emmeans::ref_grid(full_model)
