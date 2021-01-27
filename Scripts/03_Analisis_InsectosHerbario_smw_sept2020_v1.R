################ Datos de insectos de CDRS #######################
#Explorar los datos de insectos en relacion a los promedios de temperatura
#y humedad dentro de los intervalos de monitoreo.

pest_data <- read_csv("Data/Raw/datos_trampas_env_2017-20_version.2dic.csv")
pest_data <- as_tibble(pest_data)

# need to add variable columns "tiempo", "year" and "herbario" to new csv

# Changed variables to factors and scaled temperature and humidity
pest_data <- pest_data %>% mutate_at(vars(fecha_puesta, tiempo, 
                                          year, tipo_trampa, 
                                          ubicacion, herbario), list(factor))
pest_data$humedad_scaled <- scale(pest_data$humedad_media)
pest_data$temperatura_scaled <-scale(pest_data$temperatura_media)
pest_data$log_cantidad <- log(pest_data$cantidad + 1)
hist(pest_data$cantidad)
hist(pest_data$log_cantidad)

#Exploring the data
ggplot(pest_data) +
  aes(x = year, y = cantidad, fill = familia) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(trampa))

head(pest_data)
str(pest_data)

# FITTING A GLMM 

####### Efecto de humedad sobre cantidad de insectos ####
full_model <- glmer(cantidad ~ humedad_scaled + temperatura_scaled + humedad_scaled*temperatura_scaled + 
                      trampa + (1|tiempo) + (1|num_trampa) + (1|nombre_cientifico) + (1|herbario), family=poisson, 
                    data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

null_model_hum <- glmer(cantidad ~ temperatura_scaled + humedad_scaled*temperatura_scaled + 
                      trampa + (1|tiempo) + (1|num_trampa) + (1|nombre_cientifico), family=poisson, 
                    data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

null_model_trampa <- glmer(cantidad ~ humedad_scaled + temperatura_scaled + humedad_scaled*temperatura_scaled 
                           + (1|tiempo) + (1|num_trampa) + (1|nombre_cientifico), family=poisson, 
                        data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# are models too complex? Some of the variables may be correlated

#Diagnostic models
plot(fitted(full_model),resid(full_model)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(full_model))
qqline(resid(full_model), col="red")
hist(resid(full_model))

# Comparing models
anova (full_model, null_model_trampa, type="chi")
# Summary of the full model
summary(null_model_trampa)

##### Efecto de la temperatura en la cantidad de insectos ####
null_model_temp <- glmer(cantidad ~ humedad_scaled + humedad_scaled*temperatura_scaled + 
                      trampa + tiempo + (1|num_trampa) + (1|nombre_cientifico) + (1|herbario), family=poisson, 
                    data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

null_model_temp <- glmer(cantidad ~ humedad_scaled + humedad_scaled*temperatura_scaled + 
                           tiempo + (1|nombre_cientifico), family=poisson, 
                         data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
table(pest_data$nombre_cientifico, pest_data$trampa)
plot(pest_data$nombre_cientifico, pest_data$trampa)


#Comparing models
summary(null_model_temp)
anova(full_model, null_model_temp)

#### Efecto de la interaccion de temperatura y la humedad
null_model_inter <- glmer(cantidad ~ humedad_scaled + temperatura_scaled 
                     + trampa + tiempo + (1|num_trampa) + (1|nombre_cientifico) + (1|herbario), family=poisson, 
                    data=pest_data, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#Comparing models
coef(full_model)
summary(null_model_inter)
anova(full_model, null_model_inter)


# Using trap number as factor and using random slopes for trap type
full_model2 <- glmer(cantidad ~ humedad_scaled + temperatura_scaled
                    + trampa + (1+trampa|tiempo) + (1+trampa|as.factor(pest_data$num_trampa)), family=poisson, data=pest_data,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
null_model2 <- glmer(cantidad ~ temperatura_scaled +
                      trampa + (1+trampa|tiempo) + (1+trampa|as.factor(pest_data$num_trampa)), family=poisson, data=pest_data,
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#Diagnostic models
plot(fitted(full_model2),resid(full_model2)) 
abline(h=0,lty=2,col="red")
qqnorm(resid(full_model2))
qqline(resid(full_model2), col="red")
hist(resid(full_model2)) 

#Comparing models
anova (full_model2, null_model2)
#Summary of the full model
summary(full_model2)
coef(full_model2)



plot(log(cantidad) ~ jitter(humedad_media), data=pest_data)
plot(log(cantidad) ~ jitter(temperatura_media), data=pest_data)

#Considerar la interaccion entre humedad y temperatura dentro del modelo


### https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
fix.formula <- cantidad ~ scale(pest_data$humedad_media) + scale(pest_data$temperatura_media) + trampa +
  tiempo
x <- model.matrix(fix.formula, pest_data)
fix.fit <- lm(fix.formula, pest_data, method = "qr", singular.ok = T)
p <- length(coef)
coef <- fix.fit$coefficients
no.Na <- sum(is.na(coef))
rank <- fix.fit$rank






# #install.packages("tidyverse")
# temp_hum_data <- as_tibble(read.csv("Datos_temperatura_humedad_2012-2020_Herbario.csv", h=T, sep=";", dec=","))
# 
# clim_dat_fin <- select(temp_hum_data, Fecha, temp=Temperatura..entrada., humid=Humedad..entrada.) %>% 
#   mutate(date = as.Date(Fecha, format="%d/%m/%Y")) %>% 
#   filter(date>=as.Date("2017-01-01")) %>% 
#   select(-Fecha)
# 
# mean(clim_dat_fin$date)
# 
# data_sum <- clim_dat_fin %>%
#   group_by(month = floor_date(date, "month")) %>%
#   summarize(mean_temp = mean(temp, na.rm=T),
#             mean_humid = mean(humid, na.rm=T),
#             max_temp = max(temp, na.rm=T),
#             max_humid = max(humid, na.rm=T),
#             min_temp = min(temp, na.rm=T),
#             min_humid = min(humid, na.rm=T)) %>% 
#   filter(month < as.Date("2020-05-15"))
# 
# 
# df <- data_sum %>%
#   select(month, mean_humid, mean_temp) %>%
#   gather (key="Medida", value="Temperatura (Cº) y Humedad (%)", -month)
# 
# df <- mutate(df, recode(Medida, mean_temp = "Temperatura promedia", mean_humid = "Humedad promedia"))
# 
# colnames(df) <- c("Fecha", "x", "Temperatura (Cº) y Humedad (%)", "Medida")
# 
# 
# 
# data_sum_year <- data_sum %>%
#   mutate(month = months(month))
# ggplot(data_sum_year, aes(x=month, y=mean_temp)) + 
#   geom_boxplot()
# 
# data_sum_year <- data_sum %>%
#   mutate(month = months(month))
# ggplot(data_sum_year, aes(x=month, y=mean_humid)) + 
#   geom_boxplot()
# 
# ggplot(data_sum, aes(x=mean_temp, y=mean_humid)) + 
#   geom_point()
# 
# 
# # set the plotting theme:
# my_theme <- list(
#   theme_classic() +
#     theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"),
#           strip.background = element_rect(color = NA),
#           axis.text.x = element_text(size = 13),
#           axis.text.y = element_text(size = 13),
#           axis.title.x = element_text(size = 15, margin=ggplot2::margin(t=0.5, unit="cm")),
#           axis.title.y = element_text(size = 15, margin=ggplot2::margin(r=0.5, unit="cm")),
#           legend.title = element_text(size = 15),
#           legend.text = element_text(size = 13),
#           plot.title = element_text(size = 16),
#           axis.line=element_line(),
#           panel.spacing = unit(1, "lines")),
#   scale_shape_manual(values=c(17,16))
# )
# 
# quartz(w=7,h=5)
# ggplot(data_sum, aes(x=month)) +
#   theme_classic() +
#   my_theme +
#   geom_line(aes(y=mean_temp), col=2, size=1.2) +
#   # geom_ribbon(aes(ymin=min_temp, ymax=max_temp), alpha=0.5, fill=2) +
#   labs(x="Date", y="Mean monthly\nTemperature (C°)")
# 
# 
# 
# ggplot(data_sum_year, aes(x=month, y=`Temperatura (Cº) y Humedad (%)`)) + 
#   geom_bocplot
#   
# quartz(w=7,h=5)
# ggplot(data_sum, aes(x=month)) +
#   theme_classic() +
#   my_theme +
#   geom_line(aes(y=mean_humid), col=4, size=1.2) +
#   # geom_ribbon(aes(ymin=min_humid, ymax=max_humid), alpha=0.5, fill=4) +
#   labs(x="Date", y="Mean monthly\nHumidity (%)")
# 
# quartz(w=5,h=4)
# ggplot(data_sum) +
#   theme_classic() +
#   my_theme +
#   geom_point(aes(y=mean_humid, x=mean_temp), col=6, size=2) +
#   # geom_smooth(aes(x=mean_temp, y=mean_humid), method="lm") +
#   labs(x="Mean Monthly Humidity", y="Mean Monthly Temperature")
# 
# 
# 
# # Gráfico temp +  hum promedias a lo largo de los años
# 
# quartz(w=5,h=4)
# ggplot(df, aes(x=Fecha, y=`Temperatura (Cº) y Humedad (%)`)) +
#   geom_line(aes(color=Medida, linetype=Medida)) +
#   scale_color_manual(values=c("darkred", "blue")) +
# labs(x="Date", y="Temperature (C°) and Humidity (%)")
# 
# 
# ?aggregate
# head(pest_data)
# #pest_data <- aggregate(Cantidad, by=list(Fecha.de.monitoreo, sum))
# 
# plot(pest_data$Individuos.monitoreo, pest_data$Temperatura.promedio)
# 
# #######
# ## Changes in pest numbers with temp/hum
# ## Changes in pest numbers with location (central, ampl, entrada)
# ## Changes in families with temp/hum increase
# ## Test for harmful pest families/species
# 
# 
# pest_data$monitoreo=factor(pest_data$monitoreo,levels=unique(pest_data$monitoreo))
# pest_data$trampa=factor(pest_data$trampa,levels=unique(pest_data$trampa))
# pest_data$especie=factor(pest_data$especie,ordered=TRUE)
# pest_data$abundancia=factor(pest_data$abundancia,ordered=TRUE)
# View(pest_data)
# 
# #Análisis
# #Modelo por bloques (monitoreo y trampas)
# trampa1<-pest_data$Nro..De.Trampa
# trampa1<-factor(trampa1)
# (trampa1)
# monitoreo1<-pest_data$Fecha.de.monitoreo
# monitoreo1<-factor(monitoreo1)
# 
# #Bloques de referencia para la comparaci?n
# trampa1<- relevel(trampa1, ref="1")
# monitoreo1<-relevel(monitoreo1, ref="mayo-diciembre/2017")
# #Modelo lineal_comparaci?n de especies entre trampas y monitoreo
# model1<-lm(pest_data$especie~trampa1)
# model2<-lm(pest_data$especie~monitoreo1)
# summary(model1)
# summary(model2)
# 
# #Modelo lineal - comparación de abundancia entre trampas y monitoreo
# model3<-lm(abundancia~trampa1)
# model4<-lm(abundancia~monitoreo1)
# summary(model3)
# summary(model4)
# 
# #GRÁFICOS
# par(mar=c(4.3, 4.1, 0.8, 2.1),mfrow=c(2,2))
# boxplot(especie~monitoreo,xlab="Monitoreos", ylab="Nº de especies", data=pest_data)
# boxplot(pest_data$especie~trampa,xlab="Trampas", ylab="Nº de especies", data=pest_data)
# boxplot(abundancia~monitoreo,xlab="Monitoreos", ylab="Abundancia", data=pest_data)
# boxplot(abundancia~trampa, xlab="Trampas", ylab="Abundancia", data=pest_data)
# 
