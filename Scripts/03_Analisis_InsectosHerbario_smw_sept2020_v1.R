

setwd("~/Documents/Herbario CDS/Paper plagas Herbario/Análisis trampas plagas")
pest_data<-read.csv("datos_trampas_env_2017-20_oct.csv", h=T, sep=";", dec=",")
View(pest_data)
attach(pest_data)
head(pest_data)

# FITTING A GLMM 

# Efecto de humedad sobre cantidad de insectos
library("lme4")
packageVersion("lme4")
full_model <- glmer(Cantidad ~ scale(pest_data$Humedad.promedia) + scale(pest_data$Temperatura.promedio) + Tipo.de.trampa + Meses.monitoreando + (1|Nro..De.Trampa) + (1|Nombre.cientifico), family=poisson, data=pest_data, control=glmerControl(optimizer="bobyqa",
                                                                                                                                                                                                                                                  optCtrl=list(maxfun=2e5)))
null_model <- glmer(Cantidad ~ scale(pest_data$Temperatura.promedio) + Tipo.de.trampa + Meses.monitoreando + (1|Nro..De.Trampa) + (1|Nombre.cientifico), family=poisson, data=pest_data, control=glmerControl(optimizer="bobyqa",
                                                                                                                                                                                                              optCtrl=list(maxfun=2e5)))
   # Comparing models
anova (full_model, null_model, type="chi")

full_model <- glmer(Cantidad ~ scale(pest_data$Humedad.promedia) + scale(pest_data$Temperatura.promedio)
                    + Tipo.de.trampa + (1|Fecha.de.monitoreo) + (1|as.factor(pest_data$Nro..De.Trampa)), family=poisson, data=pest_data)
null_model <- glmer(Cantidad ~ scale(pest_data$Temperatura.promedio)+
                      Tipo.de.trampa + (1|Fecha.de.monitoreo) + (1|as.factor(pest_data$Nro..De.Trampa)), family=poisson, data=pest_data)
anova (full_model, null_model)
summary(full_model)

plot(log(Cantidad) ~ jitter(Humedad.promedia), data=pest_data)
plot(log(Cantidad) ~ jitter(Temperatura.promedio), data=pest_data)

#install.packages("tidyverse")
library("tidyverse")
library("lubridate")
setwd("~/Documents/Herbario CDS/Paper plagas Herbario/Análisis trampas plagas")
temp_hum_data <- as_tibble(read.csv("Datos_temperatura_humedad_2012-2020_Herbario.csv", h=T, sep=";", dec=","))

clim_dat_fin <- select(temp_hum_data, Fecha, temp=Temperatura..entrada., humid=Humedad..entrada.) %>% 
  mutate(date = as.Date(Fecha, format="%d/%m/%Y")) %>% 
  filter(date>=as.Date("2017-01-01")) %>% 
  select(-Fecha)

mean(clim_dat_fin$date)

data_sum <- clim_dat_fin %>%
  group_by(month = floor_date(date, "month")) %>%
  summarize(mean_temp = mean(temp, na.rm=T),
            mean_humid = mean(humid, na.rm=T),
            max_temp = max(temp, na.rm=T),
            max_humid = max(humid, na.rm=T),
            min_temp = min(temp, na.rm=T),
            min_humid = min(humid, na.rm=T)) %>% 
  filter(month < as.Date("2020-05-15"))


df <- data_sum %>%
  select(month, mean_humid, mean_temp) %>%
  gather (key="Medida", value="Temperatura (Cº) y Humedad (%)", -month)

df <- mutate(df, recode(Medida, mean_temp = "Temperatura promedia", mean_humid = "Humedad promedia"))

colnames(df) <- c("Fecha", "x", "Temperatura (Cº) y Humedad (%)", "Medida")



data_sum_year <- data_sum %>%
  mutate(month = months(month))
ggplot(data_sum_year, aes(x=month, y=mean_temp)) + 
  geom_boxplot()

data_sum_year <- data_sum %>%
  mutate(month = months(month))
ggplot(data_sum_year, aes(x=month, y=mean_humid)) + 
  geom_boxplot()

ggplot(data_sum, aes(x=mean_temp, y=mean_humid)) + 
  geom_point()


# set the plotting theme:
my_theme <- list(
  theme_classic() +
    theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"),
          strip.background = element_rect(color = NA),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 15, margin=ggplot2::margin(t=0.5, unit="cm")),
          axis.title.y = element_text(size = 15, margin=ggplot2::margin(r=0.5, unit="cm")),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          plot.title = element_text(size = 16),
          axis.line=element_line(),
          panel.spacing = unit(1, "lines")),
  scale_shape_manual(values=c(17,16))
)

quartz(w=7,h=5)
ggplot(data_sum, aes(x=month)) +
  theme_classic() +
  my_theme +
  geom_line(aes(y=mean_temp), col=2, size=1.2) +
  # geom_ribbon(aes(ymin=min_temp, ymax=max_temp), alpha=0.5, fill=2) +
  labs(x="Date", y="Mean monthly\nTemperature (C°)")



ggplot(data_sum_year, aes(x=month, y=`Temperatura (Cº) y Humedad (%)`)) + 
  geom_bocplot
  
quartz(w=7,h=5)
ggplot(data_sum, aes(x=month)) +
  theme_classic() +
  my_theme +
  geom_line(aes(y=mean_humid), col=4, size=1.2) +
  # geom_ribbon(aes(ymin=min_humid, ymax=max_humid), alpha=0.5, fill=4) +
  labs(x="Date", y="Mean monthly\nHumidity (%)")

quartz(w=5,h=4)
ggplot(data_sum) +
  theme_classic() +
  my_theme +
  geom_point(aes(y=mean_humid, x=mean_temp), col=6, size=2) +
  # geom_smooth(aes(x=mean_temp, y=mean_humid), method="lm") +
  labs(x="Mean Monthly Humidity", y="Mean Monthly Temperature")



# Gráfico temp +  hum promedias a lo largo de los años

quartz(w=5,h=4)
ggplot(df, aes(x=Fecha, y=`Temperatura (Cº) y Humedad (%)`)) +
  geom_line(aes(color=Medida, linetype=Medida)) +
  scale_color_manual(values=c("darkred", "blue")) +
labs(x="Date", y="Temperature (C°) and Humidity (%)")


?aggregate
head(pest_data)
#pest_data <- aggregate(Cantidad, by=list(Fecha.de.monitoreo, sum))

plot(pest_data$Individuos.monitoreo, pest_data$Temperatura.promedio)

#######
## Changes in pest numbers with temp/hum
## Changes in pest numbers with location (central, ampl, entrada)
## Changes in families with temp/hum increase
## Test for harmful pest families/species


pest_data$monitoreo=factor(pest_data$monitoreo,levels=unique(pest_data$monitoreo))
pest_data$trampa=factor(pest_data$trampa,levels=unique(pest_data$trampa))
pest_data$especie=factor(pest_data$especie,ordered=TRUE)
pest_data$abundancia=factor(pest_data$abundancia,ordered=TRUE)
View(pest_data)

#Análisis
#Modelo por bloques (monitoreo y trampas)
trampa1<-pest_data$Nro..De.Trampa
trampa1<-factor(trampa1)
(trampa1)
monitoreo1<-pest_data$Fecha.de.monitoreo
monitoreo1<-factor(monitoreo1)

#Bloques de referencia para la comparaci?n
trampa1<- relevel(trampa1, ref="1")
monitoreo1<-relevel(monitoreo1, ref="mayo-diciembre/2017")
#Modelo lineal_comparaci?n de especies entre trampas y monitoreo
model1<-lm(pest_data$especie~trampa1)
model2<-lm(pest_data$especie~monitoreo1)
summary(model1)
summary(model2)

#Modelo lineal - comparación de abundancia entre trampas y monitoreo
model3<-lm(abundancia~trampa1)
model4<-lm(abundancia~monitoreo1)
summary(model3)
summary(model4)

#GRÁFICOS
par(mar=c(4.3, 4.1, 0.8, 2.1),mfrow=c(2,2))
boxplot(especie~monitoreo,xlab="Monitoreos", ylab="Nº de especies", data=pest_data)
boxplot(pest_data$especie~trampa,xlab="Trampas", ylab="Nº de especies", data=pest_data)
boxplot(abundancia~monitoreo,xlab="Monitoreos", ylab="Abundancia", data=pest_data)
boxplot(abundancia~trampa, xlab="Trampas", ylab="Abundancia", data=pest_data)

