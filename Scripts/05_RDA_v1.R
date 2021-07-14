# ================ README ===============

# Creamos dos tipos de analisis. Un analisis por trampas y otro por monitoreos
# El analisis por trampas muestra más variación que el de monitoreos por la diferencia
# de número de muestras.Entonces para el análisis usaremos los resultados por trampas.
# La primera seccion incluye preparar los datos para los RDAs.
# La segunda seccion

# Seccion 1: PREPARACION DE DATOS ####

# ==== POR TRAMPA =======
# Convertimos los datos de abundancia de especies a una matriz de presencia
# y ausencia por especies. Donde cada columna es una especie y esta dividido por
# el ID de las trampas.

# Creamos y filtramos las matrices por especies de insectos y por orden
# de insectos.

## 1.1 Abundancia especies ####
species <- read_csv("~/R/03. Collaborations/01. CDRS Herbarium insects_Jaramillo/Insect-data-Herbarium-CDRS/Data/Processed/Matrix especies.csv")

# Filtramos las columnas para seleccionar las especies
species <- species[-c(50:57)]

# Cambiamos los nombres de las filas por el ID de las trampas.
species <- species %>% column_to_rownames("trampa_ID_unico")

# Transformar la matriz de especies en hellinger
species_hel <- decostand(species, "hellinger") # Trap species transformed

## 1.2 Abundancia Orden ####
orden <- read_csv("~/R/03. Collaborations/01. CDRS Herbarium insects_Jaramillo/Insect-data-Herbarium-CDRS/Data/Processed/Matrix orden.csv")

# Change the rows ID with the traps IDs
orden <- orden %>% column_to_rownames("trampa_ID_unico")

# Transformar la matriz de orden en hellinger
orden_hel <- decostand(orden, "hellinger")

## 1.3 Datos ambientales y experimentales ####
env <- read_csv("~/R/03. Collaborations/01. CDRS Herbarium insects_Jaramillo/Insect-data-Herbarium-CDRS/Data/Processed/Matrix temperatura humedad.csv")

# Cambiamos el nombre de las filas por el nombre de las trampas
env <- env %>% column_to_rownames("trampa_ID_unico")

## 1.4 Creamos un subset de parametros ambientales y experimentales ####

env1 <- dplyr::select(env,
               temp_mean,
               temp_max,
               temp_min,
               hum_mean,
               hum_max,
               hum_min,
               ubicacion,
               year
               )

# Monitoreo, tipo de trampa y ubicacion convertidos en factores
env1 <- env1 %>% mutate_at(vars(ubicacion,
                                # marca_trampa
                                ), list(factor))

### 1.4.1 Matriz ambiental ####
# Separamos una matriz de Temperatura y humedad.
# Temperatura media, maxima y minima. Humedad media, maxima y minima.

tem_hum <- dplyr::select(env,
                  temp_mean,
                  temp_max,
                  temp_min,
                  hum_mean,
                  hum_max,
                  hum_min)

### 1.4.2 Matriz experimental #### 
# Los parametros experimentales incluyen:
# La ubicacion de las trampas, y el año de las trampas.

experimental_var <- dplyr::select(env,
                   year,
                   ubicacion,
                   )

experimental_var <- experimental_var %>% mutate_at(vars(ubicacion), list(factor))


# POR MONITOREO ####

# Este es un subset de datos agrupados por monitoreos, en total se realizaron 7
# monitoreos 

## 1. Cargar la matriz general por monitoreo ####
monitoreo <- read_csv("~/R/03. Collaborations/01. CDRS Herbarium insects_Jaramillo/Insect-data-Herbarium-CDRS/Data/Processed/Matrix abundancia orden por monitoreo.csv")

## 2. Crear matriz por especies por monitoreo ####
monitoreo_abundance <- monitoreo[-c(14:23)]
monitoreo_abundance <- monitoreo_abundance %>% column_to_rownames("monitoreo")

# Transformacion hellinger
monitoreo_hel <- decostand(monitoreo_abundance, "hellinger")

## 3. Matriz ambiental por monitoreo ####
# Esta matriz ambiental incluye temperatura y humedad max y min
monitoreo_env <- monitoreo[-c(1:13)]
monitoreo_env <- monitoreo_env %>% column_to_rownames("monitoreo")


# Seccion 2: RDAs ####
# RDAs con todas las variables ambientales y experimentales

##  2.1 RDA de especies ####
(spe_rda <- rda(species_hel ~ ., env1)) # Observe the shortcut formula
                                        # env1 incluye los datos ambientales y
                                        # experimentales.

summary(spe_rda)	# Scaling 2 (default)

# Canonical coefficients from the rda object
# Regression coefficients for each explanatory variable
reg_coefficients_species <- coef(spe_rda)

# The R2 of a RDA is biased. An adjusted R2 near 0 indicates that
# X does not explain more of the variation of Y than random normal
# deviates would do. Adjusted R2 values can be negative, indicating
# that the explanatory variables X do worse than a set of m random normal
# deviates would. (From numerical ecology. Borcard, 2018)


# Unadjusted R^2 retrieved from the rda object
(R2_species <- RsquareAdj(spe_rda)$r.squared) # 0.3170584
# Adjusted R^2 retrieved from the rda object
# The adjusted R2 measures the unbiased amount of explained variation and
# will be used later for variation partitioning
(R2adj_species <- RsquareAdj(spe_rda)$adj.r.squared) # 0.1685928

#### 2.1.1 Plot RDA de especies 
# Species Triplot.rda function (Custom function from the book)
# Load the function

source("triplot_RDA_Borcard2018.R")

# Triplots with homemade function triplot.rda(), scalings 1 and 2
# This function can improve the look of the triplots

# Filter species under a certain goodness of fit threshold 
# to visually see the species more responsive to the environmental variables
spe_good <- goodness(spe_rda)
sel_sp <- which(spe_good[, 2] >= 0.2)
# sel_sp number is arbritrary you can adjust it to include all the species but
# that is visually difficult.

# I run this code to color the sites. But is limited since I cannot include the 
# vectors yet
scl <- 3
colvec <- c("red2", "green4", "mediumblue", "black")
plot(spe_rda, type = "n", scaling = scl)
with(env1, points(spe_rda, display = "wa", col = colvec[env1$ubicacion],
                  scaling = scl, pch = 21, bg = colvec[env1$ubicacion]))
text(spe_rda, display = "species", scaling = scl, cex = 0.8, col = "darkcyan", select = sel_sp)
with(env1, legend("bottomright", legend = levels(env1$ubicacion), bty = "n",
                  col = colvec, pch = 21, pt.bg = colvec))

# Scaling 1:
as <- triplot.rda(spe_rda,
            site.sc = "wa",
            scaling = 1,
            cex.char2 = 0.7,
            cex.char1 = 0.7,
            pos.env = 3,
            pos.centr = 2,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            select.spe = sel_sp,
            plot.sites = T,
            arrows.only = F,
            label.sites = F,
            label.centr = F,
            plot.spe = T,
            plot.env = T,
            plot.centr = T,
            cex.point = 0
)



# Scaling 2:
triplot.rda(spe_rda,
            site.sc = "lc",
            scaling = 2,
            cex.char2 = 0.7,
            pos.env = 3,
            pos.centr = 1,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            select.spe = sel_sp,
            plot.sites = T,
            arrows.only = F,
            label.sites = F,
            label.centr = F
)


#### 3.1 Species Global test of the RDA result ####
anova(spe_rda, permutations = how(nperm = 999))
# Tests of all canonical axes
anova(spe_rda, by = "terms", permutations = how(nperm = 999))
anova(spe_rda, by = "axis", permutations = how(nperm = 999))
# Apply Kaiser-Guttman criterion to residual axes
spe_rda$CA$eig[spe_rda$CA$eig > mean(spe_rda$CA$eig)]



##### 3.2 RDA de orden ####
(orden_rda <- rda(orden_hel ~ ., env1)) # Observe the shortcut formula
summary(orden_rda)	# Scaling 2 (default)

# Canonical coefficients from the rda object
reg_coef_orden_rda <- coef(orden_rda)
as_tibble(reg_coef_orden_rda)

# Unadjusted R^2 retrieved from the rda object
(R2_orden <- RsquareAdj(orden_rda)$r.squared) # 0.3341348
# Adjusted R^2 retrieved from the rda object
(R2adj_orden <- RsquareAdj(orden_rda)$adj.r.squared) # 0.1713678

orden_good <- goodness(orden_rda)
sel_orden <- which(orden_good[, 2] >= 0.2)

#### 3.2 Plot orden RDA ####
## Triplots of the rda results (lc scores)

triplot.rda(orden_rda,
            site.sc = "lc",
            scaling = 1,
            cex.char2 = 0.7,
            pos.env = 3,
            pos.centr = 1,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            # select.spe = sel_orden,
            plot.sites = T,
            arrows.only = F,
            label.sites = F,
            label.centr = F
)
triplot.rda(orden_rda,
            site.sc = "lc",
            scaling = 2,
            cex.char2 = 0.7,
            pos.env = 3,
            pos.centr = 1,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            # select.spe = sel_orden,
            plot.sites = T,
            arrows.only = F,
            label.sites = F,
            label.centr = F
)

# ## Site scores as linear combinations of the environmental variables
# dev.new(
#   title = "RDA scaling 1 and 2 + lc",
#   width = 16,
#   height = 8,
#   noRStudioGD = TRUE
# )
# par(mfrow = c(1, 2))
# plot(orden_rda,
#      scaling = 1,
#      display = c("sp", "lc", "cn"),
#      main = "Triplot RDA spe.hel ~ env3 - scaling 1 - lc scores"
# )
# spe.sc1_orden <- 
#   scores(orden_rda, 
#          choices = 1:2, 
#          scaling = 1, 
#          display = "sp"
#   )
# arrows(0, 0, 
#        spe.sc1[, 1] * 0.92,
#        spe.sc1[, 2] * 0.92,
#        length = 0, 
#        lty = 1, 
#        col = "red"
# )
# # text(-0.75, 0.7, "a", cex = 1.5)
# 
# # Scaling 2
# plot(orden_rda, 
#      display = c("sp", "lc", "cn"), 
#      main = "Triplot RDA spe.hel ~ env3 - scaling 2 - lc scores"
# )
# spe.sc2_orden <- 
#   scores(orden_rda, 
#          choices = 1:2, 
#          display = "sp"
#   )
# arrows(0, 0, 
#        spe.sc2[, 1] * 0.92, 
#        spe.sc2[, 2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# text(-0.82, 0.55, "b", cex = 1.5)

## Triplots of the rda results (wa scores)
## Site scores as weighted averages (vegan's default)
# Scaling 1 :  distance triplot
# dev.new(title = "RDA scaling 1 + wa", noRStudioGD = TRUE)
# plot(orden_rda, 
#      scaling = 1, 
#      main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores"
# )
# arrows(0, 0, 
#        spe.sc1[, 1] * 0.92, 
#        spe.sc1[, 2] * 0.92, 
#        length = 0, 
#        lty = 1, 
#        col = "red"
# )

# Select species with goodness-of-fit at least 0.6 in the 
# ordination plane formed by axes 1 and 2
orden_good <- goodness(orden_rda)
sel_orden <- which(orden_good[, 2] >= 0.6)

# Global test of the RDA result
anova(orden_rda, permutations = how(nperm = 999))
# Tests of all canonical axes
anova(orden_rda, by = "terms", permutations = how(nperm = 999))
anova(orden_rda, by = "axis", permutations = how(nperm = 999))

#### 3.3 RDA de monitoreo y orden de especies ####
(monitoreo_rda <- rda(monitoreo_hel ~ ., monitoreo_env)) # Observe the shortcut formula
summary(monitoreo_rda)	# Scaling 2 (default)

# Canonical coefficients from the rda object
coef_monitoreo_rda <- coef(monitoreo_rda)
as_tibble(coef_monitoreo_rda)

# Unadjusted R^2 retrieved from the rda object
(R2_monitoreo <- RsquareAdj(monitoreo_rda)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj_monitoreo <- RsquareAdj(monitoreo_rda)$adj.r.squared)

# ## 3.3 Triplots of the rda results (lc scores) ###
# ## Site scores as linear combinations of the environmental variables
# dev.new(
#   title = "RDA scaling 1 and 2 + lc",
#   width = 16,
#   height = 8,
#   noRStudioGD = TRUE
# )
# par(mfrow = c(1, 2))
# plot(monitoreo_rda,
#      scaling = 1,
#      display = c("sp", "lc", "cn"),
#      main = "Triplot RDA spe.hel ~ env3 - scaling 1 - lc scores"
# )
# spe.sc1_monitoreo <- 
#   scores(monitoreo_rda, 
#          choices = 1:2, 
#          scaling = 1, 
#          display = "sp"
#   )
# arrows(0, 0, 
#        spe.sc2[, 1] * 0.92, 
#        spe.sc2[, 2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# # text(-0.75, 0.7, "a", cex = 1.5)
# 
# # Scaling 2
# plot(monitoreo_rda, 
#      display = c("sp", "lc", "cn"), 
#      main = "Triplot RDA spe.hel ~ env3 - scaling 2 - lc scores"
# )
# spe.sc2_orden <- 
#   scores(monitoreo_rda, 
#          choices = 1:2, 
#          display = "sp"
#   )
# arrows(0, 0, 
#        spe.sc2[, 1] * 0.92, 
#        spe.sc2[, 2] * 0.92,
#        length = 0,
#        lty = 1,
#        col = "red"
# )
# # text(-0.82, 0.55, "b", cex = 1.5)
# 
# ## Triplots of the rda results (wa scores)
# ## Site scores as weighted averages (vegan's default)
# # Scaling 1 :  distance triplot
# dev.new(title = "RDA scaling 1 + wa", noRStudioGD = TRUE)
# plot(monitoreo_rda, 
#      scaling = 1, 
#      main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores"
# )
# arrows(0, 0, 
#        spe.sc1[, 1] * 0.92, 
#        spe.sc1[, 2] * 0.92, 
#        length = 0, 
#        lty = 1, 
#        col = "red"
# )

# Select species with goodness-of-fit at least 0.6 in the 
# ordination plane formed by axes 1 and 2
monitoreo_good <- goodness(monitoreo_rda)
monitoreo_sp <- which(monitoreo_good[, 2] >= 0.6)

# Global test of the RDA result
anova(monitoreo_rda, permutations = how(nperm = 999))
# Tests of all canonical axes
anova(monitoreo_rda, by = "terms", permutations = how(nperm = 999))
anova(monitoreo_rda, by = "axis", permutations = how(nperm = 999))

# # RDA parcial Species ##
# 
# # RDA with formula interface; the X and W variables are in the same
# # data frame. Excludes the conditional variance
# (spe_trap <- rda(species_hel ~ temp_mean
#                  + temp_max
#                  + temp_min
#                  + hum_mean
#                  + hum_max
#                  + hum_min
#                  + Condition(year
#                              + ubicacion
#                              # + marca_trampa
#                              ), data = env1))
# summary(spe_trap)
# 
# # Global test of the RDA result
# anova(spe_trap, permutations = how(nperm = 999))
# # Tests of all canonical axes
# anova(spe_trap, by = "axis", permutations = how(nperm = 999))
# anova(spe_trap, by = "terms", permutations = how(nperm = 999))
# 
# ### Partial RDA triplots (with fitted site scores) ###
# # Scaling 1
# triplot.rda(spe_trap,
#             site.sc = "lc",
#             scaling = 1,
#             cex.char2 = 0.8,
#             pos.env = 3,
#             mar.percent = 0
# )
# # Scaling 2
# triplot.rda(spe_trap,
#             site.sc = "lc",
#             scaling = 2,
#             cex.char2 = 0.8,
#             pos.env = 3,
#             mult.spe = 1.1,
#             mar.percent = 0.04
# )
# 
# # Global adjusted R^2
# (R2a_all <- RsquareAdj(spe_trap)$adj.r.squared)

#### 4) Variance inflation factors (VIF) in two RDAs ####
# First RDA all variables
vif.cca(spe_rda)
vif.cca(orden_rda)



# # Partial RDA – environmental variables only
# vif.cca(spe_trap)

#### 4.1 Reduction of variables ####
# Forward selection using vegan's ordistep()
# Species.
mod_0 <- rda(species_hel ~ 1, data = env1)
step.forward <- ordistep(mod_0,
                         scope = formula(spe_rda),
                         direction = "forward",
                         permutations = how(nperm = 499))
RsquareAdj(step.forward)

# # Orden
# mod_1 <- rda(orden_hel ~ 1, data = env1)
# step.forward.orden <- ordistep(mod_1,
#                          scope = formula(orden_rda),
#                          direction = "forward",
#                          permutations = how(nperm = 499))
# RsquareAdj(step.forward.orden)


# Forward selection using ordiR2step
step2.forward <-
  ordiR2step(mod_0,
             scope = formula(spe_rda),
             direction = "forward",
             R2scope = TRUE,
             permutations = how(nperm = 199)
  )

# step2.forward.orden <-
#   ordiR2step(mod_1,
#              scope = formula(orden_rda),
#              direction = "forward",
#              R2scope = TRUE,
#              permutations = how(nperm = 199)
#   )



# Basados en estos analis de seleccion
spe_rda_partial <- rda(species_hel ~ hum_max
                    + temp_min
                    + ubicacion, data = env1)
# orden_rda_partial <- rda(orden_hel ~ hum_max
#                        + temp_min
#                        + ubicacion, data = env1)

anova(spe_rda_partial, permutations = how(nperm = 999))
anova(spe_rda_partial, permutations = how(nperm = 999), by = "axis")
anova(spe_rda_partial, permutations = how(nperm = 999), by = "term")

# anova(orden_rda_partial, permutations = how(nperm = 999))
# anova(orden_rda_partial, permutations = how(nperm = 999), by = "axis")
# anova(orden_rda_partial, permutations = how(nperm = 999), by = "term")

(R2a.pars <- RsquareAdj(spe_rda_partial)$adj.r.squared)
# (R2a.pars.orden <- RsquareAdj(orden_rda_partial)$adj.r.squared)


# Compare the variance inflation factors
vif.cca(spe_rda)
vif.cca(spe_rda_partial)

# vif.cca(orden_rda)
# vif.cca(orden_rda_partial)


# Species
triplot.rda(spe_rda_partial,
            site.sc = "lc",
            scaling = 1,
            cex.char2 = 0.7,
            pos.env = 3,
            pos.centr = 1,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            select.spe = sel_sp,
            plot.sites = T,
            arrows.only = F,
            label.sites = F,
            label.centr = F
)
triplot.rda(spe_rda_partial,
            site.sc = "lc",
            scaling = 2,
            cex.char2 = 0.7,
            pos.env = 3,
            pos.centr = 1,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            select.spe = sel_sp,
            plot.sites = T,
            arrows.only = F,
            label.sites = F,
            label.centr = F
)

### 4.2 Variation partitioning species ####

### 4.2.1. Variation parititioning with all explanatory variables ####
# Species variance
(spe.part.all <- varpart(species_hel, tem_hum, experimental_var))

# Orden variance
orden.part.all <- varpart(orden_hel, tem_hum, experimental_var)

# Monitoreo variance
# monitoreo.part.all <- varpart(monitoreo_hel, monitoreo_env)

#### Plot of the partitioning results ####
dev.new(title = "Variation partitioning - all variables", 
        noRStudioGD = TRUE)

# Species
plot(spe.part.all, 
     digits = 2, 
     bg = c("red", "blue"),
     Xnames = c("Environmental", "Experimental"),
     id.size = 1
     )

# Orden
plot(orden.part.all, 
     digits = 2, 
     bg = c("red", "blue"),
     Xnames = c("Environmental", "Experimental"),
     id.size = 1
     )

### 4.2.2. Variation partitioning after forward selection of explanatory ####
### variables

# Separate forward selection in each subset of environmental variables
species_env <- rda(species_hel, tem_hum)
R2a.all.env <- RsquareAdj(species_env)$adj.r.squared
forward.sel(species_hel,
            tem_hum,
            adjR2thresh = R2a.all.env,
            nperm = 9999
            )

# species_exp <- rda(species_hel, experimental_var)
# R2a.all.exp <- RsquareAdj(species_exp)$adj.r.squared
# forward.sel(species_hel,
#             experimental_var,
#             adjR2thresh = R2a.all.exp,
#             nperm = 9999
# )

# Parsimonious subsets of explanatory variables, based on forward selections
names(tem_hum)
tem_hum_partial <- tem_hum[,c(5)]

# Variation partitioning of partial variables ####
species_partial <- varpart(species_hel, tem_hum_partial, experimental_var)
plot(species_partial,
     digits = 2,
     bg = c("red", "blue"),
     Xnames = c("Environmental", "Experimental"),
     id.size = 1
     )

# Tests of all testable fractions
# Test of fraction [a+b]
anova(rda(species_hel, tem_hum_partial), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(species_hel, experimental_var), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
env.pars <- cbind(tem_hum_partial, experimental_var)
anova(rda(species_hel, env.pars), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(species_hel, tem_hum_partial, experimental_var), 
      permutations = how(nperm = 999)
)
# Test of fraction [c]
anova(rda(species_hel, experimental_var, tem_hum_partial), 
      permutations = how(nperm = 999)
)

