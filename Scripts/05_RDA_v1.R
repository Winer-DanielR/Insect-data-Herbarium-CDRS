
# Insect matrix load
insects <- read_csv("Data/Processed/Matrix especies.csv")
names(insects)

# Orden matrix load
orden <- read_csv("Data/Processed/Matrix orden.csv")
names(orden)
orden <- orden %>% column_to_rownames("trampa_ID_unico")
# Transformar la matriz de orden en hellinger
orden_hel <- decostand(orden, "hellinger")



##### For insects dataset ####
# Filter other columns and only use the species
insects <- insects[-c(50:57)]
# Change the rows ID with the traps IDs
insects <- insects %>% column_to_rownames("trampa_ID_unico")


# Environmental dataset load
env <- read_csv("Data/Processed/Matrix temperatura humedad.csv")
names(env)
env <- env %>% column_to_rownames("trampa_ID_unico")
# Subset environmental variables
env1 <- select(env,
               temp_mean,
               hum_mean,
               monitoreo,
               tipo_trampa,
               ubicacion)

# Monitoreo, tipo de trampa y ubicacion convertidos en factores
env1 <- env1 %>% mutate_at(vars(monitoreo,
                                tipo_trampa,
                                ubicacion), list(factor))

# Transformar la matriz de especies en hellinger
spe_hel <- decostand(insects, "hellinger")

# Redundancy analysis (RDA) =======================================

## RDA of the Hellinger-transformed fish species data, constrained
## by all the environmental variables contained in env3 plus interaction of temp and hum


##### RDA de especies ####
(spe_rda <- rda(spe_hel ~ temp_mean + 
                  hum_mean + 
                  temp_mean*hum_mean +
                  monitoreo +
                  tipo_trampa +
                  ubicacion, env1)) # Observe the shortcut formula
summary(spe_rda)	# Scaling 2 (default)

# Canonical coefficients from the rda object
#coef_rda <- coef(spe_rda)
#coef_rda <- coef_rda[-8, ] #Remove NA rows. Do the lines one at a time.
#coef_rda <- coef_rda[-8, ]
#coef_rda <- coef_rda[-9, ]
#coef_rda1 <- coef_rda

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(spe_rda)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(spe_rda)$adj.r.squared)

## Triplots of the rda results (lc scores)
## Site scores as linear combinations of the environmental variables
dev.new(
  title = "RDA scaling 1 and 2 + lc",
  width = 16,
  height = 8,
  noRStudioGD = TRUE
)
# dev.new(
#   title = "RDA scaling 1 and 2 + lc",
#   width = 6,
#   height = 12,
#   noRStudioGD = TRUE
# )
par(mfrow = c(1, 2))
# par(mfrow = c(2, 1))
plot(spe_rda,
     scaling = 1,
     display = c("sp", "lc", "cn"),
     main = "Triplot RDA spe.hel ~ env3 - scaling 1 - lc scores"
)
spe.sc1 <- 
  scores(spe_rda, 
         choices = 1:2, 
         scaling = 1, 
         display = "sp"
  )
arrows(0, 0, 
       spe.sc1[, 1] * 0.92,
       spe.sc1[, 2] * 0.92,
       length = 0, 
       lty = 1, 
       col = "red"
)
# text(-0.75, 0.7, "a", cex = 1.5)

# Scaling 2
plot(spe_rda, 
     display = c("sp", "lc", "cn"), 
     main = "Triplot RDA spe.hel ~ env3 - scaling 2 - lc scores"
)
spe.sc2 <- 
  scores(spe_rda, 
         choices = 1:2, 
         display = "sp"
  )
arrows(0, 0, 
       spe.sc2[, 1] * 0.92, 
       spe.sc2[, 2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)
# text(-0.82, 0.55, "b", cex = 1.5)

## Triplots of the rda results (wa scores)
## Site scores as weighted averages (vegan's default)
# Scaling 1 :  distance triplot
dev.new(title = "RDA scaling 1 + wa", noRStudioGD = TRUE)
plot(spe_rda, 
     scaling = 1, 
     main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores"
)
arrows(0, 0, 
       spe.sc1[, 1] * 0.92, 
       spe.sc1[, 2] * 0.92, 
       length = 0, 
       lty = 1, 
       col = "red"
)

# Select species with goodness-of-fit at least 0.6 in the 
# ordination plane formed by axes 1 and 2
spe_good <- goodness(spe_rda)
sel_sp <- which(spe_good[, 2] >= 0.6)

# Global test of the RDA result
anova(spe_rda, permutations = how(nperm = 999))
# Tests of all canonical axes
anova(spe_rda, by = "terms", permutations = how(nperm = 999))

##### RDA de orden ####

(orden_rda <- rda(orden_hel ~ temp_mean + 
                  hum_mean + 
                  temp_mean*hum_mean
                  #monitoreo +
                  #tipo_trampa +
                  #ubicacion
                    , env1)) # Observe the shortcut formula
summary(orden_rda)	# Scaling 2 (default)

# Canonical coefficients from the rda object
coef_orden_rda <- coef(orden_rda)
as_data_frame(coef_orden_rda)

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(orden_rda)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(orden_rda)$adj.r.squared)

## Triplots of the rda results (lc scores)
## Site scores as linear combinations of the environmental variables
dev.new(
  title = "RDA scaling 1 and 2 + lc",
  width = 16,
  height = 8,
  noRStudioGD = TRUE
)
# dev.new(
#   title = "RDA scaling 1 and 2 + lc",
#   width = 6,
#   height = 12,
#   noRStudioGD = TRUE
# )
par(mfrow = c(1, 2))
# par(mfrow = c(2, 1))
plot(orden_rda,
     scaling = 1,
     display = c("sp", "lc", "cn"),
     main = "Triplot RDA spe.hel ~ env3 - scaling 1 - lc scores"
)
spe.sc1 <- 
  scores(orden_rda, 
         choices = 1:2, 
         scaling = 1, 
         display = "sp"
  )
arrows(0, 0, 
       spe.sc1[, 1] * 0.92,
       spe.sc1[, 2] * 0.92,
       length = 0, 
       lty = 1, 
       col = "red"
)
# text(-0.75, 0.7, "a", cex = 1.5)

# Scaling 2
plot(orden_rda, 
     display = c("sp", "lc", "cn"), 
     main = "Triplot RDA spe.hel ~ env3 - scaling 2 - lc scores"
)
spe.sc2 <- 
  scores(orden_rda, 
         choices = 1:2, 
         display = "sp"
  )
arrows(0, 0, 
       spe.sc2[, 1] * 0.92, 
       spe.sc2[, 2] * 0.92,
       length = 0,
       lty = 1,
       col = "red"
)
# text(-0.82, 0.55, "b", cex = 1.5)

## Triplots of the rda results (wa scores)
## Site scores as weighted averages (vegan's default)
# Scaling 1 :  distance triplot
dev.new(title = "RDA scaling 1 + wa", noRStudioGD = TRUE)
plot(spe_rda, 
     scaling = 1, 
     main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores"
)
arrows(0, 0, 
       spe.sc1[, 1] * 0.92, 
       spe.sc1[, 2] * 0.92, 
       length = 0, 
       lty = 1, 
       col = "red"
)

# Select species with goodness-of-fit at least 0.6 in the 
# ordination plane formed by axes 1 and 2
spe_good <- goodness(spe_rda)
sel_sp <- which(spe_good[, 2] >= 0.6)

# Global test of the RDA result
anova(orden_rda, permutations = how(nperm = 999))
# Tests of all canonical axes
anova(orden_rda, by = "terms", permutations = how(nperm = 999))


