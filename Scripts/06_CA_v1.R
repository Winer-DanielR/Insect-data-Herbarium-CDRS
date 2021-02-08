# Correspondence analysis (CA) ===========================================
# Code for this analysis is based on Numerical Ecology with R. Borcard et. al, 2018
## CA of the raw species dataset (species abundance per trap)

# Import dataset
spe_matrix <- read_csv("Data/Processed/Matrix especies.csv")
env_matrix <- read_csv("Data/Processed/Matrix temperatura humedad.csv")
insects <- spe_matrix[-c(1,50:57)]
insect_log <- log(insects + 1) # Log transformed data for heat map

# Compute CA
insects_ca <- cca(insects)
summary(insects_ca) # Default scaling 2 where columns (species) are
                    # at the centroids of rows (traps). 
                    # Useful for ordination of species.

summary(insects_ca, scaling = 1) # Rows (traps) are at the centroid of 
                    # columns (species). This is scaling is the most
                    # appropriate if one is insterested in the ordination
                    # of objects.

# Scree plots and broken stick model using vegan's screeplot.cca()
dev.new(tittle = "Scree plot of CA eigenvalues", noRStudioGD = TRUE)
screeplot(insects_ca, bstick = TRUE, npcs = length(insects_ca$CA$eig))

# CA biplots
dev.new(title = "CA biplots", width = 14, heigth = 7, noRStudioGD = TRUE)
par(mfrow = c(1,1))
# Scaling 1: sites are centroids of species
plot(insects_ca, scaling = 1, 
     main = "CA insect abundances - biplot scaling 1",
     type = "p")

# Scaling 2 (default): species are centroids of sites
plot(insects_ca, main = "CA fish abundances - biplot scaling 2")

# Curve fitting in a CA biplot
dev.new(title = "CA biplot with environmental curves",
        noRStudioGD = TRUE)
plot(insects_ca, main = "CA insect abundance - scaling 2",
     sub = "Fitted curves: temperature (red), humidity (green)")
insect_ca_env <- envfit(insects_ca ~ temp_mean + hum_mean, env_matrix)
plot(insect_ca_env) # Two arrows
ordisurf(insects_ca, env_matrix$temp_mean, add = TRUE, col = "red")
ordisurf(insects_ca, env_matrix$hum_mean, add = TRUE, col = "green")



# Species data table ordered after CA results
vegemite(insect_log, insects_ca, scale = "Hill")
# CA - ordered species table illustrated as a heat map
dev.new(title = "CA - ordered species table - heat map",
        noRStudioGD = TRUE)
tabasco(insect_log, insects_ca)
