################################
####
####  B. Corrected RDA Script 
####
################################



library(dplyr)
library(vegan)
library(ggplot2)
library(xlsx)
library(tidyverse)
library(magrittr)
library(ggpubr)
library(ade4)
library(gclus)
library(ape)
library(missMDA)
library(FactoMineR)


##############################
# Hellinger transformation:


# Hellinger transformation (or other transformations) are needed in some instances (you need to check if it is your case)
# For species ? (Check)

df1.nn.tr<- decostand(df1.nn, method = "hellinger")


df1.nn.tr.pca <- rda(df1.nn.tr, scale = TRUE, main = "PCA species hellinger")
df1.nn.tr.pca

par(mfrow = c(1, 2))
biplot(df1.nn.tr.pca, scaling = 1, main = "PCA species Main Plantation  Hellinger - scaling 1")
screeplot(df1.nn.tr.pca, bstick = TRUE, npcs = length(df1.nn.tr.pca$CA$eig))



####################
#  RDA:

rda_TreeDiversity_df1.nn.tr.pca <- rda(df1.nn.tr, TreeDiversity_Binary)
summary(rda_TreeDiversity_df1.nn.tr.pca)

plot(rda_TreeDiversity_df1.nn.tr.pca,
     scaling = 1,
     main = "RDA Butterfly Evening Families  ~ 
     Tree Diversity at Plot Level - scaling 1")

(spe.rda <- rda(df1.nn.tr ~ ., TreeDiversity_Binary))
summary(spe.rda)



# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rda_TreeDiversity_df1.nn.tr.pca)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rda_TreeDiversity_df1.nn.tr.pca)$adj.r.squared)


# Select species with goodness-of-fit at least 0.6 in the
# ordination plane formed by axes 1 and 2
spe.good <- goodness(spe.rda)
sel.sp <- which(spe.good[, 2] >= 0.6)
# Triplots with homemade function triplot.rda(), scalings 1 and 2
triplot.rda(rda_TreeDiversity_df1.nn.tr.pca,
            site.sc = "lc",
            scaling = 1,
            cex.char2 = 0.7,
            pos.env = 3,
            pos.centr = 1,
            mult.arrow = 1.1,
            mar.percent = 0.05,
            select.spe = sel.sp
)


## Global test of the RDA result
anova(rda_TreeDiversity_df1.nn.tr.pca, permutations = how(nperm = 999))
## Tests of all canonical axes
anova(rda_TreeDiversity_df1.nn.tr.pca, by = "axis", permutations = how(nperm = 999))

# Apply Kaiser-Guttman criterion to residual axes
rda_TreeDiversity_df1.nn.tr.pca$CA$eig[rda_TreeDiversity_df1.nn.tr.pca$CA$eig > mean(rda_TreeDiversity_df1.nn.tr.pca$CA$eig)]





############################################
#### Forward and backward selection : 


rdaenv_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, Env)
rdaenv_PlotGenusPMMP = rda(Plot_Genus_PM.nn.tr, Env)


summary(rdaenv_PlotGenusAMMP)

plot(rdaenv_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Environmental Data at the Plot Level - scaling 1")


# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaenv_PlotGenusAMMP)$r.squared) # 1 makes sense
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaenv_PlotGenusAMMP)$adj.r.squared)  # NA, makes sense and shows need to eliminate some variables


# Forward selection using forward.sel() {adespatial}
# Global adjusted R^2
(R2a.all <- RsquareAdj(rdaenv_PlotGenusAMMP)$adj.r.squared)
forward.sel(Plot_Genus_AM.nn.tr, Env, adjR2thresh = R2a.all)



# Backward elimination using vegan's ordistep()
mod1 <- rda(Plot_Genus_AM.nn.tr ~ ., Env)
mod0 <- rda(Plot_Genus_AM.nn.tr ~ 1, Env)


#Backwards
BackwardenvAMMP = ordistep(mod1, perm.max = 200) 
summary(BackwardenvAMMP)


#Df      AIC      F Pr(>F)   
#- Moisture_Percent  1 -11.7520 1.6005  0.045 * 
# - `Soil pH`         1 -11.3862 1.7659  0.040 * 
#- Percent_N         1 -11.0660 1.9135  0.020 * 
#- Percent_C         1 -11.0620 1.9154  0.020 * 
#- `Water variance`  1 -10.7978 2.0393  0.020 * 
#- `Slope type`      1 -10.4844 2.1890  0.020 * 
#- `Ditch Presence`  1 -10.2803 2.2879  0.015 * 
#- `Wet RR50`        1 -10.7023 2.0847  0.010 **
#- `Dry RR50`        1 -10.2912 2.2825  0.010 **
#- `Ditch lengths`   1  -9.9565 2.4472  0.005 **



#Forwards
ForwardenvAMMP = ordistep(mod0, scope = formula(mod1), direction="forward", perm.max = 200)
summary(ForwardenvAMMP) 
#Step: Plot_Genus_AM.nn.tr ~ Moisture_Percent


# Both
BothenvAMMP = ordistep(mod0, scope = formula(mod1), perm.max = 200)

#- Moisture_Percent  1 -15.909 3.1021  0.005 **
#+ Percent_N                  1 -16.781 1.4509  0.080 .


#-----------------

# Due to large number of variables, used selection methodes and will
# do RDA on these selected Env variables:

EnvSelected = Env[, c(11, 18)] 

rdaenvSelected_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, EnvSelected)

summary(rdaenvSelected_PlotGenusAMMP)

plot(rdaenvSelected_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Moisture and Nitrogen Percent at the Plot Level - scaling 1")


# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaenvSelected_PlotGenusAMMP)$r.squared) # 0.2491796
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaenvSelected_PlotGenusAMMP)$adj.r.squared)  # 0.1419195


## Global test of the RDA result
anova(rdaenvSelected_PlotGenusAMMP, permutations = how(nperm = 999)) #0.001 ***
## Tests of all canonical axes
anova(rdaenvSelected_PlotGenusAMMP, by = "axis", permutations = how(nperm = 999))

# Apply Kaiser-Guttman criterion to residual axes
rdaenvSelected_PlotGenusAMMP$CA$eig[rdaenvSelected_PlotGenusAMMP$CA$eig > mean(rdaenvSelected_PlotGenusAMMP$CA$eig)]



######### So, Global test significant for moisture evening species. 

########################################


# Variance inflation factors (VIF) in two RDAs
# First RDA of this Chapter: all environmental variables
# except dfs
vif.cca(rdaenv_PlotGenusAMMP)

vifGLA <- diag(solve(cor(GLA)))
vifGLA


# Struture:
rdaStructure_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, Structure)

summary(rdaStructure_PlotGenusAMMP)

plot(rdaStructure_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Structural Data at the Plot Level - scaling 1")

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaStructure_PlotGenusAMMP)$r.squared) # 1 makes sense
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaStructure_PlotGenusAMMP)$adj.r.squared)  # NA, makes sense and shows need to eliminate some variables



# Forward selection using forward.sel() {adespatial}
# Global adjusted R^2
(R2a.all <- RsquareAdj(rdaStructure_PlotGenusAMMP)$adj.r.squared)
forward.sel(Plot_Genus_AM.nn.tr, Structure, adjR2thresh = R2a.all)


# Forward select won't work bc too many variables, so use ordistep:

# Forward selection using vegan's ordistep()
# This function allows the use of factors.

# Backward elimination using vegan's ordistep()


mod1 <- rda(Plot_Genus_AM.nn.tr ~ ., Structure)
mod0 <- rda(Plot_Genus_AM.nn.tr ~ 1, Structure)

#Backwards
BackwardA = ordistep(mod1, perm.max = 200) 
summary(BackwardA)

#Forwards
ForwardA = ordistep(mod0, scope = formula(mod1), direction="forward", perm.max = 200)
summary(ForwardA) 


# Both
BothA = ordistep(mod0, scope = formula(mod1), perm.max = 200)

# Ordi R2 step:
OrdiR2A = ordiR2step(mod0, scope = formula(mod1), perm.max = 200)

# As we can see in the results, which are different depending on what we use, 
# the variables it gives are either: 1) still too correlated, 2) are
# not enough. This means that there are too many variables in the Structure
# matrix, and I will need to separate it by GLA and Florian matrixes, 
# to see what it gives for both and then put them together, then do
# it maybe again. 



# Doing it only with GLA:

# GLA:
rdaGLA_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, GLA)


summary(rdaGLA_PlotGenusAMMP)

plot(rdaGLA_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Canopy Openness Data at the Plot Level - scaling 1")


# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaGLA_PlotGenusAMMP)$r.squared) # 0.4988647 (not 1)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaGLA_PlotGenusAMMP)$adj.r.squared)  # 0.1090928






## Global test of the RDA result
anova(rdaGLA_PlotGenusAMMP, permutations = how(nperm = 999)) # 0.087 .
## Tests of all canonical axes
anova(rdaGLA_PlotGenusAMMP, by = "axis", permutations = how(nperm = 999))


# Forward selection using forward.sel() {adespatial}
# Global adjusted R^2
(R2a.all <- RsquareAdj(rdaGLA_PlotGenusAMMP)$adj.r.squared)
forward.sel(Plot_Genus_AM.nn.tr, GLA, adjR2thresh = R2a.all)


# Forward select won't work bc too many variables, so use ordistep:

# Forward selection using vegan's ordistep()
# This function allows the use of factors.

# Backward elimination using vegan's ordistep()


mod1 <- rda(Plot_Genus_AM.nn.tr ~ ., GLA)
mod0 <- rda(Plot_Genus_AM.nn.tr ~ 1, GLA)

#Backwards
BackwardGLAAMMP = ordistep(mod1, perm.max = 200) 
summary(BackwardGLAAMMP)

#        Df     AIC      F Pr(>F)  
#- Trans_dir          1 -15.023 1.6578  0.055 .
#- Trans_tot          1 -15.016 1.6643  0.055 .
#- Trans_dif_percent  1 -15.012 1.6675  0.055 .


#Forwards
ForwardGLAAMMP = ordistep(mod0, scope = formula(mod1), direction="forward", perm.max = 200)
summary(ForwardGLAAMMP) 

#Step: Plot_Genus_AM.nn.tr ~ Canopy_open_percent 


# Both
BothGLAAMMP = ordistep(mod0, scope = formula(mod1), perm.max = 200)

#- Canopy_open_percent  1 -15.909 1.7107  0.045 *


# Ordi R2 step:
OrdiR2GLAAMMP = ordiR2step(mod0, scope = formula(mod1), perm.max = 200)

#+ Trans_dif  1 -15.751 1.717  0.064 .


# ------------------------------

# RDA of selected GLA variables:


GLASelected = GLA[, c(1, 3)]

rdaGLASelected_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, GLASelected)

summary(rdaGLASelected_PlotGenusAMMP)

plot(rdaGLASelected_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Canopy oppenness and transmitted diffused light at the Plot Level - scaling 1")

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaGLASelected_PlotGenusAMMP)$r.squared) # 0.1839633
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaGLASelected_PlotGenusAMMP)$adj.r.squared)  # 0.06738666




## Global test of the RDA result
anova(rdaGLASelected_PlotGenusAMMP, permutations = how(nperm = 999)) # 0.03 *
## Tests of all canonical axes
anova(rdaGLASelected_PlotGenusAMMP, by = "axis", permutations = how(nperm = 999))




# --- Only Can op percent:

Canopy_op_perc = GLA[, c(1)]

rdaCanOpPerc_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, Canopy_op_perc)

summary(rdaCanOpPerc_PlotGenusAMMP)

plot(rdaCanOpPerc_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Canopy oppenness percent light at the Plot Level - scaling 1")

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaCanOpPerc_PlotGenusAMMP)$r.squared) # 0.1023726
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaCanOpPerc_PlotGenusAMMP)$adj.r.squared)  # 0.04253076

## Global test of the RDA result
anova(rdaCanOpPerc_PlotGenusAMMP, permutations = how(nperm = 999)) # 0.053 .



#________________________________________

# Doing it only with Florian:

# Florian: 

rdaFlorian_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, Florian)

summary(rdaFlorian_PlotGenusAMMP)

plot(rdaFlorian_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Structural Indeces Data at the Plot Level - scaling 1")

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaFlorian_PlotGenusAMMP)$r.squared) # 0.8381888 (not 1)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaFlorian_PlotGenusAMMP)$adj.r.squared)  # 0.1370068

## Global test of the RDA result
anova(rdaFlorian_PlotGenusAMMP, permutations = how(nperm = 999)) # 0.237



# Forward selection using forward.sel() {adespatial}
# Global adjusted R^2
(R2a.all <- RsquareAdj(rdaFlorian_PlotGenusAMMP)$adj.r.squared)
forward.sel(Plot_Genus_AM.nn.tr, Florian, adjR2thresh = R2a.all)

# > BA_p_mean


# Forward selection using vegan's ordistep()
# This function allows the use of factors.

# Backward elimination using vegan's ordistep()


mod1 <- rda(Plot_Genus_AM.nn.tr ~ ., Florian)
mod0 <- rda(Plot_Genus_AM.nn.tr ~ 1, Florian)



#Backwards
BackwardFlorianAMMP = ordistep(mod1, perm.max = 200) 
summary(BackwardFlorianAMMP)

#Df     AIC      F Pr(>F)   
#- sd_treeH_p_mean        1 -13.518 1.6089  0.095 . 
#- giniCoef_treeH_p_mean  1 -13.422 1.6800  0.080 . 
#- shannon_treeBA_p_mean  1 -13.365 1.7228  0.065 . 
#- mortality_abs_p_mean   1 -13.199 1.8475  0.040 * 
#- BA_p_mean              1 -12.154 2.6620  0.005 **




#Forwards
ForwardFlorianAMMP = ordistep(mod0, scope = formula(mod1), direction="forward", perm.max = 200)
summary(ForwardFlorianAMMP) 

#Step: Plot_Genus_AM.nn.tr ~ BA_p_mean 





# Both
BothFlorianAMMP = ordistep(mod0, scope = formula(mod1), perm.max = 200)

#            Df     AIC      F Pr(>F)  
#- BA_p_mean  1 -15.909 1.9778  0.025 *



# Ordi R2 step:
OrdiR2FlorianAMMP = ordiR2step(mod0, scope = formula(mod1), perm.max = 200)

#+ BA_p_mean  1 -16.014 1.9778  0.026 *




##############################################
# Onlt the most influential ones:

SigGlobalT = SelectedVariables[, c(1,2,3,4,6)]

rdaSigGlobalT_PlotGenusAMMP = rda(Plot_Genus_AM.nn.tr, SigGlobalT)

summary(rdaSigGlobalT_PlotGenusAMMP)

plot(rdaSigGlobalT_PlotGenusAMMP,
     scaling = 1,
     main = "RDA Butterfly Day-time Species  ~ 
     Selected and Global Test Significant Variables - scaling 1")

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rdaSigGlobalT_PlotGenusAMMP)$r.squared) # 0.4549497 (not 1)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rdaSigGlobalT_PlotGenusAMMP)$adj.r.squared)  # 0.2071996

## Global test of the RDA result
anova(rdaSigGlobalT_PlotGenusAMMP, permutations = how(nperm = 999)) # 0.001 *** ***


