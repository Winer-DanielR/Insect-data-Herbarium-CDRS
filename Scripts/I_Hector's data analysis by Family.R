################################
####
####  Second Report Butterflies From Hector's Last Changes' Data by Family (Main Plantation)
####
################################


library(dplyr)
install.packages("vegan")
library(vegan)
library(ggplot2)
library(xlsx)
library(tidyverse)
library(magrittr)
library(ggpubr)


# (a) Carry out some exploratory analyses that will be useful to something. For each analysis, state
#the objective that you are pursuing, indicate what question the analysis that you have chosen is
#intended to answer. Then show the results and indicate clearly what answer you have obtained to
#your question.


# 1. relative occurrences of the species with a frequency histogram: 

#   Would show the frequency at which the species are found by site. As we are comparing AM, PM and AM_PM, 
#   This would give me comparing on the frequency of species within those time in the same sites. 

# Convertir cada familia o especie de insecto en una columna

# AM: #Mariposas en la mañana

summary(Plot_Family_AM)
range(Plot_Family_AM)

summary(Plot_Family_PM)
range(Plot_Family_PM)


spe.pres.Plot_Family_AM <- apply(Plot_Family_AM > 0, 2, sum)
sort(spe.pres.Plot_Family_AM)
spe.relf.APlot_Family_AM <- 100 * spe.pres.Plot_Family_AM/nrow(Plot_Family_AM)
round(sort(spe.relf.APlot_Family_AM), 1)

spe.pres.Plot_Family_PM <- apply(Plot_Family_PM > 0, 2, sum)
sort(spe.pres.Plot_Family_PM)
spe.relf.APlot_Family_PM <- 100 * spe.pres.Plot_Family_PM/nrow(Plot_Family_PM)
round(sort(spe.relf.APlot_Family_PM), 1)

# Histograms plots:

Plot_Family_AM.nn = Plot_Family_AM %>% remove_rownames %>% column_to_rownames(var="Plot")


par(mfrow = c(2, 1))

hist(spe.pres.Plot_Family_AM,
     main = " Morning Butterfly sp. Occurrence Main Plantation (Family)",
     right = FALSE,
     las = 1,
     xlab = "Number of occurrences",
     ylab = "Number of species",
     col = "bisque"
)

hist(spe.relf.APlot_Family_AM,
     main = "Morning Butterfly sp. Relative Frequency Main Plantation (Family)",
     right = FALSE,
     las = 1,
     xlab = "Frequency of occurrences (%)",
     ylab = "Number of species",
     breaks = seq(0, 100, by = 10),
     col = "bisque"
)



Plot_Family_AM.nn = Plot_Family_AM %>% remove_rownames %>% column_to_rownames(var="Plot")

Plot_Family_PM.nn = Plot_Family_PM %>% remove_rownames %>% column_to_rownames(var="Plot")


par(mfrow = c(2, 1))

hist(spe.pres.Plot_Family_PM,
     main = " Evening Butterfly sp. Occurrence Main Plantation (Family)",
     right = FALSE,
     las = 1,
     xlab = "Number of occurrences",
     ylab = "Number of species",
     col = "bisque"
)

hist(spe.relf.APlot_Family_PM,
     main = "Eveing Butterfly sp. Relative Frequency Main Plantation (Family)",
     right = FALSE,
     las = 1,
     xlab = "Frequency of occurrences (%)",
     ylab = "Number of species",
     breaks = seq(0, 100, by = 10),
     col = "bisque"
)




# 2. These results can later on be supported or evaluated next to a species accumulation curve, which 
# would demonstrate that still many species would have to be taken into consideration in order to have 
# a more meaningful frequency approach. 


# Species accumulation curve:

par(mfrow = c(1,1))

Species_richness = specaccum(Plot_Family_AM.nn)
Species_richness
plot(Species_richness)
??plot()


Species_richness = specaccum(Plot_Family_PM.nn)
Species_richness
plot(Species_richness)
??plot()

# Use species accumulation curve but with the 1, 2 , 3 or 5 species plots as different curves to see
# what's up as in the article. 


library(vegan)
library(dplyr)


# the C(TreeDiv...) part is to cbind and rename the new column:
# Will use the secies richness of trees per plot for the species acc curve:

#Different number of row that needs to be fixed first:
Tree_richness = Alphabetic_order_Species_per_plot[c(-7, -8, -10, -12, -22),]
Tree_richness = Tree_richness[, -1]


AllButterfly_community3 =  Alphabetic_order_Butterfly_AM[, colSums(Alphabetic_order_Butterfly_AM != 0) > 0]

AllButterfly_community4 = cbind(Tree_richness = c(Tree_richness$richness_s_P), AllButterfly_community3)
AllButterfly_community4 = AllButterfly_community4[, -2]

curve_AllButterfly_community2 = specaccum(AllButterfly_community4)


# Use species accumulation curve but with the 1, 2 , 3 or 5 species plots as different curves to see
# what's up as in the article. 


library(vegan)
library(dplyr)

# the C(TreeDiv...) part is to cbind and rename the new column:

AllButterfly_community = cbind(tree_richness = c(TreeDivInd_Plot$richness_s_P), no_raw_names)

curve_AllButterfly_community2 = specaccum(AllButterfly_community)

#subset each habitat into its own df
AllButterfly_community4 %>% filter(Tree_richness == 1) -> Monoculture
AllButterfly_community4 %>% filter(Tree_richness == 2) -> Two_Species
AllButterfly_community4 %>% filter(Tree_richness == 3) -> Three_Species
AllButterfly_community4 %>% filter(Tree_richness == 5) -> Five_Species


#calc species accumulation curve for each habitat
curve_Monoculture2 = specaccum(Monoculture[, 2:50], method = "random")
curve_Two_Species2 = specaccum(Two_Species[, 2:50])
curve_Three_Species2 = specaccum(Three_Species[, 2:50])
curve_Five_Species2 = specaccum(Five_Species[, 2:50])


#plot curve_all first
plot(curve_AllButterfly_community2, main= "Species Accumulation Curve by Plot Tree Species Number")
#then plot the rest
plot(curve_Monoculture2, add = TRUE, col = 2) #col is COLOUR setting, so change it to something else if you want
plot(curve_Two_Species2, add = TRUE, col = 3)
plot(curve_Three_Species2, add = TRUE, col = 4)
plot(curve_Five_Species2, add = TRUE, col = 5)

legend( x = 12, y=20, legend = c("All Plots' Diversity", "Monocultures", "Two-Species Plots", "Three-Species Plots", "Five-Species Plots"), fill= c(1,2,3,4,5))


all_curves = cbind(curve_AllButterfly_community2, curve_Monoculture2, curve_Two_Species2,curve_Three_Species2, curve_Five_Species2)
as.data.frame(all_curves)

##################### 

# Rarefaction curve:

library(vegan)

S <- specnumber(Plot_Family_AM.nn)
(raremax <- min(rowSums(Plot_Family_AM.nn)))
Srare <- rarefy(Plot_Family_AM.nn, raremax)

plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", main = "Rarefaction Curve: Expected Number of Species as a
     Function of the Number of individuals In Main Plantation (Family)")
abline(0, 1)
rarecurve(Plot_Family_AM.nn, step = 1, sample = raremax, col = "blue", cex = 0.6, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", main = "Rarefaction Curve: Expected Number of Species as a
          Function of the Number of individuals In Main Plantation (Family)")



S <- specnumber(Plot_Family_PM.nn)
(raremax <- min(rowSums(Plot_Family_PM.nn)))
Srare <- rarefy(Plot_Family_PM.nn, raremax)

plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", main = "Rarefaction Curve: Expected Number of Evening Species as a
     Function of the Number of Individuals in Main Plantation (Family)")
abline(0, 1)
rarecurve(Plot_Family_AM.nn, step = 1, sample = raremax, col = "blue", cex = 0.6, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", main = "Rarefaction Curve: Expected Number of Species as a
          Function of the Number of Evening Individuals in Main Plantation (Family)")


###################################

####################
######## PCA:
####################

# Butterfly AM and Sp ricness etc

#Hellinger transformation


Plot_Family_AM.nn.tr<- decostand(Plot_Family_AM.nn, method = "hellinger")


Plot_Family_AM.nn.tr.pca <- rda(Plot_Family_AM.nn.tr, scale = TRUE, main = "PCA Butterfly AM hellinger")
Plot_Family_AM.nn.tr.pca

par(mfrow = c(1, 2))
biplot(Plot_Family_AM.nn.tr.pca, scaling = 1, main = "PCA Morning Butterfly Main Plantation  Hellinger - scaling 1")
screeplot(Plot_Family_AM.nn.tr.pca, bstick = TRUE, npcs = length(Plot_Family_AM.nn.tr.pca$CA$eig))



Plot_Family_PM.nn.tr<- decostand(Plot_Family_PM.nn, method = "hellinger")


Plot_Family_PM.nn.tr.pca <- rda(Plot_Family_PM.nn.tr, scale = TRUE, main = "PCA Butterfly PM hellinger")
Plot_Family_PM.nn.tr.pca

par(mfrow = c(1, 2))
biplot(Plot_Family_PM.nn.tr.pca, scaling = 1, main = "PCA Evening Butterfly Main Plantation  Hellinger - scaling 1")
screeplot(Plot_Family_PM.nn.tr.pca, bstick = TRUE, npcs = length(Plot_Family_AM.nn.tr.pca$CA$eig))





library(ade4)
library(vegan)
library(gclus)
library(ape)
library(missMDA)
library(FactoMineR)



# Review of RDA:

Alphabetic_order_Species_per_plot
Treespecies.data <- Alphabetic_order_Species_per_plot
Treespecies.data.norowname =  Treespecies.data %>% remove_rownames %>% column_to_rownames(var="Plot")
Treespecies.data_tr = decostand(Treespecies.data.norowname, method = "hellinger")
Treespecies.data_tr = Treespecies.data_tr[c(-7, -8, -10, -12, -22),]

rda_Treespecies.ButterAM_tr <- vegan::rda(Alphabetic_order_Butterfly_AM.tr, Treespecies.data_tr)

#Here's how to make a plot:
# Scaling 1
plot(rda_Treespecies.ButterAM_tr,
     scaling = 1,
     main = " RDA Butterfly AM ~ Tree Species and Diversity - scaling 1"
)








###################################




# Beta diversity:


# Gamma richness and expected species pool
?specpool
(gobs <- ncol(Plot_Family_AM.nn))
(gthe <- specpool(Plot_Family_AM.nn))

library(vegetarian)

# Multiplicative partitioning of Hill numbers (Jost 2006, 2007)
?d
# Mean alpha species richness
d(Plot_Family_AM.nn, lev = "alpha", q = 0)
# Mean alpha Shannon diversity
d(Plot_Family_AM.nn, lev = "alpha", q = 1)
# Mean alpha Simpson diversity
d(Plot_Family_AM.nn, lev = "alpha", q = 2, boot = TRUE)
# Multiplicative beta species richness
d(Plot_Family_AM.nn, lev = "beta", q = 0)
# Multiplicative beta Shannon diversity
d(Plot_Family_AM.nn, lev = "beta", q = 1)
# Multiplicative beta Simpson diversity
d(Plot_Family_AM.nn, lev = "beta", q = 2, boot = TRUE)
# Gamma species richness
d(Plot_Family_AM.nn, lev = "gamma", q = 0)
# Gamma Shannon diversity
d(Plot_Family_AM.nn, lev = "gamma", q = 1)
# Gamma Simpson diversity
d(Plot_Family_AM.nn, lev = "gamma", q = 2, boot = TRUE)
# Plot multiplicative beta diversity vs order
mbeta <- data.frame(order = 0:20, beta = NA, se = NA)
for (i in 1:nrow(mbeta)) {
  out <- d(Plot_Family_AM.nn, lev = "beta", q = mbeta$order[i], boot = TRUE)
  mbeta$beta[i] <- out$D.Value
  mbeta$se[i] <- out$StdErr
}
mbeta
ggplot(mbeta, aes(order, beta)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(order, beta, ymin = beta - se,
                    ymax = beta + se), width = 0.2) +
  labs(y = "Multiplicative beta diversity",
       x = "Order of the diversity measure")



##### Using betadiv:

library(adespatial)

# Computation using beta.div {adespatial} on
# Hellinger-transformed species data
spe.beta <- beta.div(Plot_Family_AM.nn, method = "hellinger", nperm = 9999)
summary(spe.beta)
spe.beta$beta # SSTotal and BDTotal
# Which species have a SCBD larger than the mean SCBD?
spe.beta$SCBD[spe.beta$SCBD >= mean(spe.beta$SCBD)]

spe.beta$SCBD
spe.beta$LCBD
spe.beta$p.adj




#########################
###########################



# RDA:
par(mfrow = c(1,1))
library(binaryLogic)

TreeDiv = Tree_richness[,c("Plot","richness_s_P")]
TreeDiv = TreeDiv %>% remove_rownames %>% column_to_rownames(var="Plot")

TreeDiv_1 = TreeDiv[c("CM1","HC1", "LS1", "LS2", "TR1"),]
TreeDiv_2or3 = TreeDiv[c("T1","T2", "T3", "T4", "T5", "T6"),]
TreeDiv_5 = TreeDiv[c("A1", "A2", "A3", "A4", "A5", "A6"),]

TreeDiv_Separate <- cast(Tree_richness, Plot~richness_s_P, sum)
TreeDiv_SeparateBinary = as.binary(TreeDiv_Separate)

write.xlsx(TreeDiv_Separate, "TreeDiv_Separate.xlsx")

TreeDiv_SeparateBinary = TreeDiv_Separate2 %>% remove_rownames %>% column_to_rownames(var="Plot")

# RDA for TreeDiv all together and butterflies PM:
Plot_Family_PM.nn.tr
TreeDiv

rda_TreeDivalltogether_ButterflyPM_MainPLantationFAMILY <- rda(Plot_Family_PM.nn.tr, TreeDiv)
summary(rda_TreeDivalltogether_ButterflyPM_MainPLantationFAMILY)

plot(rda_TreeDivalltogether_ButterflyPM_MainPLantationFAMILY,
     scaling = 1,
     main = "RDA Butterfly Evening Species  ~ 
     Tree Diversity at Plot Level - scaling 1")

(spe.rda <- rda(Plot_Family_AM.nn.tr ~ ., TreeDiv))
summary(spe.rda)


# # RDA for TreeDiv all together and butterflies AM:

rda_TreeDivalltogether_ButterflyAM_MainPLantationFAMILY <- rda(Plot_Family_AM.nn.tr, TreeDiv)
summary(rda_TreeDivalltogether_ButterflyAM_MainPLantationFAMILY)

plot(rda_TreeDivalltogether_ButterflyAM_MainPLantationFAMILY,
     scaling = 1,
     main = "RDA Butterfly Morning Species  ~ 
     Tree Diversity at Plot Level - scaling 1")

(spe.rda <- rda(Plot_Family_AM.nn.tr ~ ., TreeDiv))
summary(spe.rda)


#Hellinger transformation 

# RDA TReDiv Spe. PM

rda_TreeDiv_ButterflyPM_MainPLantationFAMILY <- rda(Plot_Family_PM.nn.tr, TreeDiv_SeparateBinary)
summary(rda_TreeDiv_ButterflyPM_MainPLantation)

plot(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY,
     scaling = 1,
     main = "RDA Butterfly Evening Families  ~ 
     Tree Diversity at Plot Level - scaling 1")

(spe.rda <- rda(Plot_Family_PM.nn.tr ~ ., TreeDiv_SeparateBinary))
summary(spe.rda)

# RDA TReDiv Spe. AM

rda_TreeDiv_ButterflyAM_MainPLantationFAMILY <- rda(Plot_Family_AM.nn.tr, TreeDiv_SeparateBinary)

rda_TreeDiv_ButterflyAM_MainPLantationFAMILY <- rda(Plot_Family_AM.nn.tr~., TreeDiv_SeparateBinary)


summary(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY)

plot(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY,
     scaling = 1,
     main = "RDA Butterfly Morning Families  ~ 
     Tree Diversity at Plot Level - scaling 1")

(spe.rda <- rda(Plot_Family_AM.nn.tr ~ ., TreeDiv_SeparateBinary))
summary(spe.rda)

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY)$adj.r.squared)


# Select species with goodness-of-fit at least 0.6 in the
# ordination plane formed by axes 1 and 2
spe.good <- goodness(spe.rda)
sel.sp <- which(spe.good[, 2] >= 0.6)
# Triplots with homemade function triplot.rda(), scalings 1 and 2
triplot.rda(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY,
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
anova(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY, permutations = how(nperm = 999))
## Tests of all canonical axes
anova(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY, by = "axis", permutations = how(nperm = 999))

# Apply Kaiser-Guttman criterion to residual axes
rda_TreeDiv_ButterflyAM_MainPLantationFAMILY$CA$eig[rda_TreeDiv_ButterflyAM_MainPLantationFAMILY$CA$eig > mean(rda_TreeDiv_ButterflyAM_MainPLantationFAMILY$CA$eig)]

#Here's how to make a  plot:
# Scaling 1
plot(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY,
     scaling = 1,
     main = "Triplot RDA Butterfly Families AM ~ 
     Tree Diversity Indices at Plot Level - scaling 1")

summary(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY)

# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY)$adj.r.squared)

triplot.rda(rda_TreeDiv_ButterflyPM_MainPLantation,
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
anova(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY, permutations = how(nperm = 999))
## Tests of all canonical axes
anova(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY, by = "axis", permutations = how(nperm = 999))

# Apply Kaiser-Guttman criterion to residual axes
rda_TreeDiv_ButterflyPM_MainPLantationFAMILY$CA$eig[rda_TreeDiv_ButterflyPM_MainPLantationFAMILY$CA$eig > mean(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY$CA$eig)]

?rda

summary(rda_TreeDiv_ButterflyPM_MainPLantationFAMILY)

