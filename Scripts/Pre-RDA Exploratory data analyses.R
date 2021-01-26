

###################################################
#A.  Corrected Pre-RDA Exploratory data analyses:
###################################################



library(dplyr)
library(vegan)
library(ggplot2)
library(xlsx)
library(tidyverse)
library(magrittr)
library(ggpubr)



# 1. relative occurrences of the species with a frequency histogram: Would show the frequency at which the species are found by site. As we are comparing AM, PM and AM_PM, 
#   This would give me comparing on the frequency of species within those time in the same sites. 

# df1 is a df with plots to species as melted 

summary(df1)
range(df1)


df_sp_presence <- apply(df1 > 0, 2, sum)
sort(df_sp_presence)
df_sp_relf <- 100 * df_sp_presence/nrow(df1)
round(sort(df_sp_relf), 1)



#######################
# Histograms plots:

df1.nn = df1 %>% remove_rownames %>% column_to_rownames(var="Plot")


par(mfrow = c(2, 1))

hist(df_sp_presence,
     main = " Morning Butterfly sp. Occurrence Main Plantation (Family)",
     right = FALSE,
     las = 1,
     xlab = "Number of occurrences",
     ylab = "Number of species",
     col = "bisque"
)

hist(df_sp_relf,
     main = "Morning Butterfly sp. Relative Frequency Main Plantation (Family)",
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



################################
# Species accumulation curve:

par(mfrow = c(1,1))

Species_richness = specaccum(df1.nn)
Species_richness
plot(Species_richness)



# Use species accumulation curve but with the 1, 2 , 3 or 5 species plots as different curves to see
# what's up as in the article. 


# Will use the species richness of trees per plot to make a species accumulation curve:
# (Write what it is)

Species_community =  plot_to_species_melted[, colSums(plot_to_species_melted != 0) > 0]

# Adding to the melted sp_df a column of tree richness from another df, and calling it "Tree_richness":
Species_community_and_treerichness = cbind(Tree_richness = c(Tree_richness$richness_s_P), Species_community)

curve_sp_community = specaccum(Species_community_and_treerichness)




# Use species accumulation curve considering plots of different tree richness as separated curves, to see differences by treatment:

# the C(TreeDiv...) part is to cbind and rename the new column:

Species_community = cbind(tree_richness = c(TreeDivInd_Plot$richness_s_P), no_raw_names)

curve_Species_community2 = specaccum(Species_community)

#subset each habitat into its own df
Species_community_and_treerichness %>% filter(Tree_richness == 1) -> Monoculture
Species_community_and_treerichness %>% filter(Tree_richness == 2) -> Two_Species
Species_community_and_treerichness %>% filter(Tree_richness == 3) -> Three_Species
Species_community_and_treerichness %>% filter(Tree_richness == 5) -> Five_Species


#calculating species accumulation curve for each treatment
curve_Monoculture2 = specaccum(Monoculture[, 2:50], method = "random") # 2:50 is to show from which to white column the acction is made
curve_Two_Species2 = specaccum(Two_Species[, 2:50])
curve_Three_Species2 = specaccum(Three_Species[, 2:50])
curve_Five_Species2 = specaccum(Five_Species[, 2:50])


#plot curve_all first
plot(curve_Species_community2, main= "Species Accumulation Curve by Plot Tree Species Number")
#then plot the rest
plot(curve_Monoculture2, add = TRUE, col = 2) #col is COLOUR setting, so change it to something else if you want
plot(curve_Two_Species2, add = TRUE, col = 3)
plot(curve_Three_Species2, add = TRUE, col = 4)
plot(curve_Five_Species2, add = TRUE, col = 5)

legend( x = 12, y=20, legend = c("All Plots' Diversity", "Monocultures", "Two-Species Plots", "Three-Species Plots", "Five-Species Plots"), fill= c(1,2,3,4,5))


all_curves = cbind(curve_Species_community2, curve_Monoculture2, curve_Two_Species2,curve_Three_Species2, curve_Five_Species2)
as.data.frame(all_curves)




##################### 
# Rarefaction curve:

S <- specnumber(df1.nn)
(raremax <- min(rowSums(df1.nn)))
Srare <- rarefy(df1.nn, raremax)

plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", main = "Rarefaction Curve: Expected Number of Species as a
     Function of the Number of individuals In Main Plantation (Family)")
abline(0, 1)
rarecurve(df1.nn, step = 1, sample = raremax, col = "blue", cex = 0.6, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", main = "Rarefaction Curve: Expected Number of Species as a
          Function of the Number of individuals In Main Plantation (Family)")



###################
# Beta diversity:


# Gamma richness and expected species pool
?specpool
(gobs <- ncol(df1.nn))
(gthe <- specpool(df1.nn))

library(vegetarian)

# Multiplicative partitioning of Hill numbers (Jost 2006, 2007)
?d
# Mean alpha species richness
d(df1.nn, lev = "alpha", q = 0)
# Mean alpha Shannon diversity
d(df1.nn, lev = "alpha", q = 1)
# Mean alpha Simpson diversity
d(df1.nn, lev = "alpha", q = 2, boot = TRUE)
# Multiplicative beta species richness
d(df1.nn, lev = "beta", q = 0)
# Multiplicative beta Shannon diversity
d(df1.nn, lev = "beta", q = 1)
# Multiplicative beta Simpson diversity
d(df1.nn, lev = "beta", q = 2, boot = TRUE)
# Gamma species richness
d(df1.nn, lev = "gamma", q = 0)
# Gamma Shannon diversity
d(df1.nn, lev = "gamma", q = 1)
# Gamma Simpson diversity
d(df1.nn, lev = "gamma", q = 2, boot = TRUE)
# Plot multiplicative beta diversity vs order
mbeta <- data.frame(order = 0:20, beta = NA, se = NA)
for (i in 1:nrow(mbeta)) {
  out <- d(df1.nn, lev = "beta", q = mbeta$order[i], boot = TRUE)
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


##############################
##### Using betadiv:

library(adespatial)

# Computation using beta.div {adespatial} on
# Hellinger-transformed species data
spe.beta <- beta.div(df1.nn, method = "hellinger", nperm = 9999)
summary(spe.beta)
spe.beta$beta # SSTotal and BDTotal
# Which species have a SCBD larger than the mean SCBD?
spe.beta$SCBD[spe.beta$SCBD >= mean(spe.beta$SCBD)]

spe.beta$SCBD
spe.beta$LCBD
spe.beta$p.adj


