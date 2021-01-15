
##################################################
##### Variation partitioning - at the genus level:
##################################################

# Only between Tree div and all selected env and structural vars together:

(var.selected <- varpart(Plot_Genus_AM.nn.tr, TreeDiv_SeparateBinary, SelectedVariables))
plot(var.selected, digits = 2, Xnames = c('Tree Div', 'Env and Structure'),bg = c("red", "blue"))

var.selected
summary(var.selected)



# Tree div and all the selected vars separately:

(var.selected_sep <- varpart(Plot_Genus_AM.nn.tr,TreeDiv_SeparateBinary, EnvSelected, GLASelected, FlorianSelected))
plot(var.selected_sep, digits = 2, Xnames = c('Tree Div', 'Env', 'GLA', 'Structure'), bg = c("green", "blue", "red", "yellow"))

var.selected_sep
summary(var.selected_sep)



# -------------------------


# Now with only the most influential ones:

(var.mostinf <- varpart(Plot_Genus_AM.nn.tr,TreeDiv_SeparateBinary, MostInf))
plot(var.mostinf, digits = 2, Xnames = c('Tree Div', 'Env and Structure Selected'), bg = c("green", "blue", "red", "yellow"))

var.mostinf
summary(var.mostinf)


(var.mostinf.sep <- varpart(Plot_Genus_AM.nn.tr,TreeDiv_SeparateBinary, MostInf[,1], MostInf[, 2], MostInf[, 3]))
plot(var.mostinf.sep, digits = 2, Xnames = c('Tree Div', 'Moisture %', 'Canopy openness %', 'Mean BA'), bg = c("green", "blue", "red", "yellow"))

var.mostinf.sep
summary(var.mostinf.sep)


# -----------------------------

# Now with the ordistep selected variables that had the strongest global 
# test results:

(var.SigGlobalT <- varpart(Plot_Genus_AM.nn.tr,TreeDiv_SeparateBinary, SigGlobalT))
plot(var.SigGlobalT, digits = 2, Xnames = c('Tree Div', 'Env and Structure Selected'), bg = c("green", "blue", "red", "yellow"))

var.SigGlobalT
summary(var.SigGlobalT)

#-----------------------------------------------------------------
#-----------------------------------------------------------------



# PM:

SelectedVariables_pm_noGLA = cbind(EnvSelected[, -1], meanBA_pm)

rdaSelectedVariables_noGLA_pm_PlotGenusPMMP = rda(Plot_Genus_PM.nn.tr, SelectedVariables_pm_noGLA)


(var.selected_pm <- varpart(Plot_Genus_PM.nn.tr, TreeDiv_SeparateBinary, SelectedVariables_pm_noGLA))
plot(var.selected_pm, digits = 2, Xnames = c('Tree Div', 'Moisture and BA'),bg = c("red", "blue"))

var.selected
summary(var.selected)



# Tree div and all the selected vars separately:

(var.selected_sep_pm <- varpart(Plot_Genus_PM.nn.tr,TreeDiv_SeparateBinary, EnvSelected[, -1], meanBA_pm))
plot(var.selected_sep_pm, digits = 2, Xnames = c('Tree Div', 'Moisture', 'Mean Basal Area'), bg = c("green", "blue", "red", "yellow"))

var.selected_sep_pm
summary(var.selected_sep_pm)

#### Basal Area seemsto have the strongest impact here! Should test it

