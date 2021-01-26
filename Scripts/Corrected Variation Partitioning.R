##################################################
##### C. Corrected Variation partitioning :
##################################################

# Between Tree diversity, the selected environmental variables and the selected structural variables:

(var.selected <- varpart(df1.nn.tr, TreeDiversity_Binary, SelectedVariables))
plot(var.selected, digits = 2, Xnames = c('Tree Div', 'Env and Structure'),bg = c("red", "blue"))

var.selected
summary(var.selected)



# Tree div and all the selected vars separately:

(var.selected_sep <- varpart(df1.nn.tr,TreeDiversity_Binary, EnvSelected, GLASelected, FlorianSelected))
plot(var.selected_sep, digits = 2, Xnames = c('Tree Div', 'Env', 'GLA', 'Structure'), bg = c("green", "blue", "red", "yellow"))

var.selected_sep
summary(var.selected_sep)



# -------------------------


# With only the most influential variables:

(var.mostinf <- varpart(df1.nn.tr,TreeDiversity_Binary, MostInf))
plot(var.mostinf, digits = 2, Xnames = c('Tree Div', 'Env and Structure Selected'), bg = c("green", "blue", "red", "yellow"))

var.mostinf
summary(var.mostinf)


(var.mostinf.sep <- varpart(df1.nn.tr,TreeDiversity_Binary, MostInf[,1], MostInf[, 2], MostInf[, 3]))
plot(var.mostinf.sep, digits = 2, Xnames = c('Tree Div', 'Moisture %', 'Canopy openness %', 'Mean BA'), bg = c("green", "blue", "red", "yellow"))

var.mostinf.sep
summary(var.mostinf.sep)


# -----------------------------

# With the ordistep selected variables that had the strongest global 
# test results:

(var.SigGlobalT <- varpart(df1.nn.tr,TreeDiversity_Binary, SigGlobalT))
plot(var.SigGlobalT, digits = 2, Xnames = c('Tree Div', 'Env and Structure Selected'), bg = c("green", "blue", "red", "yellow"))

var.SigGlobalT
summary(var.SigGlobalT)

#-----------------------------------------------------------------


