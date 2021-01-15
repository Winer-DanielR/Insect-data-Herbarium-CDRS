#################################################################################
#####
##### Using MFA to compare Am and PM Fores to Plantation Species in Sardinilla 
#################################################################################


# p. 282 Manual

# AM

MFA_am = data.frame(Open_Canopy_am.hel, Closed_Canopy_am.hel, TreeDiv_SeparateBinary, MostInf)
dim(MFA_am)

# Number of variables in each group
(grn <- c(ncol(Open_Canopy_am.hel), ncol(Closed_Canopy_am.hel), ncol(TreeDiv_SeparateBinary), ncol(Env_MFA)))
# Compute the MFA without multiple plots
Open_Closed_MFA_am <- MFA(MFA_am,
              group = grn,
              type = c("c", "c", "s", "s"),
              ncp = 2,
              name.group = c("Open_Canopy_am.hel ", "Closed_Canopy_am.hel", "Tree Diversity", "Environment"),graph = FALSE)

Open_Closed_MFA_am
# Plot the results
plot(Open_Closed_MFA_am,
     choix = "axes",
     habillage = "group",
     shadowtext = TRUE)
plot(
  Open_Closed_MFA_am,
  choix = "ind",
  partial = "all",
  habillage = "group")
plot(Open_Closed_MFA_am,
     choix = "var",
     habillage = "group",
     shadowtext = TRUE)
plot(Open_Closed_MFA_am, choix = "group")
# Eigenvalues, scree plot and broken stick model
ev <- Open_Closed_MFA_am$eig[, 1]
names(ev) <- paste("MFA", 1 : length(ev))
screestick(ev, las = 2)


help.search("RV")

# Getting RV:

# RV coefficients with tests (p-values above the diagonal of
# the matrix)
rvp <- Open_Closed_MFA_am$group$RV
rvp[1, 2] <- coeffRV(Open_Canopy_am.hel2, Closed_Canopy_pm.hel2, scale(MostInf))$p.value
rvp[1, 3] <- coeffRV(Open_Canopy_am.hel2, Closed_Canopy_pm.hel2, scale(MostInf))$p.value
rvp[2, 3] <- coeffRV(scale(MostInf), scale(TreeDiv_SeparateBinary))$p.value
round(rvp[-4, -4], 6)

############################################################


# PM

# There are no PM species on plot A2, and so it has been erased when grouping by species.
# Need to merg the tables together to have the A2 plot in the PM species matrix. 

Open_Canopy_pm_hel2 = Open_Canopy_pm_hel2 %>% remove_rownames %>% column_to_rownames(var="Plot")

MFA_pm = data.frame(Open_Canopy_pm_hel2, Closed_Canopy_pm_hel2, TreeDiv_SeparateBinary, SelectedVariables_pm_noGLA)
dim(MFA_pm)

# Number of variables in each group
(grn <- c(ncol(Open_Canopy_pm_hel2), ncol(Closed_Canopy_pm_hel2), ncol(TreeDiv_SeparateBinary), ncol(SelectedVariables_pm_noGLA)))
# Compute the MFA without multiple plots
Open_Closed_MFA_pm <- MFA(MFA_pm,
                          group = grn,
                          type = c("c", "c", "s", "s"),
                          ncp = 2,
                          name.group = c("Open_Canopy_pm.hel ", "Closed_Canopy_pm.hel", "Tree Diversity", "Environment"),graph = FALSE)

Open_Closed_MFA_pm
# Plot the results
plot(Open_Closed_MFA_pm,
     choix = "axes",
     habillage = "group",
     shadowtext = TRUE)
plot(
  Open_Closed_MFA_pm,
  choix = "ind",
  partial = "all",
  habillage = "group")
plot(Open_Closed_MFA_pm,
     choix = "var",
     habillage = "group",
     shadowtext = TRUE)
plot(Open_Closed_MFA_pm, choix = "group")
# Eigenvalues, scree plot and broken stick model
ev <- Open_Closed_MFA_pm$eig[, 1]
names(ev) <- paste("MFA", 1 : length(ev))
screestick(ev, las = 2)
