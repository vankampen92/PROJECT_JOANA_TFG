# This script automatize the plotting of occupancies (with shading)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2_colext_e_rbc.RData")



c = colext_Results_df_BR2$C_BR2[3]
e = colext_Results_df_BR2$E_BR2[3]

p_0_1994 <- plebe_chi2$n2[1]/plebe_chi2$N2[1]  

temps <- 1:30

Ocupancia_teorica <- p_occupancy_ce(c, e, temps, p_0_1994)
Ocupancia_teorica <- c(p_0_1994, Ocupancia_teorica)


Ocupancia_Shading_plebarg_BR2 <- data.frame(
  year = 1994:2024)
Ocupancia_Shading_plebarg_BR2$n <- plebe_chi2$n2
Ocupancia_Shading_plebarg_BR2$N <- plebe_chi2$N2
Ocupancia_Shading_plebarg_BR2$oc_empirica <- plebe_chi2$n2/plebe_chi2$N2
Ocupancia_Shading_plebarg_BR2$oc_teorica <- Ocupancia_teorica


n0 <- qbinom(0.025, Ocupancia_Shading_plebarg_BR2$N, Ocupancia_Shading_plebarg_BR2$oc_teorica)
n1 <- qbinom(0.975, Ocupancia_Shading_plebarg_BR2$N, Ocupancia_Shading_plebarg_BR2$oc_teorica)

Ocupancia_Shading_plebarg_BR2$oc_teorica_0 <- n0/Ocupancia_Shading_plebarg_BR2$N
Ocupancia_Shading_plebarg_BR2$oc_teorica_1 <- n1/Ocupancia_Shading_plebarg_BR2$N

