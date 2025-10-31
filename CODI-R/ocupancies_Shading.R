# CALCUL OCUPANCIES Plebejus argus Mediterranea Humida ####################
#
# Funcio R: Ocupancia teorica en funcio del temps quan la condicio initial es una p_0 generica:
p_occupancy_ce <- function(c, e, t, p_0) {
  value <- (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
  return(value)
}

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2.RData")

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


library(ggplot2)
library(dplyr)

ggplot(Ocupancia_Shading_plebarg_BR2, aes(x = year)) +
  # sombreado entre límites teóricos
  geom_ribbon(
    aes(ymin = oc_teorica_0, ymax = oc_teorica_1),
    fill = "grey70", alpha = 0.4
  ) +
  
  # línea teórica (estimación)
  geom_line(aes(y = oc_teorica, color = "Ocupancia teórica"),
            size = 0.9, linetype = "solid") +
  
  # línea empírica (observada)
  geom_line(aes(y = oc_empirica, color = "Ocupancia empírica"),
            size = 0.9, linetype = "dashed") +
  
  # escala de colores sobria
  scale_color_manual(
    values = c("Ocupancia teórica" = "#1b9e77", 
               "Ocupancia empírica" = "#d95f02")
  ) +
  
  # etiquetas
  labs(
    x = "Any",
    y = "Ocupancia",
    color = NULL,
    title = NULL
  ) +
  
  # estilo tipo paper
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.background = element_blank(),
    legend.key = element_blank()
  )

###########################
#Pararge aegeria
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/para_chi2.RData")

c = colext_Results_df_BR2$C_BR2[10]
e = colext_Results_df_BR2$E_BR2[10]
p_0_1994 <- para_chi2$n2[1]/para_chi2$N2[1]  

temps <- 1:30

Ocupancia_teorica <- p_occupancy_ce(c, e, temps, p_0_1994)
Ocupancia_teorica <- c(p_0_1994, Ocupancia_teorica)


Ocupancia_Shading_paraaege_BR2 <- data.frame(
  year = 1994:2024)
Ocupancia_Shading_paraaege_BR2$n <- para_chi2$n2
Ocupancia_Shading_paraaege_BR2$N <- para_chi2$N2
Ocupancia_Shading_paraaege_BR2$oc_empirica <- para_chi2$n2/para_chi2$N2
Ocupancia_Shading_paraaege_BR2$oc_teorica <- Ocupancia_teorica


n0 <- qbinom(0.025, Ocupancia_Shading_paraaege_BR2$N, Ocupancia_Shading_paraaege_BR2$oc_teorica)
n1 <- qbinom(0.975, Ocupancia_Shading_paraaege_BR2$N, Ocupancia_Shading_paraaege_BR2$oc_teorica)

Ocupancia_Shading_paraaege_BR2$oc_teorica_0 <- n0/Ocupancia_Shading_paraaege_BR2$N
Ocupancia_Shading_paraaege_BR2$oc_teorica_1 <- n1/Ocupancia_Shading_paraaege_BR2$N


library(ggplot2)
library(dplyr)

ggplot(Ocupancia_Shading_paraaege_BR2, aes(x = year)) +
  # sombreado entre límites teóricos
  geom_ribbon(
    aes(ymin = oc_teorica_0, ymax = oc_teorica_1),
    fill = "grey70", alpha = 0.4
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +

  # línea teórica (estimación)
  geom_line(aes(y = oc_teorica, color = "Ocupancia teórica"),
            size = 0.9, linetype = "solid") +
  
  # línea empírica (observada)
  geom_line(aes(y = oc_empirica, color = "Ocupancia empírica"),
            size = 0.9, linetype = "dashed") +
  
  # escala de colores sobria
  scale_color_manual(
    values = c("Ocupancia teórica" = "#1b9e77", 
               "Ocupancia empírica" = "#d95f02")
  ) +
  
  # etiquetas
  labs(
    x = "Any",
    y = "Ocupancia",
    color = NULL,
    title = NULL
  ) +
  
  # estilo tipo paper
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.background = element_blank(),
    legend.key = element_blank()
  ) 
  


