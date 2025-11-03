# This script automatize the plotting of occupancies (with shading)
library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# Funcio R: Ocupancia teorica en funcio del temps quan la condicio initial es una p_0 generica:
p_occupancy_ce <- function(c, e, t, p_0) {
  value <- (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
  return(value)
}

Occupancia_Shading_Function <- function(Occupancia_Shading, Sp, BioReg )
{
  
  ggplot(Ocupancia_Shading, aes(x = year)) +
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
      title = c(Sp, "in", BioReg)
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
  
}

list_of_graphics <- list(list())

Sp = c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 
BioReg = c("Regio Alpina i Subalpina", 
           "Regio Mediterranea humida", 
           "Regio Mediterranea arida")

FES <- 1
for (i in 1:12 ) {
  for (j in 1:3) {
    
    # Controlar especies que no hi son en alguna regio bioclimatic
    if (j == 3 && i == 2) {
      c("No data for species", i, "in region", j)
      FES = 0
    } 
    if (j == 3 && i == 5) { 
      c("No data for species", i, "in region", j)
      FES = 0
    }
    
    if ( FES != 0 ) {
      
      c = list_colext_regionsbioclima[[j]]$C[i]
      e = list_colext_regionsbioclima[[j]]$E[i]
      
      p_0_1994 <- plebe_chi2$n2[1]/plebe_chi2$N2[1]  
      
      temps <- 1:30
      
      Ocupancia_teorica <- p_occupancy_ce(c, e, temps, p_0_1994)
      Ocupancia_teorica <- c(p_0_1994, Ocupancia_teorica)
      
      Ocupancia_Shading <- data.frame()
      
      Ocupancia_Shading <- data.frame(year = 1994:2024)
      
      Ocupancia_Shading$n <- plebe_chi2$n2
      Ocupancia_Shading$N <- plebe_chi2$N2
      Ocupancia_Shading$oc_empirica <- plebe_chi2$n2/plebe_chi2$N2
      Ocupancia_Shading$oc_teorica <- Ocupancia_teorica
      
      
      n0 <- qbinom(0.025, Ocupancia_Shading$N, Ocupancia_Shading$oc_teorica)
      n1 <- qbinom(0.975, Ocupancia_Shading$N, Ocupancia_Shading$oc_teorica)
      
      Ocupancia_Shading$oc_teorica_0 <- n0/Ocupancia_Shading$N
      Ocupancia_Shading$oc_teorica_1 <- n1/Ocupancia_Shading$N
      
      # Graficar Occupancia Shading 
      list_of_graphics[[i]][[j]] = Occupancia_Shading_Function(Occpancia_Shading, 
                                                               Sp[i], 
                                                               BioReg[j])
      
    }
  }
}
