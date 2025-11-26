# This script automatize the plotting of occupancies (with shading)
library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# ---  Crear carpeta donde guardar los gráficos ---
dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/grafics_ocupancies", showWarnings = FALSE)


# Funcio R: Ocupancia teorica en funcio del temps quan la condicio initial es una p_0 generica:
p_occupancy_ce <- function(c, e, t, p_0) {
  value <- (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
  return(value)
}

Occupancia_Shading_Function <- function(Occupancia_Shading, Sp, BioReg )
{
  
  gra <- ggplot(Ocupancia_Shading, aes(x = year)) +
    # Same fixed vertical scale (0,1) for all plots
    ylim(0, 1) +  
  
    # sombreado entre límites teóricos
    geom_ribbon(
      aes(ymin = oc_teorica_0, ymax = oc_teorica_1),
      fill = "grey70", alpha = 0.4
    ) +
    
    # línea teórica (estimación)
    geom_line(aes(y = oc_teorica, color = "Ocupancia teórica"),
              linewidth = 0.9, linetype = "solid") +
    
    # línea empírica (observada)
    geom_line(aes(y = oc_empirica, color = "Ocupancia empírica"),
              linewidth = 0.9, linetype = "dashed") +
    
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
      title = paste(Sp, "in", BioReg)
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
  
    # Print the plot to the device
    print(gra)
    
    return(gra)
}

# list_of_graphics <- list(list())

Sp = c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 
BioReg = c("Regió Alpina i Subalpina", 
           "Regió Mediterrània humida", 
           "Regió Mediterrània àrida")

for (i in 1:12 ) {
  # Dades ocupancia species i en les tres regions. 
  data_ocupancia <- as.data.frame(list_chi2[[i]])
  
  for (j in 1:3) {
    
    FES <- 1 # Only if "FES" is changed to 0, the plot is not done!!!
    
   
    # Controlar especies que no hi son presents en alguna regio bioclimatic
    if (j == 3 && i == 2) { # Lyca (Sp i=2) no hi es present en regio j=3 
      print(paste("No data for species", Sp[i], "in:", BioReg[j], 
                  "No occupancy plot possible!!!"))
      FES = 0
    }
    if (j == 2 && i == 2) { # Lyca (Sp i=2) no hi es present en regio j=2 
      print(paste("No data for species", Sp[i], "in:", BioReg[j], 
                  "No occupancy plot possible!!!"))
      FES = 0
    }
    if (j == 3 && i == 5) { # Cyani (Sp i=5) no hi es present en regio j=3  
      print(paste("No data for species", Sp[i], "in", BioReg[j], 
                  "No occupancy plot possible!!!"))  
      FES = 0
    }
      
    if ( FES == 1 ) {
      c = list_colext_regionsbioclima[[j]]$C[i]
      e = list_colext_regionsbioclima[[j]]$E[i]
      
      # p_0_1994 <- list_chi2[[i]]$n2[1]/list_chi2[[i]]$N2[1]
      
      # Col on hi ha el nombre d'itineraris ocupats a la regio j
      nn = 2 + (j-1)*2  
      # Col on hi ha el nombre d'itineraris mostrejats regio j
      NN = 3 + (j-1)*2  
      # Nombre d'itineraris ocupats a regio j per l'especie i a l'inici del periode (1994);
      n_1994  = data_ocupancia[1, nn] 
      # Nombre d'itineraris mostrejats a regio j a l'inici del periode (1994)
      N_1994  = data_ocupancia[1, NN]    
      # Ocupancia a l'inici del periode (1)
      p_0_1994 <- as.numeric(n_1994/N_1994)
      
      # print(paste("col_n =", nn, "col_N =", NN, 
      #             "n_0 = ", n_1994, "N_0 = ", N_1994, "p_0 = ", p_0_1994))
      
      Ocupancia_teorica <- p_occupancy_ce(c, e, 1:30, p_0_1994)
      Ocupancia_teorica <- c(p_0_1994, Ocupancia_teorica)
      
      Ocupancia_Shading <- data.frame()
      Ocupancia_Shading <- data.frame(year = 1994:2024)
      
      Ocupancia_Shading$n <- data_ocupancia[, nn]
      Ocupancia_Shading$N <- data_ocupancia[, NN]
      
      Ocupancia_Shading$oc_empirica <- Ocupancia_Shading$n/Ocupancia_Shading$N
      Ocupancia_Shading$oc_teorica <- Ocupancia_teorica
      
      n0 <- qbinom(0.025, 
                   as.numeric(Ocupancia_Shading$N), 
                   as.numeric(Ocupancia_Shading$oc_teorica))
      n1 <- qbinom(0.975, 
                   as.numeric(Ocupancia_Shading$N), 
                   as.numeric(Ocupancia_Shading$oc_teorica))
      
      Ocupancia_Shading$oc_teorica_0 <- n0/Ocupancia_Shading$N
      Ocupancia_Shading$oc_teorica_1 <- n1/Ocupancia_Shading$N
      
      # Graficar Occupancia Shading 
      # list_of_graphics[[i]][[j]] = 
      gra <- Occupancia_Shading_Function(Ocpancia_Shading, Sp[i], BioReg[j])
      
      ggsave(filename = paste0("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/grafics_ocupancies/", Sp[i], BioReg[j], "_Ocupancia.png"),
             plot = gra, width = 10, height = 7, dpi = 300)
    }
  }
}
