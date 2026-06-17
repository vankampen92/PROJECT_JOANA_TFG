# ==========================================================================
# SCRIPT: Generación de Panel de Ocupancia (12 especies x 3 regiones)
# Proyecto: TFG Joana - Análisis de Ocupancia Bioclimática
# ==========================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(png)
library(cowplot)
install.packages("ggtext")
library(ggtext) 

# 1. CARGA DE DATOS
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/my_list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# 2. CONFIGURACIÓN INICIAL
Sp_labels <- c(
  "Celastrina Argiolus"    = "*Celastrina*<br>*Argiolus*<br><img src='PROJECT_JOANA_TFG/ICONES/Celastrina.png' height='45' style='vertical-align:middle;'/>",
  "Lycaena Vigaureae"      = "*Lycaena*<br>*Vigaureae*<br><img src='PROJECT_JOANA_TFG/ICONES/Lycaena.png' height='45' style='vertical-align:middle;'/>",
  "Plebejus argus"         = "*Plebejus*<br>*argus*<br><img src='PROJECT_JOANA_TFG/ICONES/Pararge.png' height='45' style='vertical-align:middle;'/>", 
  "Psedophilotes panoptes" = "*Psedophilotes*<br>*panoptes*<br><img src='PROJECT_JOANA_TFG/ICONES/Pseudophilotes.png' height='45' style='vertical-align:middle;'/>",
  "Cyaniris semiargus"     = "*Cyaniris*<br>*semiargus*<br><img src='PROJECT_JOANA_TFG/ICONES/Cyaniris.png' height='45' style='vertical-align:middle;'/>",
  "Vanessa cardui"         = "*Vanessa*<br>*cardui*<br><img src='PROJECT_JOANA_TFG/ICONES/Vanessa.png' height='45' style='vertical-align:middle;'/>",
  "Aglais io"              = "*Aglais*<br>*io*<br><img src='PROJECT_JOANA_TFG/ICONES/Aglais.png' height='45' style='vertical-align:middle;'/>",
  "Anthocharis euphenoides"= "*Anthocharis*<br>*euphenoides*<br><img src='PROJECT_JOANA_TFG/ICONES/Anthocharis.png' height='45' style='vertical-align:middle;'/>",
  "Melanargia occitanica"  = "*Melanargia*<br>*occitanica*<br><img src='PROJECT_JOANA_TFG/ICONES/Melanargia.png' height='45' style='vertical-align:middle;'/>",
  "Pararge aegeria"        = "*Pararge*<br>*aegeria*<br><img src='PROJECT_JOANA_TFG/ICONES/Pararge.png' height='45' style='vertical-align:middle;'/>",
  "Pyronia bathseba"       = "*Pyronia*<br>*bathseba*<br><img src='PROJECT_JOANA_TFG/ICONES/Pyrobath.png' height='45' style='vertical-align:middle;'/>",
  "Pyronia cecilia"        = "*Pyronia*<br>*cecilia*<br><img src='PROJECT_JOANA_TFG/ICONES/Pyroceci.png' height='45' style='vertical-align:middle;'/>"
)

# Función matemática de ocupación teórica
p_occupancy_ce <- function(c, e, t, p_0) {
  (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
}

# 3. PROCESAMIENTO DE DATOS (Creación del Long Dataframe)
all_data_list <- list()

for (i in 1:12) {
  data_species_raw <- as.data.frame(my_list[[i]])
  
  for (j in 1:3) {
    FES <- 1
    if ((j == 3 && i == 2) || (j == 2 && i == 2) || (j == 3 && i == 5)) {
      FES <- 0
    }
    
    if (FES == 1) {
      c_val <- list_colext_regionsbioclima[[j]]$C[i]
      e_val <- list_colext_regionsbioclima[[j]]$E[i]
      
      nn <- 2 + (j-1)*3  
      MM <- 4 + (j-1)*3  
      
      n_1994 <- data_species_raw[1, nn]
      M_1994 <- data_species_raw[1, MM]
      p_0_1994 <- if(M_1994 > 0) as.numeric(n_1994/M_1994) else 0.0
      
      years_seq <- 0:30
      Ocupancia_teorica <- p_occupancy_ce(c_val, e_val, years_seq, p_0_1994)
      
      df_cell <- data.frame(
        year = 1994:2024,
        n = data_species_raw[, nn],
        M = data_species_raw[, MM],
        oc_teorica = Ocupancia_teorica,
        Species = Sp[i],
        Region = BioReg[j]
      )
      
      df_cell$oc_empirica <- ifelse(df_cell$M > 0, df_cell$n / df_cell$M, 0)
      
      df_cell$oc_teorica_0 <- qbinom(0.025, as.numeric(df_cell$M), as.numeric(df_cell$oc_teorica)) / df_cell$M
      df_cell$oc_teorica_1 <- qbinom(0.975, as.numeric(df_cell$M), as.numeric(df_cell$oc_teorica)) / df_cell$M
      
      all_data_list[[length(all_data_list) + 1]] <- df_cell
    }
  }
}

df_final <- bind_rows(all_data_list)
df_final$Region <- factor(df_final$Region, levels = BioReg)
df_final$Species <- factor(df_final$Species, levels = Sp)

# 4. GENERACIÓN DEL GRÁFICO (FACET GRID)
plot_panel <- ggplot(df_final, aes(x = year)) +
  geom_ribbon(aes(ymin = oc_teorica_0, ymax = oc_teorica_1), 
              fill = "grey75", alpha = 0.4) +
  geom_line(aes(y = oc_teorica, color = "Ocupància teòrica"), 
            linewidth = 0.8) +
  geom_line(aes(y = oc_empirica, color = "Ocupància empírica"), 
            linewidth = 0.8, linetype = "dashed") +
  
  facet_grid(Species ~ Region, scales = "fixed", labeller = labeller(Species = Sp_labels)) +
  
  ylim(0, 1) +
  scale_x_continuous(breaks = c(1994, 2009, 2024)) +
  scale_color_manual(values = c("Ocupància teòrica" = "#1b9e77", 
                                "Ocupància empírica" = "#d95f02")) +
  labs(title = "", subtitle = "", x = "Any", y = "Proporció d'ocupància", color = NULL) +
  
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.2, "lines"),
    
    # --- CAMBIO CLAVE AQUÍ ---
    # Centramos horizontalmente (hjust=0.5) y reducimos márgenes laterales (l=2, r=2)
    strip.text.y = element_markdown(size = 9.5, angle = 0, 
                                    hjust = 0.5, vjust = 0.5, # Centrado horizontal
                                    margin = margin(t = 4, b = 4, l = 2, r = 2)), # Márgenes mínimos
    
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.4, "lines"),
    axis.text.x = element_text(size = 11, angle = 0),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )

# Mostrar el gráfico integrado y más estrecho
print(plot_panel)
# 5. GUARDAR RESULTADO
dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/panel_final", showWarnings = FALSE)

ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/panel_final/Panel_Ocupancia_12x3_icones.pdf", 
       plot = plot_panel, width = 14, height = 24, device = "pdf")

print("Panel generado con éxito en la carpeta GRAFICS/panel_final")

# 5. GUARDAR RESULTADO EN PNG
dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/panel_final", showWarnings = FALSE)

# Nota: He aumentado un poco el ancho y alto para que al ser PNG 
# y tener 300 dpi, la imagen tenga suficiente definición.
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/panel_final/Panel_Ocupancia_C.png", 
       plot = plot_panel, 
       width = 16,     # Pulgadas de ancho
       height = 26,    # Pulgadas de alto
       dpi = 300,      # Resolución alta para que sea legible
       device = "png")

