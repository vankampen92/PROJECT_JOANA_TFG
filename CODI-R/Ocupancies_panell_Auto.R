# ==========================================================================
# SCRIPT: Generación de Panel de Ocupancia (12 especies x 3 regiones)
# Proyecto: TFG Joana - Análisis de Ocupancia Bioclimática
# ==========================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. CARGA DE DATOS
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/my_list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# 2. CONFIGURACIÓN INICIAL
Sp <- c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
        "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
        "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
        "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 

BioReg <- c("Regió Alpina i Subalpina", 
            "Regió Mediterrània humida", 
            "Regió Mediterrània àrida")

# Función matemática de ocupación teórica
p_occupancy_ce <- function(c, e, t, p_0) {
  (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t)
}

# 3. PROCESAMIENTO DE DATOS (Creación del Long Dataframe)
all_data_list <- list()

for (i in 1:12) {
  # Extraer datos de la especie actual
  data_species_raw <- as.data.frame(my_list[[i]])
  
  for (j in 1:3) {
    # Control de presencia (FES)
    FES <- 1
    if ((j == 3 && i == 2) || (j == 2 && i == 2) || (j == 3 && i == 5)) {
      FES <- 0
    }
    
    if (FES == 1) {
      # Parámetros de colonización y extinción
      c_val <- list_colext_regionsbioclima[[j]]$C[i]
      e_val <- list_colext_regionsbioclima[[j]]$E[i]
      
      # Columnas según región (lógica original)
      nn <- 2 + (j-1)*3  # Itinerarios ocupados
      MM <- 4 + (j-1)*3  # Itinerarios mostreados
      
      # Ocupancia inicial (1994)
      n_1994 <- data_species_raw[1, nn]
      M_1994 <- data_species_raw[1, MM]
      p_0_1994 <- if(M_1994 > 0) as.numeric(n_1994/M_1994) else 0.0
      
      # Cálculo de curva teórica (1994-2024 = 31 puntos)
      years_seq <- 0:30
      Ocupancia_teorica <- p_occupancy_ce(c_val, e_val, years_seq, p_0_1994)
      
      # Crear dataframe temporal para esta celda
      df_cell <- data.frame(
        year = 1994:2024,
        n = data_species_raw[, nn],
        M = data_species_raw[, MM],
        oc_teorica = Ocupancia_teorica,
        Species = Sp[i],
        Region = BioReg[j]
      )
      
      # Ocupancia empírica
      df_cell$oc_empirica <- ifelse(df_cell$M > 0, df_cell$n / df_cell$M, 0)
      
      # Intervalos de confianza (Shading)
      df_cell$oc_teorica_0 <- qbinom(0.025, as.numeric(df_cell$M), as.numeric(df_cell$oc_teorica)) / df_cell$M
      df_cell$oc_teorica_1 <- qbinom(0.975, as.numeric(df_cell$M), as.numeric(df_cell$oc_teorica)) / df_cell$M
      
      all_data_list[[length(all_data_list) + 1]] <- df_cell
    }
  }
}

# Unimos todo y forzamos el orden de los factores
df_final <- bind_rows(all_data_list)
df_final$Region <- factor(df_final$Region, levels = BioReg)
df_final$Species <- factor(df_final$Species, levels = Sp)

# 4. GENERACIÓN DEL GRÁFICO (FACET GRID)
plot_panel <- ggplot(df_final, aes(x = year)) +
  # Área de confianza
  geom_ribbon(aes(ymin = oc_teorica_0, ymax = oc_teorica_1), 
              fill = "grey75", alpha = 0.4) +
  # Línea Teórica
  geom_line(aes(y = oc_teorica, color = "Ocupància teòrica"), 
            linewidth = 0.8) +
  # Línea Empírica
  geom_line(aes(y = oc_empirica, color = "Ocupància empírica"), 
            linewidth = 0.8, linetype = "dashed") +
  
  # Estructura de Panel (Filas: Especie, Columnas: Región)
  facet_grid(Species ~ Region, scales = "fixed") +
  
  # Escalas y Etiquetas
  ylim(0, 1) +
  scale_x_continuous(breaks = c(1994, 2009, 2024)) +
  scale_color_manual(values = c("Ocupància teòrica" = "#1b9e77", 
                                "Ocupància empírica" = "#d95f02")) +
  labs(title = "",
       subtitle = "",
       x = "Any", y = "Proporció d'ocupància", color = NULL) +
  
  # Estilo tipo Paper
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.2, "lines"),
    strip.text.y = element_text(size = 11, angle = 0, 
                                face = "italic",
                                hjust = 0,
                                vjust = 1,
                                margin = margin(t = 2, b = 12, l = 3)), # Nombres especies
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.4, "lines"),
    axis.text.x = element_text( size = 11, angle = 0),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
  )

# 5. GUARDAR RESULTADO
dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/panel_final", showWarnings = FALSE)

ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/panel_final/Panel_Ocupancia_12x3.pdf", 
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

print("Panel guardat correctament com a PNG a la carpeta GRAFICS/panel_final")