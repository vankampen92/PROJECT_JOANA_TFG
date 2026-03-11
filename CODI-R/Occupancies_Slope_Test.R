# This script automatize the plotting of occupancies (with shading)
library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/my_list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# ---  Crear carpeta donde guardar los gráficos ---
# dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/grafics_ocupancies", showWarnings = FALSE)

Sp = c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 

BioReg = c("Regió Alpina i Subalpina", 
           "Regió Mediterrània humida", 
           "Regió Mediterrània àrida")

ocupancies <- data.frame(
  year = c(1994:2024),
  p_1  = numeric(31),
  p_2  = numeric(31),
  p_3  = numeric(31)
)

ocupancies_2004 <- data.frame(
  year = c(2004:2024),
  p_1  = numeric(21),
  p_2  = numeric(21),
  p_3  = numeric(21)
)

load("~/PROJECT_JOANA_TFG/DADES/my_list_chi2.RData")

llista_sp_ocupancies <- list()

results_1 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_2 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_3 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:12 ) {
  # Dades ocupancia species i en les tres regions. 
  data_ocupancia <- as.data.frame(my_list[[i]])
  
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
      
      # Col on hi ha el nombre d'itineraris ocupats a la regio j
      nn = 2 + (j-1)*3  
      # Col on hi ha el nombre d'itineraris mostrejats regio j
      MM = 4 + (j-1)*3  # (M1, M2, o M3: metapoblacio potencial)
      
      ocupancies[,j+1] = data_ocupancia[, nn]/data_ocupancia[, MM]
      
      if( j == 1) {
        model <- lm(p_1 ~ year, data = ocupancies)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_1 <- rbind(results_1,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 2 ) {
        model <- lm(p_2 ~ year, data = ocupancies)  
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_2 <- rbind(results_2,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 3) {
        model <- lm(p_3 ~ year, data = ocupancies)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_3 <- rbind(results_3,
                       data.frame(species = Sp[i],
                                  slope = slope,
                                  p_value = pval))
      }
    }
  }
  
  llista_sp_ocupancies[[i]] <- ocupancies
}

# Llista and results: tots a zero patatero!!! 
llista_sp_ocupancies <- list()

results_1 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_2 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_3 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

###  Loop Ocupancia filtrada 2004 en endavanbt 
for (i in 1:12 ) {
  # Dades ocupancia species i en les tres regions. 
  data_ocupancia <- as.data.frame(my_list[[i]])
  
  data_ocupancia_2004 <- data_ocupancia[data_ocupancia$year >= 2004, ]
  
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
      
      # Col on hi ha el nombre d'itineraris ocupats a la regio j
      nn = 2 + (j-1)*3  
      # Col on hi ha el nombre d'itineraris mostrejats regio j
      MM = 4 + (j-1)*3  # (M1, M2, o M3: metapoblacio potencial)
      
      ocupancies_2004[,j+1] = data_ocupancia_2004[, nn]/data_ocupancia_2004[, MM]
      
      if( j == 1) {
        model <- lm(p_1 ~ year, data = ocupancies_2004)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_1 <- rbind(results_1,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 2 ) {
        model <- lm(p_2 ~ year, data = ocupancies_2004)  
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_2 <- rbind(results_2,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 3) {
        model <- lm(p_3 ~ year, data = ocupancies_2004)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_3 <- rbind(results_3,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
    }
  }
  
  llista_sp_ocupancies[[i]] <- ocupancies_2004
}

results_1$trend <- ifelse(results_1$p_value < 0.05 & results_1$slope > 0,
                        "Significant increase",
                        ifelse(results_1$p_value < 0.05 & results_1$slope < 0,
                               "Significant decrease",
                               "No significant trend"))

results_2$trend <- ifelse(results_2$p_value < 0.05 & results_2$slope > 0,
                          "Significant increase",
                          ifelse(results_2$p_value < 0.05 & results_2$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

results_3$trend <- ifelse(results_3$p_value < 0.05 & results_3$slope > 0,
                          "Significant increase",
                          ifelse(results_3$p_value < 0.05 & results_3$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

# Filtering for M > M_min: 
# Build reduced dataframe
ocupancia_year_p <- data.frame(
  year = numeric(),
  p    = numeric()
)

results_1 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)
results_2 <- results_1
results_3 <- results_1

Occupancia_M_Filtering_Function <- function(data_ocupancia, j, M_min = 10)
{
  # Column index for n in region j
  nn <- 2 + (j - 1) * 3
  
  # Column index for M in region j
  MM <- 4 + (j - 1) * 3
  
  # Build reduced dataframe
  ocupancia_M <- data.frame(
    year = data_ocupancia[, 1],
    n    = data_ocupancia[, nn],
    M    = data_ocupancia[, MM]
  )
  
  # Filter rows where M > M_min
  ocupancia_M <- ocupancia_M[ocupancia_M$M > M_min, ]
  
  # Directly compute output
  ocupancia_year_p <- data.frame(
    year = ocupancia_M$year,
    p    = ocupancia_M$n / ocupancia_M$M
  )
  
  return(ocupancia_year_p)
}

for (i in 1:12 ) {
  # Dades ocupancia species i en les tres regions. 
  data_ocupancia <- as.data.frame(my_list[[i]])
  
  # Activate the next line if you consider only the period 2004-2024.
  # (otherwise comment it out, and results will be generatedfor the whole period 1994-2024)
  data_ocupancia <- data_ocupancia[data_ocupancia$year >= 2004, ]
  
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
      
      # Contrium data_ocupancia_filtrat (M > 10) 
      # function (data_occupancia, M, j) i et retorna una data_ocupancia_M_Filtrada 
      
      ocupancies_year_p <- Occupancia_M_Filtering_Function(data_ocupancia, j, 10)
      
      if (nrow(ocupancies_year_p) >= 5) {
        
        model <- lm(p ~ year, data = ocupancies_year_p)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        
      } else {
        
        slope <- NA
        pval  <- NA
      }
      
      if( j == 1) {
        results_1 <- rbind(results_1,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 2 ) {
        results_2 <- rbind(results_2,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 3) {
        results_3 <- rbind(results_3,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
    }
  }
}

results_1$trend <- ifelse(results_1$p_value < 0.05 & results_1$slope > 0,
                          "Significant increase",
                          ifelse(results_1$p_value < 0.05 & results_1$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

results_2$trend <- ifelse(results_2$p_value < 0.05 & results_2$slope > 0,
                          "Significant increase",
                          ifelse(results_2$p_value < 0.05 & results_2$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

results_3$trend <- ifelse(results_3$p_value < 0.05 & results_3$slope > 0,
                          "Significant increase",
                          ifelse(results_3$p_value < 0.05 & results_3$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))



######################################################

# 1. Calcular el cambio total y clasificar con todas las categorías
df_grafico <- results_1 %>%
  mutate(total_change = slope * 31) %>%
  mutate(Categoria = case_when(
    p_value >= 0.05 ~ "Estable (No Sig.)",
    p_value < 0.05 & total_change <= -0.30 ~ "Descens Fort (>30%)",
    p_value < 0.05 & total_change > -0.30 & total_change < 0 ~ "Descens Moderat (<30%)",
    p_value < 0.05 & total_change > 0 & total_change <= 0.30 ~ "Augment Moderat (<30%)",
    p_value < 0.05 & total_change > 0.30 ~ "Augment Fort (>30%)",
    TRUE ~ "Altres"
  ))

# 2. Definir la paleta de colores completa (Semáforo ampliado)
mis_colores <- c(
  "Descens Fort (>30%)" = "#B22222",      # Rojo Intenso
  "Descens Moderat (<30%)" = "#FF6B6B",   # Rojo Claro
  "Estable (No Sig.)" = "#D3D3D3",        # Gris
  "Augment Moderat (<30%)" = "#90EE90",   # Verde Claro
  "Augment Fort (>30%)" = "#228B22"       # Verde Oscuro
)

# 3. Forzar el orden de las especies (usando el vector que ya tenemos)
df_grafico$species <- factor(df_grafico$species, levels = species_order)

# 4. Generar el Gráfico de Tendencias Final
trend_ocu_BR1 <- ggplot(df_grafico, aes(x = species, y = total_change, fill = Categoria)) +
  geom_col(color = "black", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +  # Línea de equilibrio
  geom_hline(yintercept = c(-0.30, 0.30), linetype = "dashed", color = "gray40") + # Líneas de umbral 30%
  scale_fill_manual(values = mis_colores) +
  labs(
    title = "",
    x = "Espècies",
    y = "Canvi total estimat (Slope * 31)",
    fill = "Categoria d'Estatus"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "gray90")
  )

print(trend_ocu_BR1)

#######################################################################


df_grafico2 <- results_2 %>%
  mutate(total_change = slope * 31) %>%
  mutate(Categoria = case_when(
    p_value >= 0.05 ~ "Estable (No Sig.)",
    p_value < 0.05 & total_change <= -0.30 ~ "Descens Fort (>30%)",
    p_value < 0.05 & total_change > -0.30 & total_change < 0 ~ "Descens Moderat (<30%)",
    p_value < 0.05 & total_change > 0 & total_change <= 0.30 ~ "Augment Moderat (<30%)",
    p_value < 0.05 & total_change > 0.30 ~ "Augment Fort (>30%)",
    TRUE ~ "Altres"
  ))

# 3. Forzar el orden de las especies (usando el vector que ya tenemos)
df_grafico2$species <- factor(df_grafico2$species, levels = species_order)

# 4. Generar el Gráfico de Tendencias Final
trend_ocu_BR2 <- ggplot(df_grafico2, aes(x = species, y = total_change, fill = Categoria)) +
  geom_col(color = "black", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +  # Línea de equilibrio
  geom_hline(yintercept = c(-0.30, 0.30), linetype = "dashed", color = "gray40") + # Líneas de umbral 30%
  scale_fill_manual(values = mis_colores) +
  labs(
    title = "",
    x = "Espècies",
    y = "Canvi total estimat (Slope * 31)",
    fill = "Categoria d'Estatus"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "gray90")
  )

print(trend_ocu_BR2)
########################################################################


df_grafico3 <- results_3 %>%
  mutate(total_change = slope * 31) %>%
  mutate(Categoria = case_when(
    p_value >= 0.05 ~ "Estable (No Sig.)",
    p_value < 0.05 & total_change <= -0.30 ~ "Descens Fort (>30%)",
    p_value < 0.05 & total_change > -0.30 & total_change < 0 ~ "Descens Moderat (<30%)",
    p_value < 0.05 & total_change > 0 & total_change <= 0.30 ~ "Augment Moderat (<30%)",
    p_value < 0.05 & total_change > 0.30 ~ "Augment Fort (>30%)",
    TRUE ~ "Altres"
  ))

# 3. Forzar el orden de las especies (usando el vector que ya tenemos)
df_grafico3$species <- factor(df_grafico3$species, levels = species_order)

# 4. Generar el Gráfico de Tendencias Final
trend_ocu_BR3 <- ggplot(df_grafico3, aes(x = species, y = total_change, fill = Categoria)) +
  geom_col(color = "black", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +  # Línea de equilibrio
  geom_hline(yintercept = c(-0.30, 0.30), linetype = "dashed", color = "gray40") + # Líneas de umbral 30%
  scale_fill_manual(values = mis_colores) +
  labs(
    title = "Regio Mediterrania arida",
    x = "Espècies",
    y = "Canvi total estimat (Slope * 31)",
    fill = "Categoria d'Estatus"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "gray90")
  )

print(trend_ocu_BR3)

######################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#CREACIO DEL PANELL AMB LES TENDENCIES DE LES TRES REGIONS:

# 1. Asegurar que tenemos la lista completa de especies sin NAs
lista_especies_completa <- species_order[!is.na(species_order)]

# 2. Función para limpiar cada dataframe antes de graficar,
# Esta función asegura que todas las especies estén presentes (aunque sea con valor 0

preparar_df <- function(df, orden) {
df %>%
complete(species = orden) %>% # Rellena las especies faltantes
mutate(
total_change = ifelse(is.na(slope), 0, slope * 31),
Categoria = ifelse(is.na(Categoria), "Estable (No Sig.)", Categoria)
) %>%
mutate(species = factor(species, levels = orden)) %>%
filter(!is.na(species)) # Elimina cualquier fila que sea realmente NA
}
preparar_df <- function(df, orden) {
  df %>%
    complete(species = orden) %>% # Rellena las especies faltantes
    mutate(
      total_change = ifelse(is.na(slope), 0, slope * 31),
      Categoria = ifelse(is.na(Categoria), "Estable (No Sig.)", Categoria)
    ) %>%
    mutate(species = factor(species, levels = orden)) %>%
    filter(!is.na(species)) # Elimina cualquier fila que sea realmente NA
}


# 3. Aplicar la limpieza a tus 3 dataframes
df1_final <- preparar_df(df_grafico, lista_especies_completa)
df2_final <- preparar_df(df_grafico2, lista_especies_completa)
df3_final <- preparar_df(df_grafico3, lista_especies_completa)

# 4. Definir el estilo visual minimalista
estilo_eje_y <- theme_bw() + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic", size = 8),
  legend.position = "none", # Quitamos leyendas individuales
  plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
)

# 5. Crear los 3 gráficos con ejes coordinados

g1 <- ggplot(df1_final, aes(x = species, y = total_change, fill = Categoria)) +
  geom_col(color = "black") +
  scale_fill_manual(values = mis_colores) +
  labs(title = "Regió Alpina i Subalpina", y = "Canvi total estimat (Pendent*31)") +
  estilo_eje_y

g2 <- ggplot(df2_final, aes(x = species, y = total_change, fill = Categoria)) +
  geom_col(color = "black") +
  scale_fill_manual(values = mis_colores) +
  labs(title = "Regió Mediterrània Humida", y = "") +
  estilo_eje_y +
  theme(axis.title.y = element_blank())

g3 <- ggplot(df3_final, aes(x = species, y = total_change, fill = Categoria)) +
  geom_col(color = "black") +
  scale_fill_manual(values = mis_colores) +
  labs(title = "Regió Mediterrània Àrida", y = "") +
  estilo_eje_y +
  theme(axis.title.y = element_blank())

# 6. Unificar con Patchwork
library(patchwork)
panell_tendencies <- (g1 | g2 | g3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Estatus de la Tendència en ocupància als darrers 31 anys",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  ) & theme(legend.position = "bottom")

print(panell_tendencies)


# 6. Guardarlo en tu carpeta (ajustando el tamaño para que quepa todo)
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/Extincions/Tendencies_ocupancies.png",
       plot = panell_tendencies, width = 14, height = 8, dpi = 300)