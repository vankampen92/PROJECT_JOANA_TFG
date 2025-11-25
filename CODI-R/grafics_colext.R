#carregar llibreries
library(forcats)

# Per no cargar-nos el codi que generan els altres grafics, generarem una copia que li direm 
# "colext_Results_df_BRn_ordenado_2", que contindra els valors de les 3 bioregions
# per acabar tenint un df que agrupi els valors de c i e de les tres bioregions i aixi 
# graficar els resultats conjuntament.
colext_Results_df_ordenado <- colext_Results_df_ordenado %>% mutate(bioregion = "Total")

colext_Results_df_BR1_ordenado_2 <- colext_Results_df_BR1_ordenado

colext_Results_df_BR1_ordenado_limp <- colext_Results_df_BR1_ordenado_2 %>%
  #afegim una columna que es diu bioregion
   mutate(bioregion = "Alpina i subalpina") %>%
# amb la seguent comanda esborrem els prefixos de la regio per unificarlos
  rename_with(~ str_remove(., "_BR1"), ends_with("_BR1"))

colext_Results_df_BR2_ordenado_2 <- colext_Results_df_BR2_ordenado

colext_Results_df_BR2_ordenado_limp <- colext_Results_df_BR2_ordenado_2 %>%
  mutate(bioregion = "Mediterrània humida") %>%
  rename_with(~ str_remove(., "_BR2"), ends_with("_BR2"))

colext_Results_df_BR3_ordenado_2 <- colext_Results_df_BR3_ordenado

colext_Results_df_BR3_ordenado_limp <- colext_Results_df_BR3_ordenado_2 %>%
  mutate(bioregion = "Mediterrània àrida") %>%
  rename_with(~ str_remove(., "_BR3"), ends_with("_BR3"))

#Combinar els dataframes en 1

colext_results_combined <- bind_rows(colext_Results_df_ordenado, 
                                     colext_Results_df_BR1_ordenado_limp, 
                                     colext_Results_df_BR2_ordenado_limp, 
                                     colext_Results_df_BR3_ordenado_limp)

#reordenar les columnes per bioregio 1, 2, 3, general.
colext_results_combined <- colext_results_combined %>%
  mutate(bioregion = fct_relevel(bioregion, 
                               "Alpina i subalpina", 
                               "Mediterrània àrida", 
                               "Mediterrània humida", 
                               "Total"))
#Una vegada preparat el dataframe ja podem fer el grafic:

# Filtrar para las primeras 4 especies
species_to_plot_1_4 <- unique(colext_results_combined$species)[1:4]
df_subset_1_4 <- colext_results_combined %>%
  filter(species %in% species_to_plot_1_4) %>%
  # Eliminar filas con NA en C o en los límites para que no causen problemas en el gráfico
  drop_na(C, C_low, C_up)

###
#grafic de les 4 primeres especies
# Gráfico para las primeras 4 especies
gg_col_1_4 <-
ggplot(df_subset_1_4, aes(x = species, y = C, fill = bioregion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + # width para el espaciado entre grupos
  geom_errorbar(
    aes(ymin = C_low, ymax = C_up),
    position = position_dodge(width = 0.8), # Misma posición que las barras
    width = 0.25 # Ancho de los "bigotes" de la barra de error
  ) +
  labs(
    x = "",
    y = "Colonització",
    fill = " Regions climàtiques" # Título de la leyenda de color
  ) +
  theme_minimal() + # Un tema limpio para el gráfico
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) + # Rotar etiquetas del eje X
  scale_fill_brewer(palette = "Set2") # Paleta de colores más agradable

print(gg_col_1_4)

# --- Repetir para el siguiente grupo de 4 especies (ej. especies 5-8) ---

species_to_plot_5_8 <- unique(colext_results_combined$species)[5:8]
df_subset_5_8 <- colext_results_combined %>%
  filter(species %in% species_to_plot_5_8) %>%
  drop_na(C, C_low, C_up)

gg_col_5_8 <-
  ggplot(df_subset_5_8, aes(x = species, y = C, fill = bioregion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = C_low, ymax = C_up),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  labs(
    x = "",
    y = "Colonització",
    fill = "Regions climàtiques"
  ) +
  coord_cartesian(ylim = c(0, 4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +
  scale_fill_brewer(palette = "Set2")
print(gg_col_5_8)

# --- Repetir para el siguiente grupo de 4 especies (ej. especies 9-12) ---

species_to_plot_9_12 <- unique(colext_results_combined$species)[9:12]
df_subset_9_12 <- colext_results_combined %>%
  filter(species %in% species_to_plot_9_12) %>%
  drop_na(C, C_low, C_up)

gg_col_9_12 <-
 ggplot(df_subset_9_12, aes(x = species, y = C, fill = bioregion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = C_low, ymax = C_up),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  labs(
    x = "",
    y = "Colonització",
    fill = "Regions climàtiques"
    ) +
   theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +
  scale_fill_brewer(palette = "Set2")
print(gg_col_9_12)

####
#EXTINCIO
###
# Filtrar para las primeras 4 especies
species_to_plot_1_4_ext <- unique(colext_results_combined$species)[1:4]
df_subset_1_4_ext <- colext_results_combined %>%
  filter(species %in% species_to_plot_1_4_ext) %>%
  # Eliminar filas con NA en C o en los límites para que no causen problemas en el gráfico
  drop_na(E, E_low, E_up)

###
#grafic de les 4 primeres especies
# Gráfico para las primeras 4 especies
gg_ext_1_4 <-
  ggplot(df_subset_1_4_ext, aes(x = species, y = E, fill = bioregion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + # width para el espaciado entre grupos
  geom_errorbar(
    aes(ymin = E_low, ymax = E_up),
    position = position_dodge(width = 0.8), # Misma posición que las barras
    width = 0.25 # Ancho de los "bigotes" de la barra de error
  ) +
  labs(
    x = "",
    y = "Extinció",
    fill = " Regions climàtiques" # Título de la leyenda de color
  ) +
  theme_minimal() + # Un tema limpio para el gráfico
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) + # Rotar etiquetas del eje X
  scale_fill_brewer(palette = "Set2") # Paleta de colores más agradable

print(gg_ext_1_4)

# --- Repetir para el siguiente grupo de 4 especies (ej. especies 5-8) ---

species_to_plot_5_8_ext <- unique(colext_results_combined$species)[5:8]
df_subset_5_8_ext <- colext_results_combined %>%
  filter(species %in% species_to_plot_5_8_ext) %>%
  drop_na(E, E_low, E_up)

gg_ext_5_8 <-
  ggplot(df_subset_5_8_ext, aes(x = species, y = E, fill = bioregion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = E_low, ymax = E_up),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  labs(
    x = "",
    y = "Extinció",
    fill = "Regions climàtiques"
  )  +
  coord_cartesian(ylim = c(0, 1.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +
  scale_fill_brewer(palette = "Set2")
print(gg_ext_5_8)

# --- Repetir para el siguiente grupo de 4 especies (ej. especies 9-12) ---

species_to_plot_9_12_ext <- unique(colext_results_combined$species)[9:12]
df_subset_9_12_ext <- colext_results_combined %>%
  filter(species %in% species_to_plot_9_12_ext) %>%
  drop_na(E, E_low, E_up)

gg_ext_9_12 <-
  ggplot(df_subset_9_12_ext, aes(x = species, y = E, fill = bioregion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = E_low, ymax = E_up),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  labs(
    x = "",
    y = "Extinció",
    fill = "Regions climàtiques"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +
  scale_fill_brewer(palette = "Set2")
print(gg_ext_9_12)

#########################
#############################
#Grafics per especie amb c i e separats (24 grafics en total)


library(ggplot2)
library(dplyr)

# --- ⃣ Asegurar orden de las bioregiones ---
colext_results_combined$bioregion <- factor(
  colext_results_combined$bioregion,
  levels = c(
    "Alpina i subalpina",
    "Mediterrània humida",
    "Mediterrània arida",
    "Total"
  )
)

# ---  Crear carpeta donde guardar los gráficos ---
dir.create("graficos_species", showWarnings = FALSE)

# --- Los valores NA de la med. arida tiene que volver

colext_results_combined$bioregion[is.na(colext_results_combined$bioregion)] <- "Mediterrània arida"

# ---  Paleta de colores personalizada ---
colores_bioregion <- c(
  "Alpina i subalpina" = "#1f77b4",   # azul
  "Mediterrània humida" = "#2ca02c",  # verde
  "Mediterrània arida" = "#ff7f0e",   # naranja
  "Total" = "#9467bd"                 # violeta
)

# ---  Lista de especies ---
species_list <- unique(colext_results_combined$species)

# --- Bucle para generar y guardar gráficos ---
for (sp in species_list) {
  
  df_sp <- filter(colext_results_combined, species == sp)
  
  # --- Gráfico Colonización ---
  plot_C <- ggplot(df_sp, aes(x = bioregion, y = C, fill = bioregion)) +
    geom_col(color = "black") +
    geom_errorbar(aes(ymin = C_low, ymax = C_up), width = 0.2) +
    scale_fill_manual(values = colores_bioregion, drop = FALSE) +
    scale_x_discrete(drop = FALSE) +   # <- mantiene todas las bioregiones en orden, aunque falten
    labs(title = paste("Colonización (C) -", sp),
         x = "Bioregión", y = "C") +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  # --- Gráfico Extinción ---
  plot_E <- ggplot(df_sp, aes(x = bioregion, y = E, fill = bioregion)) +
    geom_col(color = "black") +
    geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.2) +
    scale_fill_manual(values = colores_bioregion, drop = FALSE) +
    scale_x_discrete(drop = FALSE) +
    labs(title = paste("Extinción (E) -", sp),
         x = "Bioregión", y = "E") +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  # --- Guardar gráficos ---
  ggsave(filename = paste0("graficos_species/", sp, "_Colonizacion.png"),
         plot = plot_C, width = 6, height = 4, dpi = 300)
  
  ggsave(filename = paste0("graficos_species/", sp, "_Extincion.png"),
         plot = plot_E, width = 6, height = 4, dpi = 300)
  
  cat("✓ Guardados gráficos para:", sp, "\n")
}


##########################################
#############Grafics per especie amb c i e junts (12 grafics en total)
library(ggplot2)
library(dplyr)
library(patchwork)  # para combinar gráficos fácilmente

# --- 1️⃣ Asegurar orden de las bioregiones ---
colext_results_combined$bioregion <- factor(
  colext_results_combined$bioregion,
  levels = c(
    "Alpina i subalpina",
    "Mediterrània humida",
    "Mediterrània àrida",
    "Total"
  )
)

# --- 2️⃣ Crear carpeta de salida ---
dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/graficos_species_cie", showWarnings = FALSE)

colext_results_combined$bioregion[is.na(colext_results_combined$bioregion)] <- "Mediterrània àrida"

# --- 3️⃣ Paleta de colores personalizada ---
colores_bioregion <- c(
  "Alpina i subalpina" = "#1f77b4",   # azul
  "Mediterrània humida" = "#2ca02c",  # verde
  "Mediterrània àrida" = "#ff7f0e",   # naranja
  "Total" = "#9467bd"                 # violeta
)

# --- 4️⃣ Lista de especies ---
species_list <- unique(colext_results_combined$species)

# --- 5️⃣ Bucle para generar gráficos combinados ---
for (sp in species_list) {
  
  df_sp <- filter(colext_results_combined, species == sp)
  
  # --- Colonización ---
  plot_C <- ggplot(df_sp, aes(x = bioregion, y = C, fill = bioregion)) +
    geom_col(color = "black") +
    geom_errorbar(aes(ymin = C_low, ymax = C_up), width = 0.2) +
    scale_fill_manual(values = c(
      "Alpina i subalpina" = "#1f77b4",
      "Mediterrània humida" = "#2ca02c",
      "Mediterrània àrida"  = "#ff7f0e",
      "Total"               = "#9467bd"
    ),
    name = "Bioregió"
    ) +
    labs(y = "Colonització (C)", x = "") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom"
    )
  
  # --- Extinción ---
  plot_E <- ggplot(df_sp, aes(x = bioregion, y = E, fill = bioregion)) +
    geom_col(color = "black") +
    geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.2) +
    scale_fill_manual(values = c(
      "Alpina i subalpina" = "#1f77b4",
      "Mediterrània humida" = "#2ca02c",
      "Mediterrània àrida"  = "#ff7f0e",
      "Total"               = "#9467bd"
    ),
    name = "Bioregió"
    ) +
    labs(y = "Extinció (E)", x = "") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom"
    )
  
  # --- Combinar ambos gráficos ---
  graf_comb <- plot_C + plot_E +
    plot_annotation(title = bquote(italic(.(sp)))) &
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) &
    theme(legend.position = "bottom")
  
  graf_comb <- plot_C + plot_E + 
  plot_layout(guides = "collect") +
  plot_annotation(title = bquote(italic(.(sp)))) &
  theme(legend.position = "bottom")

  
  # --- Guardar gráfico combinado ---
  ggsave(filename = paste0("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/graficos_species_cie/", sp, "_Colonizacion_Extincion.png"),
         plot = graf_comb, width = 10, height = 5, dpi = 300)
  
  cat("✓ Guardado gráfico combinado para:", sp, "\n")
}