library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# =========================================================
# 1. Afegir noms d'espècies com columna
# =========================================================

totals <- n_Extincions_per_IT_Total %>%
  rownames_to_column(var = "Species")

ext2024 <- n_Extincions_per_IT_2024 %>%
  rownames_to_column(var = "Species")

# =========================================================
# 2. Calcular extincions abans de 2024
# =========================================================

before2024 <- totals

before2024[,2:4] <- totals[,2:4] - ext2024[,2:4]

# =========================================================
# 3. Passar a format llarg
# =========================================================

totals_long <- totals %>%
  pivot_longer(
    cols = 2:4,
    names_to = "Region",
    values_to = "Total"
  )

before_long <- before2024 %>%
  pivot_longer(
    cols = 2:4,
    names_to = "Region",
    values_to = "Before2024"
  )

ext2024_long <- ext2024 %>%
  pivot_longer(
    cols = 2:4,
    names_to = "Region",
    values_to = "Ext2024"
  )

# =========================================================
# 4. Combinar dades
# =========================================================

df_plot <- before_long %>%
  left_join(ext2024_long,
            by = c("Species", "Region"))

# =========================================================
# 5. Convertir a format llarg per stacked bars
# =========================================================

df_plot_long <- df_plot %>%
  pivot_longer(
    cols = c("Before2024", "Ext2024"),
    names_to = "Type",
    values_to = "Value"
  )

# =========================================================
# 6. Ordre espècies
# =========================================================

species_order <- c(
  "Pseudophilotes panoptes",
  "Cyaniris semiargus",
  "Plebejus argus",
  "Aglais io",
  "Melanargia occitanica",
  "Anthocharis euphenoides",
  "Vanessa cardui",
  "Lycaena virgaureae",
  "Pararge aegeria",
  "Celastrina argiolus",
  "Pyronia bathseba",
  "Pyronia cecilia"
)

df_plot_long$Species <- factor(df_plot_long$Species,
                               levels = species_order)

# =========================================================
# 7. Colors espècies
# =========================================================

species_colors <- c(
  "Celastrina argiolus" = "#FF4A00",
  "Lycaena virgaureae" = "#F2E600",
  "Plebejus argus" = "#4C6A7F",
  "Pseudophilotes panoptes" = "#0B1E8A",
  "Cyaniris semiargus" = "#0A2DBF",
  "Vanessa cardui" = "#D4B000",
  "Aglais io" = "#8A2BE2",
  "Anthocharis euphenoides" = "#8ED1DC",
  "Melanargia occitanica" = "#1CA3D1",
  "Pararge aegeria" = "#8B0000",
  "Pyronia bathseba" = "#C71585",
  "Pyronia cecilia" = "#F48FB1"
)

# =========================================================
# 8. Gràfic stacked
# =========================================================

panel_final <- ggplot(
  df_plot_long,
  aes(
    x = Species,
    y = Value,
    fill = Species,
    alpha = Type
  )
) +

  geom_col(width = 0.7) +

  facet_wrap(~Region, ncol = 1, scales = "free_y") +

  scale_fill_manual(values = species_colors) +

  # transparència diferent per separar tipus
  scale_alpha_manual(
    values = c(
      "Before2024" = 0.45,
      "Ext2024" = 1
    ),
    labels = c(
      "Before2024" = "Abans de 2024",
      "Ext2024" = "Any 2024"
    )
  ) +

  coord_cartesian(ylim = c(0, 1.25)) +

  labs(
    x = "Espècies",
    y = "n.º d'extincions / itinerari",
    alpha = "Tipus d'extinció",
    fill = "Espècies"
  ) +

  theme_bw(base_size = 11) +

  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(face = "italic"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

panel_final
