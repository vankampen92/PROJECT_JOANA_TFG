yearly_df$countBR1 <- yearly_counts_BR1_df$count
yearly_df$countBR2 <- yearly_counts_BR2_df$count
yearly_df$countBR3 <- yearly_counts_BR3_df$count

library(ggplot2)
library(tidyr)
library(dplyr)

# Supongamos que tu dataframe se llama df
# Si no está en formato largo, lo pasamos a formato tidy:
df_long <- yearly_df %>%
  pivot_longer(
    cols = starts_with("count"),
    names_to = "Bioregion",
    values_to = "Itinerarios"
  ) %>%
  mutate(
    Bioregion = recode(Bioregion,
                       "count" = "Total CBMS",
                       "countBR1" = "AS",
                       "countBR2" = "MH",
                       "countBR3" = "MA"),
    Bioregion = factor(Bioregion, levels = c("Total CBMS", "AS", "MH", "MA"))
  )

itin_plot <- ggplot(df_long, aes(x = year, y = Itinerarios, color = Bioregion)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  labs(
    x = "Any",
    y = "Nombre d'itineraris",
    color = ""
  ) +
  scale_color_manual(values = c(
    "Total CBMS" = "black",
    "AS" = "#7570b3",
    "MH" = "#1b9e77",
    "MA" = "#d95f02"
  )) +
  scale_x_continuous(breaks = seq(1994, 2024, by = 5)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

print(itin_plot)

ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/itineraris_bioregio_plot.png",
       width = 8, height = 5, dpi = 300)

