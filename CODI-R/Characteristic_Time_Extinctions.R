library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_sp_BR_Occ_012.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

###Per carregar el delta de temps caracteristic carrego els df amb els valors per
# cada bioregio

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3.RData")


Sp = c("Celastrina argiolus", "Lycaena vigaureae", "Plebejus argus", 
       "Pseudophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 
BioReg = c("Regió Alpina i Subalpina", 
           "Regió Mediterrània Humida", 
           "Regió Mediterrània Àrida")

Local_Extinction_Counts <- function(ocupancia_012, T_n)
{
  # Input:
  #      . ocupancia_012 es la matriu ocupancies (2 no mostrejat, 1 presencia, 0 absencia):  
  #       It  1994  ...  ...  ... ... 2024
  #       12   2     2    1    0   1   0
  #      . T_n:  patro de 0 seguits a comptabilitzar
  #  
  ocupancia_012$local_extinctions <- apply(ocupancia_012, 1, count_extinctions_row, n = T_n)
  
  N <- sum(ocupancia_012$local_extinctions)
  
  return(N) #Numero d'extincions observades
}

count_extinctions_row <- function(row, n = 3) {
  v <- as.numeric(row)
  
  r <- rle(v == 0)   # TRUE only for zeros
  sum(r$values & (r$lengths >= n))
}

count_extinction_pattern <- function(row, n = 3) {
  v <- as.numeric(row)
  
  count <- 0
  run <- 0
  
  for (i in seq_along(v)) {
    if (v[i] == 0) {
      run <- run + 1
    } else {
      # if run ends here, check if it qualifies
      if (run >= n) {
        # check if preceded by a 1
        if (i - run - 1 >= 1 && v[i - run - 1] == 1) {
          count <- count + 1
        }
      }
      run <- 0
    }
  }
  
  # check if a run ends at the last element
  if (run >= n) {
    if (length(v) - run >= 1 && v[length(v) - run] == 1) {
      count <- count + 1
    }
  }
  
  count
}

Local_Extinction_Pattern <- function(ocupancia_012, T_n)
{
  # Input:
  #      . ocupancia_012 es la matriu ocupancies (2 no mostrejat, 1 presencia, 0 absencia):  
  #       It  1994  ...  ...  ... ... 2024
  #       12   2     2    1    0   1   0
  #      . T_n:  patro de 0 seguits a comptabilitzar
  
  ocupancia_012$local_extinctions <- apply(ocupancia_012, 1, count_extinction_pattern, n = T_n)
  
  N <- sum(ocupancia_012$local_extinctions)
  
  return(N) #Numero d'extincions observades
}


############################## Provatina: Celastrina a BR1#####################################
data_ocupancia <- data_Celastrina_BR1_94_DEF

c   = 0.58
e   = 0.40
T = 1 / (c + e)
T_n = round(4 * T)

N1 <- Local_Extinction_Counts(data_ocupancia, T_n)
N2 <- Local_Extinction_Pattern(data_ocupancia, T_n)

data_ocupancia$local_extinctions <- apply(data_ocupancia, 1, count_extinction_pattern, n = T_n)
###############################################################################################

# Compta el numero de vegades que s'ha observat un exintion pattern del tipus (1 0 0 ... 0 0) 
# en el conjunt d'itineraris que composa la metapoblacio d'una especie en una bioregio

################


### PANEL 3 GRAFICS NOMBRE D-EXTINCIONS PER ITINERARI##########################################

#Transformem rownames en una columna "species"
library(tibble)
n_Extincions_per_IT <- n_Extincions_per_IT %>%
  rownames_to_column(var = "Species")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# 1. Definición de colores
species_colors <- c(
  "Celastrina argiolus" = "#FF4A00",
  "Lycaena vigaureae" = "#F2E600",
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

# 2. Definición del orden
species_order <- c(
  "Celastrina argiolus", "Lycaena vigaureae", "Plebejus argus",
  "Pseudophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui",
  "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica",
  "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia"
)

# 3. Preparar datos y aplicar factor
df_long <- n_Extincions_per_IT %>%
  pivot_longer(
    cols = 2:4,
    names_to = "Region",
    values_to = "Value"
  )

df_long$Species <- factor(df_long$Species, levels = species_order)

# 4. Generar gráfico con Zoom (0.5) y Cursivas
panel_final <- ggplot(df_long, aes(x = Species, y = Value, fill = Species)) +
  geom_col(width = 0.7) +
  facet_wrap(~Region, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_colors) +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(title = "", x = "Espècies", y = "n.º d'extincions/ itinerari", fill = "Espècies") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(face = "italic"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

print(panel_final)
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/Extincions/Panel_Extincions.png", plot = panel_final, width = 8, height = 10, dpi = 300)



##################################################
#PANELL DEL TEMPS CARACTERISTIC

# 1. Definir el orden basado en la columna Species de tu DF actual
species_order <- unique(n_Extincions_per_IT$Species)

# 1. Preparar datos pivotando las columnas T_1, T_2 y T_3
df_temps <- n_Extincions_per_IT %>%
  pivot_longer(
    cols = c("T_1", "T_2", "T_3"),
    names_to = "Regions",
    values_to = "valor"
  )

# 2. Asegurar el orden de las especies y renombrar regiones para el título
df_temps$Species <- factor(df_temps$Species, levels = species_order)
df_temps$Regions <- recode(df_temps$Regions,
                           "T_1" = "Regió Alpina i Subaplina",
                           "T_2" = "Regió Mediterrània Humida",
                           "T_3" = "Regió Mediterrània: Àrida")

# 3. Generar el Gráfico de Tiempos
panell_temps <- ggplot(df_temps, aes(x = Species, y = valor, fill = Species)) +
  geom_col(width = 0.7) +
  facet_wrap(~Regions, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "",
    x = "Espècies",
    y = "Temps característic",
    fill = ""
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

print(panell_temps)
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/Extincions/panell_temps.png",
       plot = panell_temps, width = 8, height = 10, dpi = 300)

