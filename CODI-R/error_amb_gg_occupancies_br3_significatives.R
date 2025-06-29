# Cargamos los datos
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

itin_CBMS_RegClim <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/itin_CBMS_regionsclimatiques.csv"
                              , sep ='\t' )

Samplying_Years <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/cbms_sampling_years.csv")

#carregar paquets
library(vegan)
library(tidyverse)
library(island)
library(data.table)
library(openxlsx)
library(patchwork)

# Selecting the intenaris per bioclimatic region:
itin_CBMS_RegClim_1 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 1, ]
itin_CBMS_RegClim_2 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 2, ]
itin_CBMS_RegClim_3 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 3, ]
itin_ID_1 <- itin_CBMS_RegClim_1$CODI
itin_ID_2 <- itin_CBMS_RegClim_2$CODI
itin_ID_3 <- itin_CBMS_RegClim_3$CODI


# Create the presence matrix with SITE_IDs as rows and years as columns
presence_matrix <- Samplying_Years %>%
  pivot_wider(names_from = year, values_from = presence, values_fill = 0)

# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts <- colSums(presence_matrix[,-1])

# Convert named numeric vector to data frame
yearly_df <- enframe(yearly_counts, name = "year", value = "count")

# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_df$year <- as.numeric(as.character(yearly_df$year))



# Create the presence matrix with SITE_IDs as rows and years as columns
presence_matrix_BR3 <- presence_matrix[presence_matrix$SITE_ID %in% itin_ID_3, ]





# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts_BR3 <- colSums(presence_matrix_BR3[,-1])
# Convert named numeric vector to data frame
yearly_counts_BR3_df <- enframe(yearly_counts_BR3, name = "year", value = "count")
# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_counts_BR3_df$year <- as.numeric(as.character(yearly_counts_BR3_df$year))



data_Celastrina <-
  data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Celastrina_BR3 <-data_Celastrina[data_Celastrina$IDitin %in% itin_ID_3, ]
###

data_Cyaniris <-
  data %>% filter(sp_latin == "Cyaniris semiargus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Cyaniris_BR3 <-data_Cyaniris[data_Cyaniris$IDitin %in% itin_ID_3, ]
###

###
data_Lycaena <-
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Lycaena_BR3 <-data_Lycaena[data_Lycaena$IDitin %in% itin_ID_3, ]
###

###
data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Plebejus_BR3 <-data_Plebejus[data_Plebejus$IDitin %in% itin_ID_3, ]
###

###
data_Vanessa <-
  data %>% filter(sp_latin == "Vanessa cardui") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Vanessa_BR3 <-data_Vanessa[data_Vanessa$IDitin %in% itin_ID_3, ]
###

###
data_Pseudophilotes <-
  data %>% filter(sp_latin == "Pseudophilotes panoptes") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pseudophilotes_BR3 <-data_Pseudophilotes[data_Pseudophilotes$IDitin %in% itin_ID_3, ]
###

###
data_Aglais <-
  data %>% filter(sp_latin == "Aglais io") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Aglais_BR3 <-data_Aglais[data_Aglais$IDitin %in% itin_ID_3, ]
###

###
data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Melanargia_BR3 <-data_Melanargia[data_Melanargia$IDitin %in% itin_ID_3, ]
###

###
data_Pararge <-
  data %>% filter(sp_latin == "Pararge aegeria") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pararge_BR3 <-data_Pararge[data_Pararge$IDitin %in% itin_ID_3, ]
###

###
data_PyroniaCeci <-
  data %>% filter(sp_latin == "Pyronia cecilia") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaCeci_BR3 <-data_PyroniaCeci[data_PyroniaCeci$IDitin %in% itin_ID_3, ]
###

###
data_PyroniaBath <-
  data %>% filter(sp_latin == "Pyronia bathseba") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaBath_BR3 <-data_PyroniaBath[data_PyroniaBath$IDitin %in% itin_ID_3, ]
###

###
data_Anthocharis <-
  data %>% filter(sp_latin == "Anthocharis euphenoides") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Anthocharis_BR3 <-data_Anthocharis[data_Anthocharis$IDitin %in% itin_ID_3, ]
###


# -------------------------------------------------------------------------#
# 1. Definir los años de inicio y fin, y la variable de breaks personalizados
#    (basado en tu imagen, los saltos son de 5 años)
start_year <- 1994
end_year <- 2024
my_custom_breaks <- sort(unique(c(seq(from = start_year, to = end_year, by = 5), start_year, end_year)))
print(my_custom_breaks) # Verifica que se generen correctamente: 1994 1999 2004 ... 2024

# -------------------------------------------------------------------------#
# 2. Recrea tus dataframes de presencia si es necesario,
#    o asegúrate de que están cargados y son correctos.
#    Necesitarás tener 'data_Aglais_BR3', 'yearly_df', etc. disponibles.
#    Si los creas aquí, solo un ejemplo:
#    presence_94_2024_plebejus_BR3_df <- data.frame(year = 1994:2024, occupancy = runif(31))
#    presence_94_2024_aglais_BR3_df <- data.frame(year = 1994:2024, occupancy = runif(31)/2)
#    presence_94_2024_pyroniabath_BR3_df <- data.frame(year = 1994:2024, occupancy = runif(31)/3)

#    Y también define las variables r_2_X_text_BR3 si no las tienes definidas:
#    r_2_plebejus_text_BR3 <- "R² = 0.42"
#    r_2_aglais_text_BR3 <- "R² = 0.68"
#    r_2_pyrobath_text_BR3 <- "R² = 0.31"


# -------------------------------------------------------------------------#
# 3. Define tus gráficos individuales.
#    Es ABSOLUTAMENTE CRÍTICO que usen 'my_custom_breaks' para scale_x_continuous
#    y que labs(x = NULL, y = NULL) esté presente y que NO haya axis.title.x/y = element_blank().
# Plebejus argus
##########################
yearly_presence_plebejus_BR3 <- colSums(data_Plebejus_BR3[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_BR3_df <- enframe(yearly_presence_plebejus_BR3, name = "year", value = "count")
presence_94_2024_plebejus_BR3_df <- yearly_presence_plebejus_BR3_df[-c(1:3), ]
presence_94_2024_plebejus_BR3_df$No_of_IT <- yearly_counts_BR3_df$count
presence_94_2024_plebejus_BR3_df$occupancy <- presence_94_2024_plebejus_BR3_df$count/presence_94_2024_plebejus_BR3_df$No_of_IT
presence_94_2024_plebejus_BR3_df$year <- as.numeric(presence_94_2024_plebejus_BR3_df$year)
##Calculem regresio lineal simple
model_plebejus_BR3 <- lm(presence_94_2024_plebejus_BR3_df$occupancy ~ year, data = presence_94_2024_plebejus_BR3_df)
summary(model_plebejus_BR3)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_plebejus_BR3 <- summary(model_plebejus_BR3)$r.squared
r_2_plebejus_text_BR3 <- paste0("R² = ", round(r2value_plebejus_BR3, 2))
##
gg_occupancy_plebe_BR3 <-
  ggplot(data = presence_94_2024_plebejus_BR3_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE , color = "steelblue4", fill = "steelblue4") +
  labs(title = "Plebejus argus", x = NULL, y = NULL) + # x e y son NULL
  scale_x_continuous(breaks = my_custom_breaks) + # USA my_custom_breaks
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10)) + # NO tiene axis.title.x/y = element_blank()
  annotate("text",
           x = max(presence_94_2024_plebejus_BR3_df$year), # Quitamos el +0.05
           y = max(presence_94_2024_plebejus_BR3_df$occupancy),
           label = r_2_plebejus_text_BR3,
           hjust = 1, vjust = 1,
           size = 2.5, fontface = "bold")
#Aglais io
#############################
yearly_presence_aglais_BR3 <-colSums(data_Aglais_BR3[,-1])
yearly_presence_aglais_BR3_df <- enframe(yearly_presence_aglais_BR3, name = "year", value = "count")
presence_94_2024_aglais_BR3_df <- yearly_presence_aglais_BR3_df[-c(1:3), ]
presence_94_2024_aglais_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_aglais_BR3_df$occupancy <- presence_94_2024_aglais_BR3_df$count/presence_94_2024_aglais_BR3_df$No_of_IT
presence_94_2024_aglais_BR3_df$year <- as.numeric(presence_94_2024_aglais_BR3_df$year)
##Calculem regresio lineal simple
model_aglais_BR3 <- lm(presence_94_2024_aglais_BR3_df$occupancy ~ year, data = presence_94_2024_aglais_BR3_df)
summary(model_aglais_BR3)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_aglais_BR3 <- summary(model_aglais_BR3)$r.squared
r_2_aglais_text_BR3 <- paste0("R² = ", round(r2value_aglais_BR3, 2))
##
gg_occupancy_aglais_BR3 <-
  ggplot(data = presence_94_2024_aglais_BR3_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE , color = "blueviolet", fill = "blueviolet") +
  labs(title = "Aglais io", x = NULL, y = NULL) + # x e y son NULL
  scale_x_continuous(breaks = my_custom_breaks) + # USA my_custom_breaks
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10)) + # NO tiene axis.title.x/y = element_blank()
  annotate("text",
           x = max(presence_94_2024_aglais_BR3_df$year),
           y = max(presence_94_2024_aglais_BR3_df$occupancy),
           label = r_2_aglais_text_BR3,
           hjust = 1, vjust = 1,
           size = 2.5, fontface = "bold")

#Pyronia bathseba
###########################
yearly_presence_pyroniabath_BR3 <-colSums(data_PyroniaBath_BR3[,-1])
yearly_presence_pyroniabath_BR3_df <- enframe(yearly_presence_pyroniabath_BR3, name = "year", value = "count")
presence_94_2024_pyroniabath_BR3_df <- yearly_presence_pyroniabath_BR3_df
presence_94_2024_pyroniabath_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniabath_BR3_df$occupancy <- presence_94_2024_pyroniabath_BR3_df$count/presence_94_2024_pyroniabath_BR3_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniabath_BR3_df$year <- as.numeric(presence_94_2024_pyroniabath_BR3_df$year)
##Calculem regresio lineal simple
model_pyrobath_BR3 <- lm(presence_94_2024_pyroniabath_BR3_df$occupancy ~ year, data = presence_94_2024_pyroniabath_BR3_df)
summary(model_pyrobath_BR3)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_pyrobath_BR3 <- summary(model_pyrobath_BR3)$r.squared
r_2_pyrobath_text_BR3 <- paste0("R² = ", round(r2value_pyrobath_BR3, 2))
##
gg_occupancy_Pyrobath_BR3 <-
  ggplot(data = presence_94_2024_pyroniabath_BR3_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "violetred", fill = "violetred") +
  labs(title = "Pyronia bathseba", x = NULL, y = NULL) + # x e y son NULL
  scale_x_continuous(breaks = my_custom_breaks) + # USA my_custom_breaks
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10)) + # NO tiene axis.title.x/y = element_blank()
  annotate("text",
           x = max(presence_94_2024_pyroniabath_BR3_df$year),
           y = max(presence_94_2024_pyroniabath_BR3_df$occupancy),
           label = r_2_pyrobath_text_BR3,
           hjust = 1, vjust = 1,
           size = 2.5, fontface = "bold")

# -------------------------------------------------------------------------#
# 4. Ensamblar los gráficos con patchwork
gg_occupancies_BR3_significatives <-
  (gg_occupancy_plebe_BR3 + gg_occupancy_aglais_BR3 + gg_occupancy_Pyrobath_BR3) +
  plot_layout(
    ncol = 1,
    axes = 'collect',       # Recoge los ejes para que se compartan si es posible
    axis_titles = 'collect' # Intenta generar un solo título de eje X/Y
  ) +
  plot_annotation(
    title = 'Ocupàncies regió mediterrània àrida',
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ) &
  # Aplica los labs globales de X e Y para todo el patchwork.
  labs(x = "Temps (any)", y = "Ocupància") &
  # Ajusta el estilo de los títulos de los ejes globales.
  theme(
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 12, hjust = 0.5), # Título X abajo y centrado
    axis.title.y = element_text(margin = margin(r = 10), face = "bold", size = 12, hjust = 0.5)  # Título Y a la izquierda y centrado (verticalmente)
  )

# -------------------------------------------------------------------------#
# 5. Muestra el gráfico final
gg_occupancies_BR3_significatives