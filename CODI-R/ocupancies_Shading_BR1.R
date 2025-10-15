#carregar paquets
library(vegan)
library(tidyverse)
library(island)
library(data.table)
library(openxlsx)

# Cargamos los datos
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

itin_CBMS_RegClim <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/itin_CBMS_regionsclimatiques.csv"
                              , sep ='\t' )

Samplying_Years <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/cbms_sampling_years.csv")

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
presence_matrix_BR1 <- presence_matrix[presence_matrix$SITE_ID %in% itin_ID_1, ]

# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts_BR1 <- colSums(presence_matrix_BR1[,-1])
# Convert named numeric vector to data frame
yearly_counts_BR1_df <- enframe(yearly_counts_BR1, name = "year", value = "count")
# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_counts_BR1_df$year <- as.numeric(as.character(yearly_counts_BR1_df$year))

###
data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Plebejus_BR1 <-data_Plebejus[data_Plebejus$IDitin %in% itin_ID_1, ]
###

#####################
# Identify the range of years
start_year <- min(yearly_df$year)
end_year <- max(yearly_df$year)

# Generate axis breaks: every 5 years, plus start and end if not already included
breaks <- sort(unique(c(seq(from = start_year, to = end_year, by = 5), start_year, end_year)))
breaks_ocupancia <- sort(unique(c(seq(from = start_year, to = end_year, by = 4), start_year, end_year)))
breaks_ocupancia_general <- c(start_year, end_year)


################################################################################
# Calcul de les ocupancies de les 12 especies. 
library(ggplot2)

# Plebejus argus
##########################
yearly_presence_plebejus_BR1 <- colSums(data_Plebejus_BR1[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_BR1_df <- enframe(yearly_presence_plebejus_BR1, name = "year", value = "count")
presence_94_2024_plebejus_BR1_df <- yearly_presence_plebejus_BR1_df[-c(1:3), ]
presence_94_2024_plebejus_BR1_df$No_of_IT <- yearly_counts_BR1_df$count
presence_94_2024_plebejus_BR1_df$occupancy <- presence_94_2024_plebejus_BR1_df$count/presence_94_2024_plebejus_BR1_df$No_of_IT
presence_94_2024_plebejus_BR1_df$year <- as.numeric(presence_94_2024_plebejus_BR1_df$year)
##Calculem regresio lineal simple
model_plebejus_BR1 <- lm(presence_94_2024_plebejus_BR1_df$occupancy ~ year, data = presence_94_2024_plebejus_BR1_df)
summary(model_plebejus_BR1)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_plebejus_BR1 <- summary(model_plebejus_BR1)$r.squared
r_2_plebejus_text_BR1 <- paste0("R² = ", round(r2value_plebejus_BR1, 2))
##
gg_occupancy_plebe_BR1 <-
  ggplot(data = presence_94_2024_plebejus_BR1_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue4") + # Línea de tendencia lineal sin error estándar
  labs(title = "Plebejus argus  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_plebejus_BR1_df$year)+0.05, # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_plebejus_BR1_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_plebejus_text_BR1,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
####
#END CALCUL OCUPANCIES Plebejus argus ##################################
#####################
#
# Funcio R: Ocupancia teorica en funcio del temps quan la condicio initial es una p_0 generica:
p <- function(c, e, t, p_0) {
  value <- (c / (e + c)) * (1 - exp(-(e + c) * t)) + p_0 * exp(-(e + c) * t))
  return(value)
}



