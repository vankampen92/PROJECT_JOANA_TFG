# Cargamos los datos
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

itin_CBMS_RegClim <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/itin_CBMS_regionsclimatiques.csv"
                              , sep ='\t' )

#carregar paquets
library(vegan)
library(tidyverse)
library(island)
library(data.table)
library(openxlsx)

# Selecting the intenaris per bioclimatic region:
itin_CBMS_RegClim_1 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 1, ]
itin_CBMS_RegClim_2 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 2, ]
itin_CBMS_RegClim_3 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 3, ]
itin_ID_1 <- itin_CBMS_RegClim_1$CODI
itin_ID_2 <- itin_CBMS_RegClim_2$CODI
itin_ID_3 <- itin_CBMS_RegClim_3$CODI


  
data_celastrina <-
  data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Lycaena <-
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Vanessa <-
  data %>% filter(sp_latin == "Vanessa cardui") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pseudophilotes <-
  data %>% filter(sp_latin == "Pseudophilotes panoptes") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Aglais <-
  data %>% filter(sp_latin == "Aglais io") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pararge <-
  data %>% filter(sp_latin == "Pararge aegeria") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaCeci <-
  data %>% filter(sp_latin == "Pyronia cecilia") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaBath <-
  data %>% filter(sp_latin == "Pyronia bathseba") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Anthocharis <-
  data %>% filter(sp_latin == "Anthocharis euphenoides") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Cyaniris <-
  data %>% filter(sp_latin == "Cyaniris semiargus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))
###################################

Samplying_Years <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/cbms_sampling_years.csv")
# Create the presence matrix with SITE_IDs as rows and years as columns
presence_matrix <- Samplying_Years %>%
  pivot_wider(names_from = year, values_from = presence, values_fill = 0)

# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts <- colSums(presence_matrix[,-1])

# Convert named numeric vector to data frame
yearly_df <- enframe(yearly_counts, name = "year", value = "count")

# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_df$year <- as.numeric(as.character(yearly_df$year))


################################################################################
# Calcul de les ocupancies de les 12 especies. 


# Celastrina argiolus
##########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_celastrina <- colSums(data_celastrina[,-1])
# Convert named numeric vector to data frame
yearly_presence_celastrina_df <- enframe(yearly_presence_celastrina, name = "year", value = "count")
presence_94_2024_celastrina_df <- yearly_presence_celastrina_df[-c(1:3), ]
#prepararmos el dataframe para el calculo de las ocupancias#
presence_94_2024_celastrina_df$No_of_IT <- yearly_df$count
presence_94_2024_celastrina_df$occupancy <- presence_94_2024_celastrina_df$count/presence_94_2024_celastrina_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico despues
presence_94_2024_celastrina_df$year <- as.numeric(presence_94_2024_celastrina_df$year)
##Calculem regresio lineal simple
model_cela <- lm(presence_94_2024_celastrina_df$occupancy ~ year, data = presence_94_2024_celastrina_df)
summary(model_cela)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_cela <- summary(model_cela)$r.squared
r_2_cela_text <- paste0("R² = ", round(r2value_cela, 2))
##
gg_occupancy_cela <-
  ggplot(data = presence_94_2024_celastrina_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "orangered") + # Línea de tendencia lineal sin error estándar
  labs(title = "Celastrina argiolus  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_celastrina_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_celastrina_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_cela_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##


#Cyaniris semiargus
##########################
yearly_presence_cyaniris<- colSums(data_celastrina[,-1])
yearly_presence_cyaniris_df <- enframe(yearly_presence_cyaniris, name = "year", value = "count")
presence_94_2024_cyaniris_df <- yearly_presence_cyaniris_df[-c(1:3), ]
presence_94_2024_cyaniris_df$No_of_IT <- yearly_df$count
presence_94_2024_cyaniris_df$occupancy <- presence_94_2024_cyaniris_df$count/presence_94_2024_cyaniris_df$No_of_IT
presence_94_2024_cyaniris_df$year <- as.numeric(presence_94_2024_cyaniris_df$year)
##Calculem regresio lineal simple
model_cyani <- lm(presence_94_2024_cyaniris_df$occupancy ~ year, data = presence_94_2024_cyaniris_df)
summary(model_cyani)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_cyani <- summary(model_cyani)$r.squared
r_2_cyani_text <- paste0("R² = ", round(r2value_cyani, 2))
##
gg_occupancy_cyani <-
  ggplot(data = presence_94_2024_cyaniris_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "mediumblue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Cyaniris semiargus ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_cyaniris_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_cyaniris_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_cyani_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##
##


# Lycaena virgareae
###########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_lycaena <- colSums(data_Lycaena[,-1])
# Convert named numeric vector to data frame
yearly_presence_lycaena_df <- enframe(yearly_presence_lycaena, name = "year", value = "count")
# Aquest data frame no te tots els anys, per n'hi ha alguns on l'especie no va ser observada en cap itinerari. 
# Les comandes seguents son per afegir 0 els anys on l'especie no va ser observada en cap itinerari.
library(dplyr)
# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_lycaena_df$year), by = 1))
yearly_presence_lycaena_df$year <- as.numeric(as.character(yearly_presence_lycaena_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_lycaena_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_lycaena_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_lycaena_df <- yearly_presence_lycaena_df_complete
presence_94_2024_lycaena_df$No_of_IT <- yearly_df$count
presence_94_2024_lycaena_df$occupancy <- presence_94_2024_lycaena_df$count/presence_94_2024_lycaena_df$No_of_IT
presence_94_2024_lycaena_df$year <- as.numeric(presence_94_2024_lycaena_df$year)
##Calculem regresio lineal simple
model_lycaena <- lm(presence_94_2024_lycaena_df$occupancy ~ year, data = presence_94_2024_lycaena_df)
summary(model_lycaena)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_lycaena <- summary(model_lycaena)$r.squared
r_2_lycaena_text <- paste0("R² = ", round(r2value_lycaena, 2))
##
gg_occupancy_lyca <-
  ggplot(data = presence_94_2024_lycaena_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "yellow") + # Línea de tendencia lineal sin error estándar
  labs(title = "Lycaena virgaureae  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = min(presence_94_2024_lycaena_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_lycaena_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_lycaena_text,
           hjust = 0, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##

# Plebejus argus
##########################
yearly_presence_plebejus <- colSums(data_Plebejus[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_df <- enframe(yearly_presence_plebejus, name = "year", value = "count")
presence_94_2024_plebejus_df <- yearly_presence_plebejus_df[-c(1:3), ]
presence_94_2024_plebejus_df$No_of_IT <- yearly_df$count
presence_94_2024_plebejus_df$occupancy <- presence_94_2024_plebejus_df$count/presence_94_2024_plebejus_df$No_of_IT
presence_94_2024_plebejus_df$year <- as.numeric(presence_94_2024_plebejus_df$year)
##Calculem regresio lineal simple
model_plebejus <- lm(presence_94_2024_plebejus_df$occupancy ~ year, data = presence_94_2024_plebejus_df)
summary(model_plebejus)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_plebejus <- summary(model_plebejus)$r.squared
r_2_plebejus_text <- paste0("R² = ", round(r2value_plebejus, 2))
##
gg_occupancy_plebe <-
  ggplot(data = presence_94_2024_plebejus_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue4") + # Línea de tendencia lineal sin error estándar
  labs(title = "Plebejus argus  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_plebejus_df$year)+0.05, # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_plebejus_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_plebejus_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##


#Vanessa cardui
#######################
#crear un vector numeric amb el numero d'itineraris per any en que s'ha observat l'sp.
yearly_presence_vanessa <-colSums(data_Vanessa[,-1])
#convertir aquest vector en un data frame
yearly_presence_vanessa_df <- enframe(yearly_presence_vanessa, name = "year", value = "count")
#eliminem les 3 primeres files
presence_94_2024_vanessa_df <- yearly_presence_vanessa_df[-c(1:3), ]
#Afegir la columna de sampling years per calcular la ocupancia
presence_94_2024_vanessa_df$No_of_IT <- yearly_df$count
#Creem una columna que es el resultat de calcular la ocupancia
presence_94_2024_vanessa_df$occupancy <- presence_94_2024_vanessa_df$count/presence_94_2024_vanessa_df$No_of_IT
presence_94_2024_vanessa_df$year <- as.numeric(presence_94_2024_vanessa_df$year)
##Calculem regresio lineal simple
model_vane <- lm(presence_94_2024_vanessa_df$occupancy ~ year, data = presence_94_2024_vanessa_df)
summary(model_vane)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_vane <- summary(model_vane)$r.squared
r_2_vane_text <- paste0("R² = ", round(r2value_vane, 2))
##
gg_occupancy_vane <-
  ggplot(data = presence_94_2024_vanessa_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gold2") + 
  labs(title = "Vanessa cardui  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_vanessa_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = min(presence_94_2024_vanessa_df$occupancy)+0.05, # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_vane_text,
           hjust = 1, vjust = 0, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##


#Pseudophilotes panoptes
############################
yearly_presence_pseudophilotes <-colSums(data_Pseudophilotes[,-1])
yearly_presence_pseudophilotes_df <- enframe(yearly_presence_pseudophilotes, name = "year", value = "count")
presence_94_2024_pseudophilotes_df <- yearly_presence_pseudophilotes_df
presence_94_2024_pseudophilotes_df$No_of_IT <- yearly_df$count
presence_94_2024_pseudophilotes_df$occupancy <- presence_94_2024_pseudophilotes_df$count/presence_94_2024_pseudophilotes_df$No_of_IT
presence_94_2024_pseudophilotes_df$year <- as.numeric(presence_94_2024_pseudophilotes_df$year)
##Calculem regresio lineal simple
model_pseudophilotes <- lm(presence_94_2024_pseudophilotes_df$occupancy ~ year, data = presence_94_2024_pseudophilotes_df)
summary(model_pseudophilotes)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_pseudophilotes <- summary(model_pseudophilotes)$r.squared
r_2_pseudophilotes_text <- paste0("R² = ", round(r2value_pseudophilotes, 2))
##
gg_occupancy_pseudo <-
  ggplot(data = presence_94_2024_pseudophilotes_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE , color = "darkblue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pseudophilotes panoptes  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_pseudophilotes_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_pseudophilotes_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_pseudophilotes_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##


#Aglais io
#############################
yearly_presence_aglais <-colSums(data_Aglais[,-1])
yearly_presence_aglais_df <- enframe(yearly_presence_aglais, name = "year", value = "count")
presence_94_2024_aglais_df <- yearly_presence_aglais_df[-c(1:3), ]
presence_94_2024_aglais_df$No_of_IT <- yearly_df$count
presence_94_2024_aglais_df$occupancy <- presence_94_2024_aglais_df$count/presence_94_2024_aglais_df$No_of_IT
presence_94_2024_aglais_df$year <- as.numeric(presence_94_2024_aglais_df$year)
##Calculem regresio lineal simple
model_aglais <- lm(presence_94_2024_aglais_df$occupancy ~ year, data = presence_94_2024_aglais_df)
summary(model_aglais)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_aglais <- summary(model_aglais)$r.squared
r_2_aglais_text <- paste0("R² = ", round(r2value_aglais, 2))
##
gg_occupancy_aglais <-
  ggplot(data = presence_94_2024_aglais_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE , color = "blueviolet") + 
  labs(title = "Aglais io  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_aglais_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_aglais_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_aglais_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##


#Melanargia occitanica
#############################
yearly_presence_melanargia <-colSums(data_Melanargia[,-1])
yearly_presence_melanargia_df <- enframe(yearly_presence_melanargia, name = "year", value = "count")
presence_94_2024_melanargia_df <- yearly_presence_melanargia_df[-1, ]
presence_94_2024_melanargia_df$No_of_IT <- yearly_df$count
presence_94_2024_melanargia_df$occupancy <- presence_94_2024_melanargia_df$count/presence_94_2024_melanargia_df$No_of_IT
presence_94_2024_melanargia_df$year <- as.numeric(presence_94_2024_melanargia_df$year)
##Calculem regresio lineal simple
model_melanargia <- lm(presence_94_2024_melanargia_df$occupancy ~ year, data = presence_94_2024_melanargia_df)
summary(model_melanargia)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_melanargia <- summary(model_melanargia)$r.squared
r_2_melanargia_text <- paste0("R² = ", round(r2value_melanargia, 2))
##
gg_occupancy_mela <- 
  ggplot(data = presence_94_2024_melanargia_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE , color = "deepskyblue1") + # Línea de tendencia lineal sin error estándar
  labs(title = "Melanargia occitanica ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_melanargia_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_melanargia_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_melanargia_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##

#Pararge aegeria
############################
yearly_presence_pararge <-colSums(data_Pararge[,-1])
yearly_presence_pararge_df <- enframe(yearly_presence_pararge, name = "year", value = "count")
presence_94_2024_pararge_df <- yearly_presence_pararge_df[-c(1:3), ]
presence_94_2024_pararge_df$No_of_IT <- yearly_df$count
presence_94_2024_pararge_df$occupancy <- presence_94_2024_pararge_df$count/presence_94_2024_pararge_df$No_of_IT
presence_94_2024_pararge_df$year <- as.numeric(presence_94_2024_pararge_df$year)
##Calculem regresio lineal simple
model_pararge <- lm(presence_94_2024_pararge_df$occupancy ~ year, data = presence_94_2024_pararge_df)
summary(model_pararge)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_pararge <- summary(model_pararge)$r.squared
r_2_pararge_text <- paste0("R² = ", round(r2value_pararge, 2))
##
##
gg_occupancy_pararge <-
  ggplot(data = presence_94_2024_pararge_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red4") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pararge aegeria  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_pararge_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_pararge_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_pararge_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##


#Pyronia cecilia
###########################
yearly_presence_pyroniaceci <-colSums(data_PyroniaCeci[,-1])
yearly_presence_pyroniaceci_df <- enframe(yearly_presence_pyroniaceci, name = "year", value = "count")
presence_94_2024_pyroniaceci_df <- yearly_presence_pyroniaceci_df[-c(1:3), ]
presence_94_2024_pyroniaceci_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniaceci_df$occupancy <- presence_94_2024_pyroniaceci_df$count/presence_94_2024_pyroniaceci_df$No_of_IT
#Convertimos 'year' en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniaceci_df$year <- as.numeric(presence_94_2024_pyroniaceci_df$year)
##calculem regressio lineal simple
model_pyroceci <- lm(presence_94_2024_pyroniaceci_df$occupancy ~ year, data = presence_94_2024_pyroniaceci_df)
summary(model_pyroceci)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_pyroceci <- summary(model_pyroceci)$r.squared
r_2_pyroceci_text <- paste0("R² = ", round(r2value_pyroceci, 2))
##
##
gg_occupancy_pyroceci <-
  ggplot(data = presence_94_2024_pyroniaceci_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "palevioletred1") + 
  labs(title = "Pyronia cecilia  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(), 
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_pyroniaceci_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_pyroniaceci_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_pyroceci_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente
##



#Pyronia bathseba
###########################
yearly_presence_pyroniabath <-colSums(data_PyroniaBath[,-1])
yearly_presence_pyroniabath_df <- enframe(yearly_presence_pyroniabath, name = "year", value = "count")
presence_94_2024_pyroniabath_df <- yearly_presence_pyroniabath_df
presence_94_2024_pyroniabath_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniabath_df$occupancy <- presence_94_2024_pyroniabath_df$count/presence_94_2024_pyroniabath_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniabath_df$year <- as.numeric(presence_94_2024_pyroniabath_df$year)
##Calculem regresio lineal simple
model_pyrobath <- lm(presence_94_2024_pyroniabath_df$occupancy ~ year, data = presence_94_2024_pyroniabath_df)
summary(model_pyrobath)
#guardamos el valor de r2 para incorporarlo al grafico
r2value_pyrobath <- summary(model_pyrobath)$r.squared
r_2_pyrobath_text <- paste0("R² = ", round(r2value_pyrobath, 2))
##

##
gg_occupancy_Pyrobath <-
  ggplot(data = presence_94_2024_pyroniabath_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "violetred") +
  labs(title = "Pyronia bathseba  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())+
  annotate("text",
           x = max(presence_94_2024_pyroniabath_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_pyroniabath_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_pyrobath_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente

##


#Anthocharis euphenoides
##########################
yearly_presence_anthocharis <-colSums(data_Anthocharis[,-1])
yearly_presence_anthocharis_df <- enframe(yearly_presence_anthocharis, name = "year", value = "count")
presence_94_2024_anthocharis_df <- yearly_presence_anthocharis_df
presence_94_2024_anthocharis_df$No_of_IT <- yearly_df$count
presence_94_2024_anthocharis_df$occupancy <- presence_94_2024_anthocharis_df$count/presence_94_2024_anthocharis_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_anthocharis_df$year <- as.numeric(presence_94_2024_anthocharis_df$year)
###
model_antho <- lm(presence_94_2024_anthocharis_df$occupancy ~ year, data = presence_94_2024_anthocharis_df)
summary(model_antho)
r2value_antho <- summary(model_antho)$r.squared
r_2_antho_text <- paste0("R² = ", round(r2value_antho, 2))
##
gg_occupancy_Antho <-
  ggplot(data = presence_94_2024_anthocharis_df, aes(x = year, y = occupancy)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE , color = "cadetblue2") + 
  labs(title = "Anthocharis euphenoides  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text",
           x = max(presence_94_2024_anthocharis_df$year), # Puedes ajustar esto al inicio de tu eje X o un valor específico
           y = max(presence_94_2024_anthocharis_df$occupancy), # Puedes ajustar esto al final de tu eje Y o un valor específico
           label = r_2_antho_text,
           hjust = 1, vjust = 1, # Ajusta justificación para que el texto empiece en (x,y)
           size = 2.5, fontface = "bold") # Puedes ajustar el tamaño y estilo de la fuente

print(gg_occupancy_Antho)
#END CALCUL OCUPANCIES PER ESPECIES##################################

#####################
#
#
#
#
# Identify the range of years
start_year <- min(yearly_df$year)
end_year <- max(yearly_df$year)

# Generate axis breaks: every 5 years, plus start and end if not already included
breaks <- sort(unique(c(seq(from = start_year, to = end_year, by = 5), start_year, end_year)))
breaks_ocupancia <- sort(unique(c(seq(from = start_year, to = end_year, by = 4), start_year, end_year)))
breaks_ocupancia_general <- c(start_year, end_year)


#Comenc,en les comandes dels 12 grafics de les ocupancies per especie pel total dels itineraris:#
library(ggplot2)

###############################

#Grafico combinado ocupancias 12 spp
install.packages("patchwork")
library(patchwork)

gg_occupancies_total <- (gg_occupancy_pseudo + gg_occupancy_lyca + gg_occupancy_plebe + gg_occupancy_cyani +
                           gg_occupancy_vane + gg_occupancy_cela + gg_occupancy_aglais + gg_occupancy_mela +
                           gg_occupancy_pararge + gg_occupancy_pyroceci + gg_occupancy_Pyrobath + gg_occupancy_Antho) +
  plot_layout(ncol = 4) + # Especifica 4 columnas
  plot_annotation(
    title = 'Ocupàncies totals per espècie ',
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.title.x = element_text(margin = margin(t = 10)), # Ajusta margen para X
                  axis.title.y = element_text(margin = margin(r = 10))) # Ajusta margen para Y
  ) & labs(x = "Any", y = "Ocupància")
gg_occupancies_total
ggsave("ocupancies_totals.png", plot = gg_occupancies_total, path = "/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/", width = 15, height = 10, units = "in", dpi = 500)

########################
#Per afegir una columna amb de la ucupancia teorica
colext_Results_df$Occu_teorica <- colext_Results_df$C / (colext_Results_df$C+colext_Results_df$E)


