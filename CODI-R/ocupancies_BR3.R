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



################################################################################
# Calcul de les ocupancies de les 12 especies. 


# Celastrina argiolus
##########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_Celastrina_BR3 <- colSums(data_Celastrina_BR3[,-1])
# Convert named numeric vector to data frame
yearly_presence_Celastrina_BR3_df <- enframe(yearly_presence_Celastrina_BR3, name = "year", value = "count")
presence_94_2024_Celastrina_BR3_df <- yearly_presence_Celastrina_BR3_df[-c(1:3), ]
#prepararmos el dataframe para el calculo de las ocupancias#
presence_94_2024_Celastrina_BR3_df$No_of_IT <- yearly_counts_BR3_df$count
presence_94_2024_Celastrina_BR3_df$occupancy <- presence_94_2024_Celastrina_BR3_df$count/presence_94_2024_Celastrina_BR3_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico despues
presence_94_2024_Celastrina_BR3_df$year <- as.numeric(presence_94_2024_Celastrina_BR3_df$year)

#Cyaniris semiargus
##########################
yearly_presence_cyaniris_BR3<- colSums(data_Cyaniris_BR3[,-1])
yearly_presence_cyaniris_BR3_df <- enframe(yearly_presence_cyaniris_BR3, name = "year", value = "count")

library(dplyr)
# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_cyaniris_BR3_df$year), by = 1))
yearly_presence_cyaniris_BR3_df$year <- as.numeric(as.character(yearly_presence_cyaniris_BR3_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_cyaniris_BR3_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_cyaniris_BR3_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_cyaniris_BR3_df <- yearly_presence_cyaniris_BR3_df_complete
presence_94_2024_cyaniris_BR3_df$No_of_IT <- yearly_counts_BR3_df$count
presence_94_2024_cyaniris_BR3_df$occupancy <- presence_94_2024_cyaniris_BR3_df$count/presence_94_2024_cyaniris_BR3_df$No_of_IT
presence_94_2024_cyaniris_BR3_df$year <- as.numeric(presence_94_2024_cyaniris_BR3_df$year)


# Lycaena virgareae
###########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_lycaena_BR3 <- colSums(data_Lycaena_BR3[,-1])
# Convert named numeric vector to data frame
yearly_presence_lycaena_BR3_df <- enframe(yearly_presence_lycaena_BR3, name = "year", value = "count")
# Aquest data frame no te tots els anys, per n'hi ha alguns on l'especie no va ser observada en cap itinerari. 
# Les comandes seguents son per afegir 0 els anys on l'especie no va ser observada en cap itinerari.
library(dplyr)
# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_lycaena_BR3_df$year), by = 1))
yearly_presence_lycaena_BR3_df$year <- as.numeric(as.character(yearly_presence_lycaena_BR3_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_lycaena_BR3_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_lycaena_BR3_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_lycaena_BR3_df <- yearly_presence_lycaena_BR3_df_complete
presence_94_2024_lycaena_BR3_df$No_of_IT <- yearly_counts_BR3_df$count
presence_94_2024_lycaena_BR3_df$occupancy <- presence_94_2024_lycaena_BR3_df$count/presence_94_2024_lycaena_BR3_df$No_of_IT
presence_94_2024_lycaena_BR3_df$year <- as.numeric(presence_94_2024_lycaena_BR3_df$year)


# Plebejus argus
##########################
yearly_presence_plebejus_BR3 <- colSums(data_Plebejus_BR3[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_BR3_df <- enframe(yearly_presence_plebejus_BR3, name = "year", value = "count")
presence_94_2024_plebejus_BR3_df <- yearly_presence_plebejus_BR3_df[-c(1:3), ]
presence_94_2024_plebejus_BR3_df$No_of_IT <- yearly_counts_BR3_df$count
presence_94_2024_plebejus_BR3_df$occupancy <- presence_94_2024_plebejus_BR3_df$count/presence_94_2024_plebejus_BR3_df$No_of_IT
presence_94_2024_plebejus_BR3_df$year <- as.numeric(presence_94_2024_plebejus_BR3_df$year)


#Vanessa cardui
#######################
#crear un vector numeric amb el numero d'itineraris per any en que s'ha observat l'sp.
yearly_presence_vanessa_BR3 <-colSums(data_Vanessa_BR3[,-1])
#convertir aquest vector en un data frame
yearly_presence_vanessa_BR3_df <- enframe(yearly_presence_vanessa_BR3, name = "year", value = "count")
#eliminem les 3 primeres files
presence_94_2024_vanessa_BR3_df <- yearly_presence_vanessa_BR3_df[-c(1:3), ]
#Afegir la columna de sampling years per calcular la ocupancia
presence_94_2024_vanessa_BR3_df$No_of_IT <- yearly_counts_BR3_df$count
#Creem una columna que es el resultat de calcular la ocupancia
presence_94_2024_vanessa_BR3_df$occupancy <- presence_94_2024_vanessa_BR3_df$count/presence_94_2024_vanessa_BR3_df$No_of_IT
presence_94_2024_vanessa_BR3_df$year <- as.numeric(presence_94_2024_vanessa_BR3_df$year)

#Pseudophilotes panoptes
############################
yearly_presence_pseudophilotes_BR3 <-colSums(data_Pseudophilotes_BR3[,-1])
yearly_presence_pseudophilotes_BR3_df <- enframe(yearly_presence_pseudophilotes_BR3, name = "year", value = "count")
presence_94_2024_pseudophilotes_BR3_df <- yearly_presence_pseudophilotes_BR3_df
presence_94_2024_pseudophilotes_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_pseudophilotes_BR3_df$occupancy <- presence_94_2024_pseudophilotes_BR3_df$count/presence_94_2024_pseudophilotes_BR3_df$No_of_IT
presence_94_2024_pseudophilotes_BR3_df$year <- as.numeric(presence_94_2024_pseudophilotes_BR3_df$year)

#Aglais io
#############################
yearly_presence_aglais_BR3 <-colSums(data_Aglais_BR3[,-1])
yearly_presence_aglais_BR3_df <- enframe(yearly_presence_aglais_BR3, name = "year", value = "count")
presence_94_2024_aglais_BR3_df <- yearly_presence_aglais_BR3_df[-c(1:3), ]
presence_94_2024_aglais_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_aglais_BR3_df$occupancy <- presence_94_2024_aglais_BR3_df$count/presence_94_2024_aglais_BR3_df$No_of_IT
presence_94_2024_aglais_BR3_df$year <- as.numeric(presence_94_2024_aglais_BR3_df$year)

#Melanargia occitanica
#############################
yearly_presence_melanargia_BR3 <-colSums(data_Melanargia_BR3[,-1])
yearly_presence_melanargia_BR3_df <- enframe(yearly_presence_melanargia_BR3, name = "year", value = "count")
presence_94_2024_melanargia_BR3_df <- yearly_presence_melanargia_BR3_df[-1, ]
presence_94_2024_melanargia_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_melanargia_BR3_df$occupancy <- presence_94_2024_melanargia_BR3_df$count/presence_94_2024_melanargia_BR3_df$No_of_IT
presence_94_2024_melanargia_BR3_df$year <- as.numeric(presence_94_2024_melanargia_BR3_df$year)

#Pararge aegeria
############################
yearly_presence_pararge_BR3 <-colSums(data_Pararge_BR3[,-1])
yearly_presence_pararge_BR3_df <- enframe(yearly_presence_pararge_BR3, name = "year", value = "count")
presence_94_2024_pararge_BR3_df <- yearly_presence_pararge_BR3_df[-c(1:3), ]
presence_94_2024_pararge_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_pararge_BR3_df$occupancy <- presence_94_2024_pararge_BR3_df$count/presence_94_2024_pararge_BR3_df$No_of_IT
presence_94_2024_pararge_BR3_df$year <- as.numeric(presence_94_2024_pararge_BR3_df$year)

#Pyronia cecilia
###########################
yearly_presence_pyroniaceci_BR3 <-colSums(data_PyroniaCeci_BR3[,-1])
yearly_presence_pyroniaceci_BR3_df <- enframe(yearly_presence_pyroniaceci_BR3, name = "year", value = "count")
presence_94_2024_pyroniaceci_BR3_df <- yearly_presence_pyroniaceci_BR3_df[-c(1:3), ]
presence_94_2024_pyroniaceci_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniaceci_BR3_df$occupancy <- presence_94_2024_pyroniaceci_BR3_df$count/presence_94_2024_pyroniaceci_BR3_df$No_of_IT
#Convertimos 'year' en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniaceci_BR3_df$year <- as.numeric(presence_94_2024_pyroniaceci_BR3_df$year)

#Pyronia bathseba
###########################
yearly_presence_pyroniabath_BR3 <-colSums(data_PyroniaBath_BR3[,-1])
yearly_presence_pyroniabath_BR3_df <- enframe(yearly_presence_pyroniabath_BR3, name = "year", value = "count")
presence_94_2024_pyroniabath_BR3_df <- yearly_presence_pyroniabath_BR3_df
presence_94_2024_pyroniabath_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniabath_BR3_df$occupancy <- presence_94_2024_pyroniabath_BR3_df$count/presence_94_2024_pyroniabath_BR3_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniabath_BR3_df$year <- as.numeric(presence_94_2024_pyroniabath_BR3_df$year)

#Anthocharis euphenoides
##########################
yearly_presence_anthocharis_BR3 <-colSums(data_Anthocharis_BR3[,-1])
yearly_presence_anthocharis_BR3_df <- enframe(yearly_presence_anthocharis_BR3, name = "year", value = "count")
presence_94_2024_anthocharis_BR3_df <- yearly_presence_anthocharis_BR3_df
presence_94_2024_anthocharis_BR3_df$No_of_IT <- yearly_df$count
presence_94_2024_anthocharis_BR3_df$occupancy <- presence_94_2024_anthocharis_BR3_df$count/presence_94_2024_anthocharis_BR3_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_anthocharis_BR3_df$year <- as.numeric(presence_94_2024_anthocharis_BR3_df$year)

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

gg_occupancy_cela_BR3 <-
  ggplot(data = presence_94_2024_Celastrina_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orangered") + # Línea de tendencia lineal sin error estándar
  labs(title = "Celastrina argiolus  ")  +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_cela_BR3)
##

##
gg_occupancy_cyani_BR3 <-
  ggplot(data = presence_94_2024_cyaniris_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "mediumblue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Cyaniris semiargus ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_cyani_BR3)
##

##
gg_occupancy_lyca_BR3 <-
  ggplot(data = presence_94_2024_lycaena_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") + # Línea de tendencia lineal sin error estándar
  labs(title = "Lycaena virgaureae  ")  +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_lyca_BR3)
##

##
gg_occupancy_plebe_BR3 <-
  ggplot(data = presence_94_2024_plebejus_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue4") + # Línea de tendencia lineal sin error estándar
  labs(title = "Plebejus argus") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_plebe_BR3)
##

##
gg_occupancy_vane_BR3 <-
  ggplot(data = presence_94_2024_vanessa_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "gold2") + # Línea de tendencia lineal sin error estándar
  labs(title = "Vanessa cardui  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_vane_BR3)
##


##
gg_occupancy_pseudo_BR3 <-
  ggplot(data = presence_94_2024_pseudophilotes_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pseudophilotes panoptes  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_pseudo_BR3)
##

##
gg_occupancy_aglais_BR3 <-
  ggplot(data = presence_94_2024_aglais_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blueviolet") + # Línea de tendencia lineal sin error estándar
  labs(title = "Aglais io  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_aglais_BR3)
##

##
gg_occupancy_mela_BR3 <- 
  ggplot(data = presence_94_2024_melanargia_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "deepskyblue1") + # Línea de tendencia lineal sin error estándar
  labs(title = "Melanargia occitanica ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_mela_BR3)
##

##
gg_occupancy_pararge_BR3 <-
  ggplot(data = presence_94_2024_pararge_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red4") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pararge aegeria  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_pararge_BR3)
##

##
gg_occupancy_pyroceci_BR3 <-
  ggplot(data = presence_94_2024_pyroniaceci_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "palevioletred1") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pyronia cecilia  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(), 
                          axis.title.y = element_blank())
print(gg_occupancy_pyroceci_BR3)
##

##
gg_occupancy_Pyrobath_BR3 <-
  ggplot(data = presence_94_2024_pyroniabath_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pyronia bathseba  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_Pyrobath_BR3)
##

##
gg_occupancy_Antho_BR3 <-
  ggplot(data = presence_94_2024_anthocharis_BR3_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "cadetblue2") + # Línea de tendencia lineal sin error estándar
  labs(title = "Anthocharis euphenoides  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
print(gg_occupancy_Antho_BR3)

###############################

#Grafico combinado ocupancias 12 spp
install.packages("patchwork")
library(patchwork)

gg_occupancies_BR3 <- (gg_occupancy_pseudo_BR3 + gg_occupancy_lyca_BR3 + gg_occupancy_plebe_BR3 + gg_occupancy_cyani_BR3 +
                         gg_occupancy_vane_BR3 + gg_occupancy_cela_BR3 + gg_occupancy_aglais_BR3 + gg_occupancy_mela_BR3 +
                         gg_occupancy_pararge_BR3 + gg_occupancy_pyroceci_BR3 + gg_occupancy_Pyrobath_BR3 + gg_occupancy_Antho_BR3) +
  plot_layout(ncol = 4) + # Especifica 4 columnas
  plot_annotation(
    title = 'Ocupàncies regió mediterrània àrida ',
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.title.x = element_text(margin = margin(t = 10)), # Ajusta margen para X
                  axis.title.y = element_text(margin = margin(r = 10))) # Ajusta margen para Y
  ) & labs(x = "Any", y = "Ocupància")
gg_occupancies_BR3
ggsave("ocupancies_BR3.png", plot = gg_occupancies_BR3, path = "/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/", width = 15, height = 10, units = "in", dpi = 300)

########################
#Per afegir una columna amb de la ucupancia teorica
colext_Results_df$Occu_teorica <- colext_Results_df$C / (colext_Results_df$C+colext_Results_df$E)

