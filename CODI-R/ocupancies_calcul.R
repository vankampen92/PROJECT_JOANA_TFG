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

#Cyaniris semiargus
##########################
yearly_presence_cyaniris<- colSums(data_celastrina[,-1])
yearly_presence_cyaniris_df <- enframe(yearly_presence_cyaniris, name = "year", value = "count")
presence_94_2024_cyaniris_df <- yearly_presence_cyaniris_df[-c(1:3), ]
presence_94_2024_cyaniris_df$No_of_IT <- yearly_df$count
presence_94_2024_cyaniris_df$occupancy <- presence_94_2024_cyaniris_df$count/presence_94_2024_cyaniris_df$No_of_IT
presence_94_2024_cyaniris_df$year <- as.numeric(presence_94_2024_cyaniris_df$year)


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


# Plebejus argus
##########################
yearly_presence_plebejus <- colSums(data_Plebejus[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_df <- enframe(yearly_presence_plebejus, name = "year", value = "count")
presence_94_2024_plebejus_df <- yearly_presence_plebejus_df[-c(1:3), ]
presence_94_2024_plebejus_df$No_of_IT <- yearly_df$count
presence_94_2024_plebejus_df$occupancy <- presence_94_2024_plebejus_df$count/presence_94_2024_plebejus_df$No_of_IT
presence_94_2024_plebejus_df$year <- as.numeric(presence_94_2024_plebejus_df$year)


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

#Pseudophilotes panoptes
############################
yearly_presence_pseudophilotes <-colSums(data_Pseudophilotes[,-1])
yearly_presence_pseudophilotes_df <- enframe(yearly_presence_pseudophilotes, name = "year", value = "count")
presence_94_2024_pseudophilotes_df <- yearly_presence_pseudophilotes_df
presence_94_2024_pseudophilotes_df$No_of_IT <- yearly_df$count
presence_94_2024_pseudophilotes_df$occupancy <- presence_94_2024_pseudophilotes_df$count/presence_94_2024_pseudophilotes_df$No_of_IT
presence_94_2024_pseudophilotes_df$year <- as.numeric(presence_94_2024_pseudophilotes_df$year)

#Aglais io
#############################
yearly_presence_aglais <-colSums(data_Aglais[,-1])
yearly_presence_aglais_df <- enframe(yearly_presence_aglais, name = "year", value = "count")
presence_94_2024_aglais_df <- yearly_presence_aglais_df[-c(1:3), ]
presence_94_2024_aglais_df$No_of_IT <- yearly_df$count
presence_94_2024_aglais_df$occupancy <- presence_94_2024_aglais_df$count/presence_94_2024_aglais_df$No_of_IT
presence_94_2024_aglais_df$year <- as.numeric(presence_94_2024_aglais_df$year)

#Melanargia occitanica
#############################
yearly_presence_melanargia <-colSums(data_Melanargia[,-1])
yearly_presence_melanargia_df <- enframe(yearly_presence_melanargia, name = "year", value = "count")
presence_94_2024_melanargia_df <- yearly_presence_melanargia_df[-1, ]
presence_94_2024_melanargia_df$No_of_IT <- yearly_df$count
presence_94_2024_melanargia_df$occupancy <- presence_94_2024_melanargia_df$count/presence_94_2024_melanargia_df$No_of_IT
presence_94_2024_melanargia_df$year <- as.numeric(presence_94_2024_melanargia_df$year)

#Pararge aegeria
############################
yearly_presence_pararge <-colSums(data_Pararge[,-1])
yearly_presence_pararge_df <- enframe(yearly_presence_pararge, name = "year", value = "count")
presence_94_2024_pararge_df <- yearly_presence_pararge_df[-c(1:3), ]
presence_94_2024_pararge_df$No_of_IT <- yearly_df$count
presence_94_2024_pararge_df$occupancy <- presence_94_2024_pararge_df$count/presence_94_2024_pararge_df$No_of_IT
presence_94_2024_pararge_df$year <- as.numeric(presence_94_2024_pararge_df$year)

#Pyronia cecilia
###########################
yearly_presence_pyroniaceci <-colSums(data_PyroniaCeci[,-1])
yearly_presence_pyroniaceci_df <- enframe(yearly_presence_pyroniaceci, name = "year", value = "count")
presence_94_2024_pyroniaceci_df <- yearly_presence_pyroniaceci_df[-c(1:3), ]
presence_94_2024_pyroniaceci_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniaceci_df$occupancy <- presence_94_2024_pyroniaceci_df$count/presence_94_2024_pyroniaceci_df$No_of_IT
#Convertimos 'year' en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniaceci_df$year <- as.numeric(presence_94_2024_pyroniaceci_df$year)

#Pyronia bathseba
###########################
yearly_presence_pyroniabath <-colSums(data_PyroniaBath[,-1])
yearly_presence_pyroniabath_df <- enframe(yearly_presence_pyroniabath, name = "year", value = "count")
presence_94_2024_pyroniabath_df <- yearly_presence_pyroniabath_df
presence_94_2024_pyroniabath_df$No_of_IT <- yearly_df$count
presence_94_2024_pyroniabath_df$occupancy <- presence_94_2024_pyroniabath_df$count/presence_94_2024_pyroniabath_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_pyroniabath_df$year <- as.numeric(presence_94_2024_pyroniabath_df$year)

#Anthocharis euphenoides
##########################
yearly_presence_anthocharis <-colSums(data_Anthocharis[,-1])
yearly_presence_anthocharis_df <- enframe(yearly_presence_anthocharis, name = "year", value = "count")
presence_94_2024_anthocharis_df <- yearly_presence_anthocharis_df
presence_94_2024_anthocharis_df$No_of_IT <- yearly_df$count
presence_94_2024_anthocharis_df$occupancy <- presence_94_2024_anthocharis_df$count/presence_94_2024_anthocharis_df$No_of_IT
#Convertimos year en numerico para despues poder hacer bien el grafico#
presence_94_2024_anthocharis_df$year <- as.numeric(presence_94_2024_anthocharis_df$year)

#END CALCUL OCUPANCIES PER ESPECIES##################################
#####################
#
#
#
#Comenc,en les comandes dels 12 grafics de les ocupancies per especie pel total dels itineraris:#
library(ggplot2)
breaks_ocupancia <- sort(unique(c(seq(from = start_year, to = end_year, by = 4), start_year, end_year)))
breaks_ocupancia_general <- c(start_year, end_year)
##
gg_occupancy_cela <-
  ggplot(data = presence_94_2024_celastrina_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Celastrina argiolus  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_cyani <-
  ggplot(data = presence_94_2024_cyaniris_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Cyaniris semiargus ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_lyca <-
  ggplot(data = presence_94_2024_lycaena_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Lycaena virgaureae  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_plebe <-
  ggplot(data = presence_94_2024_plebejus_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Plebejus argus  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_vane <-
  ggplot(data = presence_94_2024_vanessa_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Vanessa cardui  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_pseudo <-
  ggplot(data = presence_94_2024_pseudophilotes_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pseudophilotes panoptes  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_aglais <-
  ggplot(data = presence_94_2024_aglais_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Aglais io  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_mela <- 
  ggplot(data = presence_94_2024_melanargia_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Melanargia occitanica ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_pararge <-
  ggplot(data = presence_94_2024_pararge_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pararge aegeria  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_pyroceci <-
  ggplot(data = presence_94_2024_pyroniaceci_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pyronia cecilia  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(), 
                          axis.title.y = element_blank())
##

##
gg_occupancy_Pyrobath <-
  ggplot(data = presence_94_2024_pyroniabath_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Pyronia bathseba  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())
##

##
gg_occupancy_Antho <-
  ggplot(data = presence_94_2024_anthocharis_df, aes(x = year, y = occupancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Línea de tendencia lineal sin error estándar
  labs(title = "Anthocharis euphenoides  ") +
  scale_x_continuous(breaks = breaks_ocupancia_general) +
  theme_minimal() + theme(plot.title = element_text(face = "italic", hjust = 0.5, size = 10),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank())

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
ggsave("ocupancies_totals.png", plot = gg_occupancies_total, path = "graphics", width = 15, height = 10, units = "in", dpi = 300)

########################
#Per afegir una columna amb de la ucupancia teorica
colext_Results_df$Occu_teorica <- colext_Results_df$C / (colext_Results_df$C+colext_Results_df$E)

#

