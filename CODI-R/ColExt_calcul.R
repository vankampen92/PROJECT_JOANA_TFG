
# Email Vicente:  Construccio de la llista de una especies (p.e. Pararge aegeria) que s'utilitzara com a 
# input de irregular multiple 
# Loop to calculate the (c, e) pair for each species across itineraris at once
list_multiple <- list()

colext_Sp_Results <- list()
No_of_TRANSITIONS <- vector()

# Crear Specie_Latin_Names amb el nom de cada especie
Species_Latin_Names <- unique(data$sp_latin)

# Crear una llista amb tots els resultats: colext_Sp_Results[[]]
for( i in 1:length(Species_Latin_Names) ) {
  
  list_multiple[[i]] <- data %>%
    group_by(IDitin) %>%
    group_split() %>%
    lapply(function(df) {
      id <- unique(df$IDitin)
      df_out <- df %>%
        group_by(Any, sp_latin) %>%
        count() %>%
        pivot_wider(names_from = Any, values_from = n) %>%
        ungroup() %>%
        mutate(across(-sp_latin, negate(is.na))) %>%
        mutate(across(-sp_latin, as.numeric)) %>%
        filter(sp_latin == Species_Latin_Names[i]) %>%
        as.data.frame()
      if (nrow(df_out) > 0 && ncol(df_out) > 0) {
        df_out$IDitin <- id  # Retain IDitin
      }
      
      df_out
    }) %>%
    Filter(function(df) nrow(df) > 0 && ncol(df) > 5, .)
  
  ncols <- sapply(list_multiple[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length( list_multiple[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  # Aixo es el numero de transiction totals de les especie i en tots els itineraris. 
  # (numero de factors de la funcio de versemblanc,a multiplicativa)
  No_of_TRANSITIONS[i] = sum(sapply(list_vectors, length) - 1)
  
  colext_Sp_Results[[i]] <- irregular_multiple_datasets(list_multiple[[i]], list_vectors, 0.0001, 0.0001, CI = TRUE)
}

C      <-vector()
C_low  <-vector()
C_up   <-vector()
E      <-vector()  
E_low  <-vector()
E_up   <-vector()
N      <-vector()
NLL    <-vector()
for(i in 1: length(Species_Latin_Names)) {
  C[i] <- colext_Sp_Results[[i]]$c
  C_low[i] <- colext_Sp_Results[[i]]$c_low
  C_up[i] <- colext_Sp_Results[[i]]$c_up
  E[i] <- colext_Sp_Results[[i]]$e
  E_low[i] <- colext_Sp_Results[[i]]$e_low
  E_up[i] <- colext_Sp_Results[[i]]$e_up
  N[i] <- colext_Sp_Results[[i]]$N
  NLL[i] <- colext_Sp_Results[[i]]$NLL
}

# Create the data frame
colext_Results_df <- data.frame(species = Species_Latin_Names)

# Afegir els vectors creats anteriorment (C, C_low, ...) al data frame:
colext_Results_df$C <- C
colext_Results_df$C_low <- C_low
colext_Results_df$C_up <- C_up
colext_Results_df$E <- E
colext_Results_df$E_low <- E_low
colext_Results_df$E_up <- E_up
colext_Results_df$N <- N
colext_Results_df$NLL <- NLL

#creacio del grafic general de col ext per tots els itineraris

# Load the ggplot2 package
library(ggplot2)
#preparar datos para el grafico#
orden_personalizado_spp_colex <- c(
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
  "Pyronia cecilia") 
colext_Results_df$species <- factor(colext_Results_df$species, levels = orden_personalizado_spp_colex)

# install.packages("dplyr") # Si no lo tienes
library(dplyr)

colext_Results_df_ordenado <- colext_Results_df %>%
  arrange(species)

my_colors <- c(
  
  "Pseudophilotes panoptes" = "darkblue",
  "Cyaniris semiargus" = "mediumblue",
  "Plebejus argus" = "steelblue4",
  "Aglais io" = "blueviolet",
  "Melanargia occitanica" = "deepskyblue1",
  "Anthocharis euphenoides" = "cadetblue2",
  "Vanessa cardui" = "gold2",
  "Lycaena virgaureae" = "yellow",
  "Pararge aegeria" = "red4",
  "Celastrina argiolus" = "orangered",
  "Pyronia bathseba" = "violetred",
  "Pyronia cecilia" = "palevioletred1")



gg_colext_total <- ggplot(colext_Results_df, aes(x = C, y = E, color = species)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.05) +
  geom_errorbarh(aes(xmin = C_low, xmax = C_up), height = 0.05) +
  theme_minimal() +
  labs(
    x = "Colonització",
    y = "Extinció",
    color = "Espècie"
  ) +
  scale_color_manual(values = my_colors)  +
  theme(legend.text = element_text(face = "italic"))


# Para visualizarlo:
print(gg_colext_total)
###############################################################
###############################################################


#calcul de col ex per bioregio
#######################################
#Scatter plot regio bioclimatica 1 amb barres d'error (Chatgpt example)
# Load the ggplot2 package
library(ggplot2)
# Create the plot
ggplot(colext_Results_df_1, aes(x = C, y = E)) +
  geom_point(size = 2, color = "green") +  # Scatter points
  geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.05, color = "green") +  # Vertical error bars for E
  geom_errorbarh(aes(xmin = C_low, xmax = C_up), height = 0.05, color = "green") +  # Horizontal error bars for C
  theme_minimal() +
  labs(
    title = "Scatter plot of C vs E with Confidence Intervals",
    x = "Colonitzacio",
    y = "Extincio"
  )

ggplot(colext_Results_df_1_sp11, aes(x = C, y = E)) +
  geom_point(size = 2, color = "green") +  # Scatter points
  geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.05, color = "green") +  # Vertical error bars for E
  geom_errorbarh(aes(xmin = C_low, xmax = C_up), height = 0.05, color = "green") +  # Horizontal error bars for C
  theme_minimal() +
  labs(
    title = "Scatter plot of C vs E with Confidence Intervals",
    x = "Colonitzacio",
    y = "Extincio"
  )

# Sense barres d'error: 
ggplot(colext_Results_df_1, aes(x = C, y = E)) +
  geom_point(size = 2, color = "green") +  # Scatter points
  theme_minimal() +
  labs(
    title = "Scatter plot of C vs E with Confidence Intervals",
    x = "Colonitzacio",
    y = "Extincio"
  )

