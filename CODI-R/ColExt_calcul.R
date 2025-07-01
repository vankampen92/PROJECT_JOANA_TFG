# Obtenemos los esquemas de muestreo
# (just to check whether this change is visible when Joana does git pull!!!)
library(tidyverse)
library(island)
library(data.table)
install.packages("stargazer")
library(stargazer)

# Cargamos los datos
# BEGIN: Exemples inicials... 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

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
  
  colext_Sp_Results[[i]] <- irregular_multiple_datasets(list_multiple[[i]], list_vectors, 
                                                        0.0001, 0.0001, CI = TRUE)
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
colext_Results_df$Ratio <- NLL/No_of_TRANSITIONS
colext_Results_df$Temps_Ca <- 1/(C + E)

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

my_colors <- c( "Pseudophilotes panoptes" = "darkblue",
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
save(colext_Results_df_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_ordenado.RData")
# Export LaTeX table
stargazer(colext_Results_df_ordenado, summary = FALSE, out = "~/PROJECT_JOANA_TFG/DOCS-LATEX/colex_Results_df_ordenado.tex")
###############################################################
###############################################################

#calcul de col ext per bioregio
#######################################
#No olvidemos cargar los paquetes y los datos necesarios:
itin_CBMS_RegClim <- read.csv(file = "~/PROJECT_JOANA_TFG/DADES/itin_CBMS_regionsclimatiques.csv", 
                              sep = '\t')
library(ggplot2)
library(tidyverse)
library(island)
library(data.table)
library(dplyr)
# Selecting the intenaris per bioclimatic region:
itin_CBMS_RegClim_1 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 1, ]
itin_CBMS_RegClim_2 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 2, ]
itin_CBMS_RegClim_3 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 3, ]

itin_ID_1 <- itin_CBMS_RegClim_1$CODI
itin_ID_2 <- itin_CBMS_RegClim_2$CODI
itin_ID_3 <- itin_CBMS_RegClim_3$CODI

# Regio Alpine
filtered_list_BR1 <- list()
# Med. Humida
filtered_list_BR2 <- list()
# Med. Arida
filtered_list_BR3 <- list()

# Crear les tres llistes segons regions 
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
  
  filtered_list_BR1[[i]] <- Filter(function(x) x$IDitin %in% itin_ID_1, list_multiple[[i]])
  filtered_list_BR2[[i]] <- Filter(function(x) x$IDitin %in% itin_ID_2, list_multiple[[i]])
  filtered_list_BR3[[i]] <- Filter(function(x) x$IDitin %in% itin_ID_3, list_multiple[[i]])
}


# Crear una llista amb tots els resultats (de la Regio de Muntanya Alpina: colext_Sp_Res_BR1[[]] 
# utilitzant la llista d'itineraris 
colext_Sp_Res_BR1 <- list()
for( i in 1:length(Species_Latin_Names) ) {
  
  ncols <- sapply(filtered_list_BR1[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length( filtered_list_BR1[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  # Aixo es el numero de transiction totals de les especie i en tots els itineraris. 
  # (numero de factors de la funcio de versemblanc,a multiplicativa)
  No_of_TRANSITIONS[i] = sum(sapply(list_vectors, length) - 1)
  
  colext_Sp_Res_BR1[[i]] <- irregular_multiple_datasets(filtered_list_BR1[[i]], list_vectors, 0.0001, 0.0001, CI = TRUE)
}

C_BR1      <-vector()
C_low_BR1  <-vector()
C_up_BR1  <-vector()
E_BR1      <-vector()  
E_low_BR1  <-vector()
E_up_BR1   <-vector()
N_BR1      <-vector()
NLL_BR1    <-vector()
for(i in 1: length(Species_Latin_Names)) {
  C_BR1[i] <- colext_Sp_Res_BR1[[i]]$c
  C_low_BR1[i] <- colext_Sp_Res_BR1[[i]]$c_low
  C_up_BR1[i] <- colext_Sp_Res_BR1[[i]]$c_up
  E_BR1[i] <- colext_Sp_Res_BR1[[i]]$e
  E_low_BR1[i] <- colext_Sp_Res_BR1[[i]]$e_low
  E_up_BR1[i] <- colext_Sp_Res_BR1[[i]]$e_up
  N_BR1[i] <- colext_Sp_Res_BR1[[i]]$N
  NLL_BR1[i] <- colext_Sp_Res_BR1[[i]]$NLL
}

# Create the data frame
colext_Results_df_BR1<- data.frame(species = Species_Latin_Names)

# Afegir els vectors creats anteriorment (C, C_low, ...) al data frame:
colext_Results_df_BR1$C_BR1     <- C_BR1
colext_Results_df_BR1$C_low_BR1 <- C_low_BR1
colext_Results_df_BR1$C_up_BR1  <- C_up_BR1
colext_Results_df_BR1$E_BR1     <- E_BR1
colext_Results_df_BR1$E_low_BR1 <- E_low_BR1
colext_Results_df_BR1$E_up_BR1  <- E_up_BR1
colext_Results_df_BR1$N_BR1     <- N_BR1
colext_Results_df_BR1$NLL_BR1   <- NLL_BR1
colext_Results_df_BR1$Ratio     <- NLL_BR1/No_of_TRANSITIONS
colext_Results_df_BR1$Temps_Ca  <- 1/(C_BR1 + E_BR1)

save(colext_Results_df_BR1, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1.RData")

# Tu vector de colores personalizado
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
  "Pyronia cecilia" = "palevioletred1"
)

# Definición del orden personalizado de las especies
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
  "Pyronia cecilia"
)

# Paso 1: Crear o modificar la columna 'species' en el dataframe original (o una copia)
# para que sea un factor con tu orden personalizado.
# Es mejor trabajar con el dataframe que luego vas a ordenar.
colext_Results_df_BR1_temp <- colext_Results_df_BR1 # Hacemos una copia temporal si no queremos modificar el original

colext_Results_df_BR1_temp$species <- factor(
  colext_Results_df_BR1_temp$species,
  levels = orden_personalizado_spp_colex
)

# Paso 2: Ahora que 'species' es un factor con el orden correcto,
# usa arrange() para ordenar las filas del dataframe.
colext_Results_df_BR1_ordenado <- colext_Results_df_BR1_temp %>%
  arrange(species)

# Si quieres hacerlo en un solo paso con el pipe, puedes encadenar las operaciones:
colext_Results_df_BR1_ordenado <- colext_Results_df_BR1 %>%
  dplyr::mutate(species = factor(species, levels = orden_personalizado_spp_colex)) %>%
  dplyr::arrange(species)


gg_colext_BR1 <-
  ggplot(filter(colext_Results_df_BR1_ordenado, species != "Melanargia occitanica"), aes(x = C_BR1, y = E_BR1,  color = species)) +
  geom_point(size = 2) + 
  geom_errorbarh(data = filter(colext_Results_df_BR1_ordenado, species != "Melanargia occitanica"),
                  aes(xmin = C_low_BR1, xmax = C_up_BR1), height = 0.05, size = 0.8) +
  geom_errorbar(data = filter(colext_Results_df_BR1_ordenado, species != "Melanargia occitanica"),
  aes(ymin = E_low_BR1, ymax = E_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(
    title = "Regió Alpina i Subaplina",
    x = "Colonització",
    y = "Extinció",
    color = "Espècie" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = orden_personalizado_spp_colex) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )

print(gg_colext_BR1)
save(colext_Results_df_BR1_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1_ordenado.RData")
# Export LaTeX table
stargazer(colext_Results_df_BR1_ordenado, summary = FALSE, out = "~/PROJECT_JOANA_TFG/DOCS-LATEX/colex_Results_df_BR1_ordenado.tex")

#Els valors de melanargia son molt alts per afegirlos a la grafica.
#Els valors son els seguents: C <- 3.7958428
#                             C_low <- 0.1862706
#                             C _up <-26.1190076
#                             E <- 15.17992361
#                             E_low <- 2.19767937
#                             E_up <- 309.30890507
#COMENTARI: Valors out-layer:
# Melanargia (Al cbms) s'observa en regressio forta general en el nombre d'abundancia 
#Hi ha tan sols 1 itinerari = id itin =167 Toirigo, en la que es va observar tan sol un any el 2023
#la descripcio de la spp diu que nomes es troba de 0 - 1000m i aquest itinerari
#te una alc'ada mitjana de 1500 m. 
#Vanessa te un valor de col=2,7 (tendencia pob: regressio moderada general, estable alpina)
#Lycaena te un valor de col=1,3 (tendencia pob: general estable, i no avaluada)
#Pyronia cecilia te una barra d'error de E_up = 1.9
##########################################
##########################################

# --- Cálculo de col/ext para la Región Mediterrania Humida (BR2) ---
colext_Sp_Res_BR2 <- list()
# Asegúrate de inicializar No_of_TRANSITIONS si no se hizo globalmente o si se necesita re-inicializar
# No_of_TRANSITIONS <- vector("numeric", length(Species_Latin_Names)) # Descomentar si es necesario

for( i in 1:length(Species_Latin_Names) ) {
  # Solo procesar si hay itinerarios filtrados para esta especie en BR2
  if (length(filtered_list_BR2[[i]]) > 0) {
    # Calcular ncols de forma robusta
    temp_ncols <- lapply(filtered_list_BR2[[i]], function(df_item) {
      if (is.data.frame(df_item)) {
        return(ncol(df_item))
      } else {
        warning(paste0("Elemento en filtered_list_BR2[[", i, "]] no es un dataframe. Es un ", class(df_item), "."))
        return(NA_integer_)
      }
    })
    ncols <- unlist(temp_ncols)
    ncols <- ncols[!is.na(ncols)] # Eliminar NAs si los hay
    
    if (length(ncols) > 0) { # Si hay conteos de columnas válidos
      list_vectors <- list()
      for(j in 1:length(filtered_list_BR2[[i]])) { # Iterar sobre los dataframes válidos
        if (j <= length(ncols) && is.numeric(ncols[j]) && ncols[j] > 1) {
          list_vectors[[j]] <- 2:(ncols[j] - 1)
        } else {
          list_vectors[[j]] <- integer(0) # Asignar vector entero vacío si hay problemas
        }
      }
      # Cálculo robusto para No_of_TRANSITIONS (asumo que se maneja un único vector No_of_TRANSITIONS global)
      # Si No_of_TRANSITIONS debe ser específico para BR2, se necesitaría un nuevo vector (ej., No_of_TRANSITIONS_BR2)
      # Por ahora, se sigue usando el global, lo que podría no ser lo ideal si los cálculos son distintos por BR.
      No_of_TRANSITIONS[i] = sum(sapply(list_vectors, function(vec) max(0, length(vec) - 1)))
      
      colext_Sp_Res_BR2[[i]] <- island::irregular_multiple_datasets(filtered_list_BR2[[i]], list_vectors, 0.0001, 0.0001, CI = TRUE)
    } else {
      # No hay dataframes válidos para procesar para esta especie en esta región
      colext_Sp_Res_BR2[[i]] <- list(c = NA, c_low = NA, c_up = NA, e = NA, e_low = NA, e_up = NA, N = NA, NLL = NA)
      No_of_TRANSITIONS[i] = NA
    }
  } else {
    # filtered_list_BR2[[i]] es una lista vacía para esta especie
    colext_Sp_Res_BR2[[i]] <- list(c = NA, c_low = NA, c_up = NA, e = NA, e_low = NA, e_up = NA, N = NA, NLL = NA)
    No_of_TRANSITIONS[i] = NA
  }
}

# Inicializar vectores para extraer resultados de BR2
C_BR2 <- vector()
C_low_BR2 <- vector()
C_up_BR2 <- vector()
E_BR2 <- vector()
E_low_BR2 <- vector()
E_up_BR2 <- vector()
N_BR2 <- vector()
NLL_BR2 <- vector()

for(i in 1:length(Species_Latin_Names)) {
  # Extraer resultados, manejando casos donde colext_Sp_Res_BR2[[i]] podría ser NA o vacío
  if (!is.null(colext_Sp_Res_BR2[[i]]) && !is.na(colext_Sp_Res_BR2[[i]][[1]])) {
    C_BR2[i] <- colext_Sp_Res_BR2[[i]]$c
    C_low_BR2[i] <- colext_Sp_Res_BR2[[i]]$c_low
    C_up_BR2[i] <- colext_Sp_Res_BR2[[i]]$c_up
    E_BR2[i] <- colext_Sp_Res_BR2[[i]]$e
    E_low_BR2[i] <- colext_Sp_Res_BR2[[i]]$e_low
    E_up_BR2[i] <- colext_Sp_Res_BR2[[i]]$e_up
    N_BR2[i] <- colext_Sp_Res_BR2[[i]]$N
    NLL_BR2[i] <- colext_Sp_Res_BR2[[i]]$NLL
  } else {
    # Asignar NA si no hay resultados válidos para la especie
    C_BR2[i] <- NA
    C_low_BR2[i] <- NA
    C_up_BR2[i] <- NA
    E_BR2[i] <- NA
    E_low_BR2[i] <- NA
    E_up_BR2[i] <- NA
    N_BR2[i] <- NA
    NLL_BR2[i] <- NA
  }
}

# --- CREAR Y RELLENAR EL DATAFRAME colext_Results_df_BR2 ---
# ¡IMPORTANTE: Crear el dataframe ANTES de añadir las columnas!
colext_Results_df_BR2 <- data.frame(species = Species_Latin_Names)

# Añadir los vectores creados anteriormente (C, C_low, ...) al data frame:
colext_Results_df_BR2$C_BR2 <- C_BR2
colext_Results_df_BR2$C_low_BR2 <- C_low_BR2
colext_Results_df_BR2$C_up_BR2 <- C_up_BR2
colext_Results_df_BR2$E_BR2 <- E_BR2
colext_Results_df_BR2$E_low_BR2 <- E_low_BR2
colext_Results_df_BR2$E_up_BR2 <- E_up_BR2
colext_Results_df_BR2$N_BR2 <- N_BR2
colext_Results_df_BR2$NLL_BR2 <- NLL_BR2
colext_Results_df_BR2$Ratio     <- NLL_BR2/No_of_TRANSITIONS
colext_Results_df_BR2$Temps_Ca  <- 1/(C_BR2 + E_BR2)

save(colext_Results_df_BR2, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2.RData")

colext_Results_df_BR2_temp <- colext_Results_df_BR2 # Hacemos una copia temporal si no queremos modificar el original

colext_Results_df_BR2_temp$species <- factor(
  colext_Results_df_BR2_temp$species,
  levels = orden_personalizado_spp_colex
)

# Paso 2: Ahora que 'species' es un factor con el orden correcto,
# usa arrange() para ordenar las filas del dataframe.
colext_Results_df_BR2_ordenado <- colext_Results_df_BR2_temp %>%
  arrange(species)

# Si quieres hacerlo en un solo paso con el pipe, puedes encadenar las operaciones:
colext_Results_df_BR2_ordenado <- colext_Results_df_BR2 %>%
  dplyr::mutate(species = factor(species, levels = orden_personalizado_spp_colex)) %>%
  dplyr::arrange(species)


# --- Generación del gráfico ggplot2 para la Región Mediterrania Humida (BR2) ---
gg_colext_BR2 <-
  ggplot(colext_Results_df_BR2_ordenado, aes(x = C_BR2, y = E_BR2, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbarh(aes(xmin = C_low_BR2, xmax = C_up_BR2), height = 0.05, size = 0.8) +
  # Añadir barras de error verticales para los intervalos de confianza de 'E'
  geom_errorbar(aes(ymin = E_low_BR2, ymax = E_up_BR2), width = 0.005, size = 0.8) +  # Puntos dispersos. El color se define por 'species' en aes()
  theme_minimal() +
  labs(
    title = "Regió Mediterrània Humida",
    x = "Colonització",
    y = "Extinció",
    color = "Espècie" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = orden_personalizado_spp_colex) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )

print(gg_colext_BR2)
#lycaena no esta representada al grafic perque no ha pogut calcular c i e

save(colext_Results_df_BR2_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2_ordenado.RData")
# Export LaTeX table
stargazer(colext_Results_df_BR2_ordenado, summary = FALSE, out = "~/PROJECT_JOANA_TFG/DOCS-LATEX/colex_Results_df_BR2_ordenado.tex")

#######################################
#######################################
# --- Cálculo de col/ext para la Región Mediterrania Arida (BR3) ---
colext_Sp_Res_BR3 <- list()
# Asegúrate de inicializar No_of_TRANSITIONS si no se hizo globalmente o si se necesita re-inicializar
# No_of_TRANSITIONS <- vector("numeric", length(Species_Latin_Names)) # Descomentar si es necesario

for( i in 1:length(Species_Latin_Names) ) {
  # Solo procesar si hay itinerarios filtrados para esta especie en BR2
  if (length(filtered_list_BR3[[i]]) > 0) {
    # Calcular ncols de forma robusta
    temp_ncols <- lapply(filtered_list_BR3[[i]], function(df_item) {
      if (is.data.frame(df_item)) {
        return(ncol(df_item))
      } else {
        warning(paste0("Elemento en filtered_list_BR3[[", i, "]] no es un dataframe. Es un ", class(df_item), "."))
        return(NA_integer_)
      }
    })
    ncols <- unlist(temp_ncols)
    ncols <- ncols[!is.na(ncols)] # Eliminar NAs si los hay
    
    if (length(ncols) > 0) { # Si hay conteos de columnas válidos
      list_vectors <- list()
      for(j in 1:length(filtered_list_BR3[[i]])) { # Iterar sobre los dataframes válidos
        if (j <= length(ncols) && is.numeric(ncols[j]) && ncols[j] > 1) {
          list_vectors[[j]] <- 2:(ncols[j] - 1)
        } else {
          list_vectors[[j]] <- integer(0) # Asignar vector entero vacío si hay problemas
        }
      }
      # Cálculo robusto para No_of_TRANSITIONS (asumo que se maneja un único vector No_of_TRANSITIONS global)
      # Si No_of_TRANSITIONS debe ser específico para BR2, se necesitaría un nuevo vector (ej., No_of_TRANSITIONS_BR3)
      # Por ahora, se sigue usando el global, lo que podría no ser lo ideal si los cálculos son distintos por BR.
      No_of_TRANSITIONS[i] = sum(sapply(list_vectors, function(vec) max(0, length(vec) - 1)))
      
      colext_Sp_Res_BR3[[i]] <- island::irregular_multiple_datasets(filtered_list_BR3[[i]], list_vectors, 0.0001, 0.0001, CI = TRUE)
    } else {
      # No hay dataframes válidos para procesar para esta especie en esta región
      colext_Sp_Res_BR3[[i]] <- list(c = NA, c_low = NA, c_up = NA, e = NA, e_low = NA, e_up = NA, N = NA, NLL = NA)
      No_of_TRANSITIONS[i] = NA
    }
  } else {
    # filtered_list_BR3[[i]] es una lista vacía para esta especie
    colext_Sp_Res_BR3[[i]] <- list(c = NA, c_low = NA, c_up = NA, e = NA, e_low = NA, e_up = NA, N = NA, NLL = NA)
    No_of_TRANSITIONS[i] = NA
  }
}

# Inicializar vectores para extraer resultados de BR3
C_BR3 <- vector()
C_low_BR3 <- vector()
C_up_BR3 <- vector()
E_BR3 <- vector()
E_low_BR3 <- vector()
E_up_BR3 <- vector()
N_BR3 <- vector()
NLL_BR3 <- vector()

for(i in 1:length(Species_Latin_Names)) {
  # Extraer resultados, manejando casos donde colext_Sp_Res_BR3[[i]] podría ser NA o vacío
  if (!is.null(colext_Sp_Res_BR3[[i]]) && !is.na(colext_Sp_Res_BR3[[i]][[1]])) {
    C_BR3[i] <- colext_Sp_Res_BR3[[i]]$c
    C_low_BR3[i] <- colext_Sp_Res_BR3[[i]]$c_low
    C_up_BR3[i] <- colext_Sp_Res_BR3[[i]]$c_up
    E_BR3[i] <- colext_Sp_Res_BR3[[i]]$e
    E_low_BR3[i] <- colext_Sp_Res_BR3[[i]]$e_low
    E_up_BR3[i] <- colext_Sp_Res_BR3[[i]]$e_up
    N_BR3[i] <- colext_Sp_Res_BR3[[i]]$N
    NLL_BR3[i] <- colext_Sp_Res_BR3[[i]]$NLL
  } else {
    # Asignar NA si no hay resultados válidos para la especie
    C_BR3[i] <- NA
    C_low_BR3[i] <- NA
    C_up_BR3[i] <- NA
    E_BR3[i] <- NA
    E_low_BR3[i] <- NA
    E_up_BR3[i] <- NA
    N_BR3[i] <- NA
    NLL_BR3[i] <- NA
  }
}

# --- CREAR Y RELLENAR EL DATAFRAME colext_Results_df_BR3 ---
# ¡IMPORTANTE: Crear el dataframe ANTES de añadir las columnas!
colext_Results_df_BR3 <- data.frame(species = Species_Latin_Names)

# Añadir los vectores creados anteriormente (C, C_low, ...) al data frame:
colext_Results_df_BR3$C_BR3 <- C_BR3
colext_Results_df_BR3$C_low_BR3 <- C_low_BR3
colext_Results_df_BR3$C_up_BR3 <- C_up_BR3
colext_Results_df_BR3$E_BR3 <- E_BR3
colext_Results_df_BR3$E_low_BR3 <- E_low_BR3
colext_Results_df_BR3$E_up_BR3 <- E_up_BR3
colext_Results_df_BR3$N_BR3 <- N_BR3
colext_Results_df_BR3$NLL_BR3 <- NLL_BR3
colext_Results_df_BR3$Ratio     <- NLL_BR2/No_of_TRANSITIONS
colext_Results_df_BR3$Temps_Ca  <- 1/(C_BR3 + E_BR3)

save(colext_Results_df_BR3, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3.RData")

colext_Results_df_BR3_temp <- colext_Results_df_BR3 # Hacemos una copia temporal si no queremos modificar el original



colext_Results_df_BR3_temp$species <- factor(
  colext_Results_df_BR3_temp$species,
  levels = orden_personalizado_spp_colex
)

# Paso 2: Ahora que 'species' es un factor con el orden correcto,
# usa arrange() para ordenar las filas del dataframe.
colext_Results_df_BR3_ordenado <- colext_Results_df_BR3_temp %>%
  arrange(species)

# Si quieres hacerlo en un solo paso con el pipe, puedes encadenar las operaciones:
colext_Results_df_BR3_ordenado <- colext_Results_df_BR3 %>%
  dplyr::mutate(species = factor(species, levels = orden_personalizado_spp_colex)) %>%
  dplyr::arrange(species)

# --- Generación del gráfico ggplot2 para la Región Mediterrania Humida (BR2) ---
gg_colext_BR3 <-
  ggplot(colext_Results_df_BR3_ordenado, aes(x = C_BR3, y = E_BR3, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbarh(aes(xmin = C_low_BR3, xmax = C_up_BR3), height = 0.05, size = 0.8) +
  # Añadir barras de error verticales para los intervalos de confianza de 'E'
  geom_errorbar(aes(ymin = E_low_BR3, ymax = E_up_BR3), width = 0.005, size = 0.8) +  # Puntos dispersos. El color se define por 'species' en aes()
  theme_minimal() +
  labs(
    title = "Regió Mediterrània Àrida",
    x = "Colonització",
    y = "Extinció",
    color = "Espècie" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = orden_personalizado_spp_colex) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )

print(gg_colext_BR3)
save(colext_Results_df_BR3_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3_ordenado.RData")
# Export LaTeX table
stargazer(colext_Results_df_BR3_ordenado, summary = FALSE, out = "~/PROJECT_JOANA_TFG/DOCS-LATEX/colex_Results_df_BR3_ordenado.tex")

###############################################
###############################################
#COMENTARIS: 
#cyaniris semiargus i la lycaena virgaureae han estat eliminades del grafic
#perque no esta present en cap itinerari de clima mediterrani arid
#llavors es normal que no les trobem


