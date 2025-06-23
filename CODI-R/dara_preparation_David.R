# Cargamos los datos
# Obtenemos los esquemas de muestreo
library(tidyverse)
library(island)
library(data.table)

# BEGIN: Exemples inicials... 
ColExtDades <- read.csv(file="/home/dalonso/My_Documents/Work/My_Supervised_Students/Undergrads/PRACTIQUES-UdG/JOANA_DEKKER/CBMS_colext.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/My_Documents/Work/My_Supervised_Students/Undergrads/PRACTIQUES-UdG/JOANA_DEKKER/CBMS_colext_2024.csv")

data <- ColExtDades
data %>% filter(sp_latin == "Celastrina argiolus", IDitin == 1, IDsecc == 1)

# Construir una matriz donde las filas sean las especies y la columnas los anyos 
# para cada itinerario. Y Despues hacemos una lista que sea el conjunto de todos 
# las matrices, con todos los itinerarios.  

# Nos quedamos con los transectos muestreados durante más de un año
more1year <- scheme %>% group_by(IDitin, Any) %>% count() %>% group_by(IDitin) %>% 
  count() %>% mutate(more = n > 1) %>% filter(more)

data2 <- data %>% filter(IDitin %in% more1year$IDitin)

# Obtenemos (casi) la matriz de presencias ausencias para un itinerario concreto
n %>% View()

data4 <- data2 %>% filter(IDitin == 1) %>% group_by(Any, sp_latin) %>% count() %>% 
  pivot_wider(names_from = Any, values_from = n) %>% #ungroup() %>% 
  # select(-1) %>% 
  is.na() 

# %>% View()

data5 <- data2 %>% filter(IDitin == 1) %>% group_by(Any, sp_latin) %>% count() %>% 
  pivot_wider(names_from = Any, values_from = n) %>% is.na() 

# %>% View()
# %>% rowSums()
# END: Exemples inicials. 
 
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

#Scatter plot general amb barres d'error (Chatgpt example)
# Load the ggplot2 package
library(ggplot2)
# Create the plot
ggplot(colext_Results_df, aes(x = C, y = E)) +
  geom_point(size = 2, color = "blue") +  # Scatter points
  geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.05, color = "blue") +  # Vertical error bars for E
  geom_errorbarh(aes(xmin = C_low, xmax = C_up), height = 0.05, color = "blue") +  # Horizontal error bars for C
  theme_minimal() +
  labs(
    title = "Scatter plot of C vs E with Confidence Intervals",
    x = "Colonitzacio",
    y = "Extincio"
  )


# Selecting the intenaris per bioclimatic region:
itin_CBMS_RegClim_1 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 1, ]
itin_CBMS_RegClim_2 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 2, ]
itin_CBMS_RegClim_3 <- itin_CBMS_RegClim[itin_CBMS_RegClim[[ncol(itin_CBMS_RegClim)]] == 3, ]
itin_ID_1 <- itin_CBMS_RegClim_1$CODI
itin_ID_2 <- itin_CBMS_RegClim_2$CODI
itin_ID_3 <- itin_CBMS_RegClim_3$CODI

# Regio Alpine
filtered_list_1 <- list()
# Med. Humida
filtered_list_2 <- list()
# Med. Arida
filtered_list_3 <- list()

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
  
   filtered_list_1[[i]] <- Filter(function(x) x$IDitin %in% itin_ID_1, list_multiple[[i]])
   filtered_list_2[[i]] <- Filter(function(x) x$IDitin %in% itin_ID_2, list_multiple[[i]])
   filtered_list_3[[i]] <- Filter(function(x) x$IDitin %in% itin_ID_3, list_multiple[[i]])
}

# An example from ChatGPT
filtered_list <- Filter(function(x) x$IDitin %in% ids_to_keep, my_list)

# Crear una llista amb tots els resultats (de la Regio de Muntanya Alpina: colext_Sp_Results_1[[]] 
# utilitzant la llista d'itineraris 
colext_Sp_Results_1 <- list()
for( i in 1:length(Species_Latin_Names) ) {
  
  ncols <- sapply(filtered_list_1[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length( filtered_list_1[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  # Aixo es el numero de transiction totals de les especie i en tots els itineraris. 
  # (numero de factors de la funcio de versemblanc,a multiplicativa)
  No_of_TRANSITIONS[i] = sum(sapply(list_vectors, length) - 1)
  
  colext_Sp_Results_1[[i]] <- irregular_multiple_datasets(filtered_list_1[[i]], list_vectors, 0.0001, 0.0001, CI = TRUE)
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
  C[i] <- colext_Sp_Results_1[[i]]$c
  C_low[i] <- colext_Sp_Results_1[[i]]$c_low
  C_up[i] <- colext_Sp_Results_1[[i]]$c_up
  E[i] <- colext_Sp_Results_1[[i]]$e
  E_low[i] <- colext_Sp_Results_1[[i]]$e_low
  E_up[i] <- colext_Sp_Results_1[[i]]$e_up
  N[i] <- colext_Sp_Results_1[[i]]$N
  NLL[i] <- colext_Sp_Results_1[[i]]$NLL
}

# Create the data frame
colext_Results_df_1 <- data.frame(species = Species_Latin_Names)

# Afegir els vectors creats anteriorment (C, C_low, ...) al data frame:
colext_Results_df_1$C <- C
colext_Results_df_1$C_low <- C_low
colext_Results_df_1$C_up <- C_up
colext_Results_df_1$E <- E
colext_Results_df_1$E_low <- E_low
colext_Results_df_1$E_up <- E_up
colext_Results_df_1$N <- N
colext_Results_df_1$NLL <- NLL

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

# 1: Pararge aegeria
input_paraeg_multiple <- data %>%
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
      filter(sp_latin == "Pararge aegeria") %>%
      as.data.frame()
    
    if (nrow(df_out) > 0 && ncol(df_out) > 0) {
      df_out$IDitin <- id  # Retain IDitin
    }
    
    df_out
  }) %>%
  Filter(function(df) nrow(df) > 0 && ncol(df) > 5, .)

# To create a list containing the time sequences 2:32, 2:24, 2:30, etc and locate and use the right time 
# vectors, we need first:
ncols <- sapply(input_paraeg_multiple, ncol)  
list_vectors <- list()
for(i in 1:length(input_paraeg_multiple)){
  list_vectors[[i]] <- 2:(ncols[i] - 1)
}
paraeg_m <- irregular_multiple_datasets(input_paraeg_multiple, list_vectors, 0.0001, 0.0001, CI = TRUE)



#####################################################################################################################
######################### J O A N A   D E K K E R   C O D E  ########################################################
data <- read.csv("~/My_Documents/Work/My_Supervised_Students/Undergrads/PRACTIQUES-UdG/JOANA_DEKKER/CBMS_colext.csv")
# Including 2024 year: 
data <- read.csv(file="/home/dalonso/My_Documents/Work/My_Supervised_Students/Undergrads/PRACTIQUES-UdG/JOANA_DEKKER/CBMS_colext_2024.csv")

library(tidyverse)

data_itin1 <-  
  data %>% filter(IDitin == 1) %>% group_by(Any, sp_latin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!sp_latin, negate(is.na))) %>%
  mutate(across(!sp_latin, as.numeric))

library(island)
irregular_single_dataset(dataframe = data_itin1,
                         vector = 2:34,
                         c = .09,
                         e = .01, 
                         CI = TRUE)

data_celastrina <-
  data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

celarg <-irregular_single_dataset(dataframe = data_celastrina,
                                   vector = 2:35,
                                   c = .09, e = .01, CI = TRUE)

data_Lycaena <-
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

lycvirg <-irregular_single_dataset(dataframe = data_Lycaena,
                                    vector = 2:25,
                                    c = .09, e = .01, CI = TRUE)

data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

plearg <-irregular_single_dataset(dataframe = data_Plebejus, vector = 2:34, c = .09, e = .01)
  
data_Vanessa <-
  data %>% filter(sp_latin == "Vanessa cardui") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

vancar <-irregular_single_dataset(dataframe = data_Vanessa, vector = 2:34, c = .09, e = .01)

data_Pseudophilotes <-
  data %>% filter(sp_latin == "Pseudophilotes panoptes") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

psepan <-irregular_single_dataset(dataframe = data_Pseudophilotes, vector = 2:31, c = .09, e = .01)

data_Aglais <-
  data %>% filter(sp_latin == "Aglais io") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

aglaio <-irregular_single_dataset(dataframe = data_Aglais, vector = 2:34, c = .09, e = .01)

data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

melocc <-irregular_single_dataset(dataframe = data_Melanargia,
                                  vector = 2:32,
                                  c = .09, e = .01)

data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

melocc <-irregular_single_dataset(dataframe = data_Melanargia,
                                  vector = 2:32,
                                  c = .09, e = .01)

data_Pararge <-
  data %>% filter(sp_latin == "Pararge aegeria") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

paraeg <-irregular_single_dataset(dataframe = data_Pararge,
                                  vector = 2:34,
                                  c = .09, e = .01)

data_PyroniaCeci <-
  data %>% filter(sp_latin == "Pyronia cecilia") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

pyrcec <-irregular_single_dataset(dataframe = data_PyroniaCeci,
                                  vector = 2:34,
                                  c = .09, e = .01)

data_PyroniaBath <-
  data %>% filter(sp_latin == "Pyronia bathseba") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

pyrbath <-irregular_single_dataset(dataframe = data_PyroniaBath,
                                  vector = 2:31,
                                  c = .09, e = .01)

data_Anthocharis <-
  data %>% filter(sp_latin == "Anthocharis euphenoides") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

anteuph <-irregular_single_dataset(dataframe = data_Anthocharis,
                                   vector = 2:31,
                                   c = .09, e = .01)

data_Cyaniris <-
  data %>% filter(sp_latin == "Cyaniris semiargus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

cyasem <-irregular_single_dataset(dataframe = data_Cyaniris,
                                   vector = 2:31,
                                   c = .09, e = .01)

View(data_celastrina)

lista_datos <- list() #inicializa una lista vacía con nombre lista_datos
lista_vectores <- list() #inicializa otra lista vacia para meter los vectores

n <- 1 #Estableces un contador
for(i in IDitin){ #Empiezas bucle
  # Creas el dataframe con todas las especies como ya sabes hacer.
  # Filtras ese dataframe (la columna de las especies) para quedarte solo con la especie que quieres mirar.
  # Si el dataframe filtrado no contiene la especie pasar al siguiente dataframe.
  # Ver el numero de columnas del data.frame de la especie (ncol(dataframe))
  lista_datos[[n]] <- dataframe
  lista_vectores[[n]] <- 2:ncol(dataframe) # diria que ahí están los datos
  n <- n + 1
}

list_colums <- ncol(results)
#####################################################################################################################
#####################################################################################################################

# The piece of code that follows requires three libraries: 
library(tidyverse)
library(island)
library(data.table)

library(openxlsx)

# Legir positions dels intineraris (lat, long)
itin_CBMS_RegClim <-read.xlsx("~/My_Documents/Work/My_Supervised_Students/Undergrads/PRACTIQUES-UdG/JOANA_DEKKER/itin_CBMS_regionsclimatiques.xlsx", sheet = 1)
# data includes 2024 year 

iditin_vals <- unique(data$IDitin)

results <- list()
results_bis <- list()

colonization <- vector()
extinction   <- vector()
itineraris   <- vector()
latitud      <- vector()
longitud     <- vector()

CI_Colonization <- matrix()
CI_Extinction   <- matrix()

lista_vectores <- list()

colext_Result <- list()

n <- 1

for (id in iditin_vals) {
      
           results[[as.character(id)]] <- data %>%
                                          filter(IDitin == id) %>%
                                          group_by(Any, sp_latin) %>%
                                          count() %>%
                                          pivot_wider(names_from = Any, values_from = n) %>%
                                          ungroup() %>%
                                          mutate(across(!sp_latin, negate(is.na))) %>%
                                          mutate(across(!sp_latin, as.numeric))
             
           nCols <- ncol(as.data.table(results[as.character(id)]))
           
           if(nCols >= 3) {
             lista_vectores[[as.character(id)]] <- 2:nCols
             results_bis[[as.character(id)]] <- as.data.table(results[[as.character(id)]])   
            
             colext_Result[[as.character(id)]] <- irregular_single_dataset(dataframe = results_bis[[as.character(id)]], 
                                                                           vector = 2:nCols, c = 0.09, e = 0.1, CI = FALSE)
             A <- as.data.table(colext_Result[[as.character(id)]])
             
             colonization[n] <- A$c
             extinction[n]   <- A$e
             
             CI_Colonization[n][1] <- A$c_low
             CI_Colonization[n][2] <- A$c_up
             
             CI_Extinction[n][1] <- A$e_low
             CI_Extinction[n][2] <- A$e_up
             
             itineraris[n]   <- id
             
             latitud[n]  <- itin_CBMS_RegClim$LAT[id]
             
             longitud[n] <- itin_CBMS_RegClim$LNG[id]
             
             n <- n + 1
           }
}

#####################################################################################################################
#####################################################################################################################

data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data %>% filter(IDitin == id) %>% group_by(Any, sp_latin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!sp_latin, negate(is.na))) %>%
  mutate(across(!sp_latin, as.numeric))

Species_Latin_Names <- unique(data$sp_latin)

n_sp = 0
lista_datos_spp <- list(list())

for (sp in Species_Latin_Names ) {
  
  n_sp = n_sp + 1 
  
  lista_datos_spp[[n_sp]]     <- list() #inicializa una lista vacía con nombre lista_datos
  lista_vectores_spp[[n_sp]]  <- list() #inicializa otra lista vacia para meter los vectores
  
  # lista_datos_spp     <- list() #inicializa una lista vacía con nombre lista_datos
  # lista_vectores_spp  <- list() #inicializa otra lista vacia para meter los vectores
  
  n <- 1 #Estableces un contador
  for(id in iditin_vals){ #Empiezas bucle
    
    # Creas el dataframe con todas las especies como ya sabes hacer.
    B <- data %>% filter(IDitin == id) %>%
                  group_by(Any, sp_latin) %>%
                  count() %>%
                  pivot_wider(names_from = Any, values_from = n) %>%
                  ungroup() %>%
                  mutate(across(!sp_latin, negate(is.na))) %>%
                  mutate(across(!sp_latin, as.numeric))
    
    # Filtras ese dataframe (la columna de las especies) para quedarte solo con la especie que quieres mirar.
    C <- B %>% filter(sp_latin == sp) 
    
    # Si el dataframe filtrado no contiene la especie pasar al siguiente dataframe.
    if ( nrow(C) == 0 || ncol(C) == 2 ) {
      print("L'especies no hi es o si hi es apareix nomes un any")
    }
    else {
        # Ver el numero de columnas del data.frame de la especie (ncol(dataframe))
        lista_datos_spp[[n_sp]][[as.character(id)]]    <- C
        
        names <- colnames(C);
        
        lista_vectores_spp[[n_sp]][[as.character(id)]] <- as.numeric(names[2:ncol(C)]) # diria que ahí están los datos
        n <- n + 1
    }
  }
  # Calcular el nombre d'anys de cada itinerari de l'especie sp 
  # lista_vectores_spp[[n_sp]]  
  # Usar la lista dels itineraris on es troba l'especies sp:
  # lista_datos_spp[[n_sp]]
  # per a calcular (c e) d'aquesta especie en tots els itineraris
  # usant irregular.multiple.dataset(...)
}

list_colums <- ncol(results)
#####################################################################################################################
#####################################################################################################################
# Mantel test: 
colon_extinc <- data.frame( colonization, extinction )
colon_extinc_itinID <- data.frame( colonization, extinction, itineraris)
posicions <- data.frame( latitud, longitud )
dist_geo <- dist(posicions, method = "euclidean")
dist_ecol <- dist(colon_extinc, method = "euclidean")
library(vegan)
mantel_resultat <- mantel(dist_geo, dist_ecol, method = "pearson", permutations = 999)
print(mantel_resultat)

################################################################################################################
# Nombre de transectes per any. 
library(tidyverse)

Samplying_Years <- read.csv("~/My_Documents/Work/My_Supervised_Students/Undergrads/PRACTIQUES-UdG/JOANA_DEKKER/cbms_sampling_years.csv")

# Create the presence matrix with SITE_IDs as rows and years as columns
presence_matrix <- Samplying_Years %>%
  pivot_wider(names_from = year, values_from = presence, values_fill = 0)

# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts <- colSums(presence_matrix[,-1])

# View the first few rows
print(head(presence_matrix))

# Convert named numeric vector to data frame
yearly_df <- enframe(yearly_counts, name = "year", value = "count")

# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_df$year <- as.numeric(as.character(yearly_df$year))

# Plot using ggplot2
ggplot(yearly_df, aes(x = year, y = count)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Sites Sampled per Year",
       x = "Year",
       y = "Number of Sites") +
  theme_minimal()

# Identify the range of years
start_year <- min(yearly_df$year)
end_year <- max(yearly_df$year)

# Generate axis breaks: every 5 years, plus start and end if not already included
breaks <- sort(unique(c(seq(from = start_year, to = end_year, by = 5), start_year, end_year)))

# Plot
ggplot(yearly_df, aes(x = year, y = count)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  labs(title = "Number of Sites Sampled per Year",
       x = "Year",
       y = "Number of Sites") +
  theme_minimal()

# Plot
ggplot(yearly_df, aes(x = year, y = count)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  labs(x = "Any",
       y = "Nombre d'itineraris") +
  theme_minimal()

################################################################################
# Calcul de les ocupancies de les 12 especies. 

# Celastrina argiolus
################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_celastrina <- colSums(data_celastrina[,-1])
# Convert named numeric vector to data frame
yearly_presence_celastrina_df <- enframe(yearly_presence_celastrina, name = "year", value = "count")
presence_94_2023_celastrina_df <- yearly_presence_celastrina_df[-c(1:3), ]

# Lycaena virgareae
################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_lycaena <- colSums(data_Lycaena[,-1])
# Convert named numeric vector to data frame
yearly_presence_lycaena_df <- enframe(yearly_presence_lycaena, name = "year", value = "count")
# Aquest data frame no te tots els anys, per n'hi ha alguns on l'especie no va ser observada en cap itinerari. 
# Les comandes seguents son per afegir 0 els anys on l'especie no va ser observada en cap itinerari.
library(dplyr)
# yearly_presence_lycaena_df is my data frame with 'year' and 'count'
# Step 1: Create a sequence of all years
years_94_2023 <- data.frame(year = seq(1994, max(yearly_presence_lycaena_df$year), by = 1))
yearly_presence_lycaena_df$year <- as.numeric(as.character(yearly_presence_lycaena_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_lycaena_df_complete <- years_94_2023 %>%
  left_join(yearly_presence_lycaena_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2023_lycaena_df <- yearly_presence_lycaena_df_complete

# Plebejus argus
################
yearly_presence_plebejus <- colSums(data_Plebejus[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_df <- enframe(yearly_presence_plebejus, name = "year", value = "count")

presence_94_2023_plebejus_df <- yearly_presence_plebejus_df[-c(1:3), ]

# Preparar el data frame per calcular ocupancies: 
yearly_df_2023 <- yearly_df[-31,]
presence_94_2023_celastrina_df$No_of_IT <- yearly_df_2023$count
presence_94_2023_celastrina_df$occupancy <- presence_94_2023_celastrina_df$count/presence_94_2023_celastrina_df$No_of_IT


