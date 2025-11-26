### A continuacio s'utilitza la primera part del codi de les ocupancies BR1, BR2 I BR3###
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


# Create the the matrix of SITE_IDs (1: sampled, 0: non-sampled) as rows and years 
# as columns:
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
###Aquesta matriu son els anys mostrejats i no mostrejats dels itineratis de la Regio 1.
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts_BR1 <- colSums(presence_matrix_BR1[,-1])
# Convert named numeric vector to data frame
yearly_counts_BR1_df <- enframe(yearly_counts_BR1, name = "year", value = "count")
# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_counts_BR1_df$year <- as.numeric(as.character(yearly_counts_BR1_df$year))
###################

# Create the presence matrix with SITE_IDs as rows and years as columns
presence_matrix_BR2 <- presence_matrix[presence_matrix$SITE_ID %in% itin_ID_2, ]
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts_BR2 <- colSums(presence_matrix_BR2[,-1])
# Convert named numeric vector to data frame
yearly_counts_BR2_df <- enframe(yearly_counts_BR2, name = "year", value = "count")
# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_counts_BR2_df$year <- as.numeric(as.character(yearly_counts_BR2_df$year))
################

# Create the presence matrix with SITE_IDs as rows and years as columns
presence_matrix_BR3 <- presence_matrix[presence_matrix$SITE_ID %in% itin_ID_3, ]
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_counts_BR3 <- colSums(presence_matrix_BR3[,-1])
# Convert named numeric vector to data frame
yearly_counts_BR3_df <- enframe(yearly_counts_BR3, name = "year", value = "count")
# Make sure 'year' is numeric for proper ordering on the x-axis
yearly_counts_BR3_df$year <- as.numeric(as.character(yearly_counts_BR3_df$year))
###############

# Input arguments: 
# Sp: Nom de les especies
# Bioreg: Nom de les bioregions. 
Species_Latin_Names <- unique(data$sp_latin)
Sp = Species_Latin_Names

# c("Celastrina argiolus", "Lycaena virgaureae", "Plebejus argus", 
#       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
#       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
#       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia")

# Sp = c("Celastrina Argiolus")

File_Pre_Names <- c(
  "cela", "lyca", "plebe",
  "pseudo", "cyani", "vane",
  "agla", "antho", "mela",
  "para", "pyrobath", "pyroceci"
)

BioReg = c("Regio Alpina i Subalpina", 
           "Regio Mediterranea humida", 
           "Regio Mediterranea arida")

itin_ID_List <- list(itin_ID_1, itin_ID_2, itin_ID_3)

Itinerary_List <- list(presence_matrix_BR1, presence_matrix_BR2, presence_matrix_BR3)

yearly_counts_BioReg_List = list(yearly_counts_BR1_df, yearly_counts_BR2_df, yearly_counts_BR3_df)

n_List = list("n1", "n2", "n3")

N_List = list("N1", "N2", "N3")

M_List = list("M1", "M2", "M3")

list_Chi2_Function <- function(data, 
                               Itinerary_List, yearly_counts_BioReg_List, itin_ID_List,  
                               n_List, N_List, M_List, 
                               Sp, BioReg)
{
  
  list_chi2 <- list()
  
  for (i in 1:12 ) {
    file_name <- paste0(
      "/home/dalonso/PROJECT_JOANA_TFG/DADES/",
      File_Pre_Names[i],
      "_chi2.RData"
    )
    
    data_Sp <- 
      data %>% filter(sp_latin == Sp[i]) %>% group_by(Any, IDitin) %>% count() %>%
      pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
      mutate(across(!IDitin, negate(is.na))) %>%
      mutate(across(!IDitin, as.numeric))
    
    sp_chi2 <- data.frame()   # Do NOT overwrite this inside j-loop
    sp_list <- list()         # temporary list for the 3 bioregions
    
    for (j in 1:3) {
      
      FES <- 1 # Only if "FES" is changed to 0, the plot is not done!!!
      
      # Controlar especies que no hi son presents en alguna regio bioclimatic
      if (j == 3 && i == 2) { # Lyca (Sp i=2) no hi es present en regio j=3 
        print(paste("No data for species", Sp[i], "in:", BioReg[j], 
                    "No calculation possible!!!"))
        FES = 0
      }
      if (j == 2 && i == 2) { # Lyca (Sp i=2) no hi es present en regio j=2 
        print(paste("No data for species", Sp[i], "in:", BioReg[j], 
                    "No calculation possible!!!"))
        FES = 0
      }
      if (j == 3 && i == 5) { # Cyani (Sp i=5) no hi es present en regio j=3  
        print(paste("No data for species", Sp[i], "in", BioReg[j], 
                    "No calculation possible!!!"))  
        FES = 0
      }
      if ( FES == 1 ) {
        print(paste("Species", Sp[i], "in:", BioReg[j]))
        # Pause until user presses Enter
        readline(prompt = "Press [Enter] to continue...")
      
        data_Sp_BioReg <-data_Sp[data_Sp$IDitin %in% itin_ID_List[[j]], ]
        
        # Define the full set of years you want
        all_years <- as.character(1994:2024)
        # Find which years are missing in your data frame
        missing <- setdiff(all_years, names(data_Sp_BioReg))
        # Add each missing year as a column filled with 0
        data_Sp_BioReg[missing] <- 0
        # Optionally sort columns chronologically
        data_Sp_BioReg <- data_Sp_BioReg[, c("IDitin", all_years)]
  
        # data_Sp_BioReg_94 <- data_Sp_BioReg[,-c(2:4)]
        year_cols <- grep("^[0-9]{4}$", colnames(data_Sp_BioReg))  # columns that are 4-digit years
        keep_cols <- year_cols[as.numeric(colnames(data_Sp_BioReg)[year_cols]) >= 1994]
        keep_cols <- c(1, keep_cols)
      
        data_Sp_BioReg_94 <- data_Sp_BioReg[, keep_cols, drop = FALSE]
      
        data_Sp_BioReg_94_DEF <- data_Sp_BioReg_94
      
        itin_ID_BioReg_Sp <- data_Sp_BioReg_94$IDitin 
        
        Itinerary_Matrix_BioReg <- Itinerary_List[[j]]
        
        yearly_counts_BioReg_df <- yearly_counts_BioReg_List[[j]]
        
        n_BR = n_List[[j]]
        
        N_BR = N_List[[j]]
        
        M_BR = M_List[[j]]
        
        Itinerary_Matrix_Sp_BioReg <-Itinerary_Matrix_BioReg[Itinerary_Matrix_BioReg$SITE_ID %in% itin_ID_BioReg_Sp, ]
        
        # Ordenem els dos data frames segons ID del itinerari (1a columna). 
        data_Sp_BioReg_94_DEF <- data_Sp_BioReg_94_DEF[
          order(data_Sp_BioReg_94_DEF[[1]]),]
        
        Itinerary_Matrix_Sp_BioReg <- Itinerary_Matrix_Sp_BioReg[
          order(Itinerary_Matrix_Sp_BioReg[[1]]),]
        
        data_Sp_BioReg_94_DEF[Itinerary_Matrix_Sp_BioReg == 0] <- 2
        
        # Eliminim la primera columna
        years <- colnames(data_Celastrina_BR1_94_DEF)[-1]
        data_Sp_BioReg_94_EF <-data_Sp_BioReg_94_DEF[,-1]
        
        metapo <- sapply(seq_along(years), function(i) { # 
          
          subset <- data_Sp_BioReg_94_EF[, 1:i, drop = FALSE]
          # 1) Ha tenido al menos un 1 en algún momento hasta el año i
          has_presence_before <- apply(subset == 1, 1, any)
          # 2) En el año i NO tiene un 2 (es decir, fue muestreado)
          not_unsampled_this_year <- subset[, i] != 2
          # Itinerarios que cumplen ambas
          sum(has_presence_before & not_unsampled_this_year)
        })
        
        yearly_presence_Sp_BioReg <- colSums(data_Sp_BioReg[,-1])
        
        yearly_presence_Sp_BioReg_df <- enframe(yearly_presence_Sp_BioReg, name = "year", value = "count")
        
        presence_94_2024_Sp_BioReg_df <- yearly_presence_Sp_BioReg_df
        
        presence_94_2024_Sp_BioReg_df$year <- as.numeric(presence_94_2024_Sp_BioReg_df$year)
        
        presence_94_2024_Sp_BioReg_df <- presence_94_2024_Sp_BioReg_df[
          presence_94_2024_Sp_BioReg_df$year >= 1994, ]
        
        #prepararmos el dataframe para el calculo de las ocupancias#
        presence_94_2024_Sp_BioReg_df$No_of_IT <- yearly_counts_BioReg_df$count
        
        # sp_chi2 <- presence_94_2024_Sp_BioReg_df
        tmp <- presence_94_2024_Sp_BioReg_df   # local df for this j
        
        # rename columns n, N, M correctly
        names(tmp)[names(tmp) == "count"] <- n_BR
        names(tmp)[names(tmp) == "No_of_IT"] <- N_BR
        names(tmp)[names(tmp) == "M"] <- M_BR
        
        sp_list[[j]] <- tmp   # store result for each bioregion
        
        #Agregamos n i N (de la BR corresponent) al dataframe de l'especie
        sp_chi2$count <- presence_94_2024_Sp_BioReg_df$count
        names(sp_chi2)[names(sp_chi2) == "count"] <- n_BR 
        
        sp_chi2$No_of_IT <- presence_94_2024_Sp_BioReg_df$No_of_IT
        names(sp_chi2)[names(sp_chi2) == "No_of_IT"] <- N_BR
        
        sp_chi2$M <- metapo
        names(sp_chi2)[names(sp_chi2) == "M"] <- M_BR 
      }
    }
    
    # After the j-loop finishes, combine the 3 bioregions:
    sp_chi2 <- Reduce(function(x,y) merge(x, y, by="year", all=TRUE), sp_list)
    
    # Salvar sp_chi2
    save(sp_chi2, file = file_name)
  
    list_chi2[[i]] <- sp_chi2
  }
  
  return(list_chi2)
}

my_list <- list_Chi2_Function(data, 
                              Itinerary_List, yearly_counts_BioReg_List, itin_ID_List, 
                              n_List, N_List, M_List,  
                              Sp, BioReg)

###############
data_Celastrina <-
  data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Celastrina_BR1 <-data_Celastrina[data_Celastrina$IDitin %in% itin_ID_1, ]

# data_Celastrina_BR1_94 <- data_Celastrina_BR1[,-c(2:4)]

year_cols <- grep("^[0-9]{4}$", colnames(data_Celastrina_BR1))  # columns that are 4-digit years
keep_cols <- year_cols[as.numeric(colnames(data_Celastrina_BR1)[year_cols]) >= 1994]

data_Celastrina_BR1_94 <- data_Celastrina_BR1[, keep_cols, drop = FALSE]


data_Celastrina_BR1_94_DEF <- data_Celastrina_BR1_94

itin_ID_1_Celastrina <- data_Celastrina_BR1_94$IDitin

presence_matrix_Celastrina_BR1 <-presence_matrix_BR1[presence_matrix_BR1$SITE_ID %in% itin_ID_1_Celastrina, ]

# Ordenem els dos data frames segons ID del itinerari (1a columna). 
data_Celastrina_BR1_94_DEF <- data_Celastrina_BR1_94_DEF[
  order(data_Celastrina_BR1_94_DEF[[1]]),]

presence_matrix_Celastrina_BR1 <- presence_matrix_Celastrina_BR1[
  order(presence_matrix_Celastrina_BR1[[1]]),]

data_Celastrina_BR1_94_DEF[presence_matrix_Celastrina_BR1 == 0] <- 2

# Eliminim la primera columna
years <- colnames(data_Celastrina_BR1_94_DEF)[-1]
data_Celastrina_BR1_94_EF <-data_Celastrina_BR1_94_DEF[,-1]

metapo_cela <- sapply(seq_along(years), function(i) { # 
  
  subset <- data_Celastrina_BR1_94_EF[, 1:i, drop = FALSE]
  # 1) Ha tenido al menos un 1 en algún momento hasta el año i
  has_presence_before <- apply(subset == 1, 1, any)
  # 2) En el año i NO tiene un 2 (es decir, fue muestreado)
  not_unsampled_this_year <- subset[, i] != 2
  # Itinerarios que cumplen ambas
  sum(has_presence_before & not_unsampled_this_year)
})

####
data_Celastrina <-
  data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Celastrina_BR2 <-data_Celastrina[data_Celastrina$IDitin %in% itin_ID_2, ]

###
data_Celastrina <-
  data %>% filter(sp_latin == "Celastrina argiolus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Celastrina_BR3 <-data_Celastrina[data_Celastrina$IDitin %in% itin_ID_3, ]
###

###Aqui es on realment calculavem les ocupancies, nosaltres necesitem les presencies i absencies (n)
# i el numero total d'anys mostrejats (N) de cada itinerari.


# Celastrina argiolus BR1
##########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_celastrina_BR1 <- colSums(data_Celastrina_BR1[,-1])
# Convert named numeric vector to data frame
yearly_presence_celastrina_BR1_df <- enframe(yearly_presence_celastrina_BR1, name = "year", value = "count")
presence_94_2024_celastrina_BR1_df <- yearly_presence_celastrina_BR1_df[-c(1:3), ]
#prepararmos el dataframe para el calculo de las ocupancias#
presence_94_2024_celastrina_BR1_df$No_of_IT <- yearly_counts_BR1_df$count
#Convertimos year en numerico para despues poder hacer bien el grafico despues
presence_94_2024_celastrina_BR1_df$year <- as.numeric(presence_94_2024_celastrina_BR1_df$year)

#creamos df de celastrina con el nombre cela_Chi2 
cela_chi2 <-data.frame()
cela_chi2 <- presence_94_2024_celastrina_BR1_df
names(cela_chi2)[names(cela_chi2) == "count"] <- "n1"
names(cela_chi2)[names(cela_chi2) == "No_of_IT"] <- "N1" 
cela_chi2$M1 <- metapo_cela

# Celastrina argiolus BR2 #aqui se calcula n2 i N2 (las presencias y los itinerarios de la BR2)
##########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_celastrina_BR2 <- colSums(data_Celastrina_BR2[,-1])
# Convert named numeric vector to data frame
yearly_presence_celastrina_BR2_df <- enframe(yearly_presence_celastrina_BR2, name = "year", value = "count")
presence_94_2024_celastrina_BR2_df <- yearly_presence_celastrina_BR2_df[-c(1:3), ]
#prepararmos el dataframe para el calculo de las ocupancias#
presence_94_2024_celastrina_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

#Agregamos n2 i N2 al dataframe de celatrina
cela_chi2$n2 <- presence_94_2024_celastrina_BR2_df$count
cela_chi2$N2 <- presence_94_2024_celastrina_BR2_df$No_of_IT

# Celastrina argiolus BR3
##########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_celastrina_BR3 <- colSums(data_Celastrina_BR3[,-1])
# Convert named numeric vector to data frame
yearly_presence_celastrina_BR3_df <- enframe(yearly_presence_celastrina_BR3, name = "year", value = "count")
presence_94_2024_celastrina_BR3_df <- yearly_presence_celastrina_BR3_df[-c(1:3), ]
#prepararmos el dataframe para el calculo de las ocupancias#
presence_94_2024_celastrina_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

#Agregamos n3 i N3 al dataframe de celatrina
cela_chi2$n3 <- presence_94_2024_celastrina_BR3_df$count
cela_chi2$N3 <- presence_94_2024_celastrina_BR3_df$No_of_IT

save(cela_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/cela_chi2.RData")

###########################
###########################
#Cyaniris semiargus
##########################
data_Cyaniris <-
  data %>% filter(sp_latin == "Cyaniris semiargus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Cyaniris_BR1 <-data_Cyaniris[data_Cyaniris$IDitin %in% itin_ID_1, ]

###

data_Cyaniris <-
  data %>% filter(sp_latin == "Cyaniris semiargus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Cyaniris_BR2 <-data_Cyaniris[data_Cyaniris$IDitin %in% itin_ID_2, ]
###
data_Cyaniris <-
  data %>% filter(sp_latin == "Cyaniris semiargus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Cyaniris_BR3 <-data_Cyaniris[data_Cyaniris$IDitin %in% itin_ID_3, ]
###
yearly_presence_cyaniris_BR1<- colSums(data_Cyaniris_BR1[,-1])
yearly_presence_cyaniris_BR1_df <- enframe(yearly_presence_cyaniris_BR1, name = "year", value = "count")

library(dplyr)
# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_cyaniris_BR1_df$year), by = 1))
yearly_presence_cyaniris_BR1_df$year <- as.numeric(as.character(yearly_presence_cyaniris_BR1_df$year))
#

yearly_presence_cyaniris_BR1_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_cyaniris_BR1_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_cyaniris_BR1_df <- yearly_presence_cyaniris_BR1_df_complete
presence_94_2024_cyaniris_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de cyaniris 
cyani_chi2 <-data.frame()
cyani_chi2 <- presence_94_2024_cyaniris_BR1_df
names(cyani_chi2)[names(cyani_chi2) == "count"] <- "n1"
names(cyani_chi2)[names(cyani_chi2) == "No_of_IT"] <- "N1" 


###BR2#
yearly_presence_cyaniris_BR2<- colSums(data_Cyaniris_BR2[,-1])
yearly_presence_cyaniris_BR2_df <- enframe(yearly_presence_cyaniris_BR2, name = "year", value = "count")

# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_cyaniris_BR2_df$year), by = 1))
yearly_presence_cyaniris_BR2_df$year <- as.numeric(as.character(yearly_presence_cyaniris_BR2_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_cyaniris_BR2_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_cyaniris_BR2_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_cyaniris_BR2_df <- yearly_presence_cyaniris_BR2_df_complete
presence_94_2024_cyaniris_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

#Agregamos n2 i N2 al dataframe de cyaniris
cyani_chi2$n2 <- presence_94_2024_cyaniris_BR2_df$count
cyani_chi2$N2 <- presence_94_2024_cyaniris_BR2_df$No_of_IT

###BR3#
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

#Agregamos n3 i N3 al dataframe de cyaniris
cyani_chi2$n3 <- presence_94_2024_cyaniris_BR3_df$count
cyani_chi2$N3 <- presence_94_2024_cyaniris_BR3_df$No_of_IT

save(cyani_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/cyani_chi2.RData")
#############################
#############################

# Lycaena virgareae
###########################
data_Lycaena <-
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Lycaena_BR1 <-data_Lycaena[data_Lycaena$IDitin %in% itin_ID_1, ]
###
###
data_Lycaena <-
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Lycaena_BR2 <-data_Lycaena[data_Lycaena$IDitin %in% itin_ID_2, ]
###
###
data_Lycaena <-
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Lycaena_BR3 <-data_Lycaena[data_Lycaena$IDitin %in% itin_ID_3, ]
###
#BR1
yearly_presence_lycaena_BR1 <- colSums(data_Lycaena_BR1[,-1])
# Convert named numeric vector to data frame
yearly_presence_lycaena_BR1_df <- enframe(yearly_presence_lycaena_BR1, name = "year", value = "count")
# Aquest data frame no te tots els anys, per n'hi ha alguns on l'especie no va ser observada en cap itinerari. 
# Les comandes seguents son per afegir 0 els anys on l'especie no va ser observada en cap itinerari.
# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_lycaena_BR1_df$year), by = 1))
yearly_presence_lycaena_BR1_df$year <- as.numeric(as.character(yearly_presence_lycaena_BR1_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_lycaena_BR1_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_lycaena_BR1_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_lycaena_BR1_df <- yearly_presence_lycaena_BR1_df_complete
presence_94_2024_lycaena_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de lycaena 
lyca_chi2 <-data.frame()
lyca_chi2 <- presence_94_2024_lycaena_BR1_df
names(lyca_chi2)[names(lyca_chi2) == "count"] <- "n1"
names(lyca_chi2)[names(lyca_chi2) == "No_of_IT"] <- "N1" 

#BR2
# Lycaena virgareae
###########################
# Exclude the SITE_ID column and sum across columns (i.e., years)
yearly_presence_lycaena_BR2 <- colSums(data_Lycaena_BR2[,-1])
# Convert named numeric vector to data frame
yearly_presence_lycaena_BR2_df <- enframe(yearly_presence_lycaena_BR2, name = "year", value = "count")
# Aquest data frame no te tots els anys, per n'hi ha alguns on l'especie no va ser observada en cap itinerari. 
# Les comandes seguents son per afegir 0 els anys on l'especie no va ser observada en cap itinerari.
library(dplyr)
# Step 1: Create a sequence of all years
years_94_2024 <- data.frame(year = seq(1994, max(yearly_presence_lycaena_BR2_df$year), by = 1))
yearly_presence_lycaena_BR2_df$year <- as.numeric(as.character(yearly_presence_lycaena_BR2_df$year))

# Step 2: Merge the sequence with your data frame, filling missing years with 0
yearly_presence_lycaena_BR2_df_complete <- years_94_2024 %>%
  left_join(yearly_presence_lycaena_BR2_df, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))
presence_94_2024_lycaena_BR2_df <- yearly_presence_lycaena_BR2_df_complete
presence_94_2024_lycaena_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

#Agregamos n2 i N2 al dataframe de lycaena
lyca_chi2$n2 <- presence_94_2024_lycaena_BR2_df$count
lyca_chi2$N2 <- presence_94_2024_lycaena_BR2_df$No_of_IT


#BR3
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

#Agregamos n3 i N3 al dataframe de lycaena
lyca_chi2$n3 <- presence_94_2024_lycaena_BR3_df$count
lyca_chi2$N3 <- presence_94_2024_lycaena_BR3_df$No_of_IT

save(lyca_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/cyani_chi2.RData")

###
# Plebejus argus
##########################
###########################
data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Plebejus_BR1 <-data_Plebejus[data_Plebejus$IDitin %in% itin_ID_1, ]
###
###
data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Plebejus_BR2 <-data_Plebejus[data_Plebejus$IDitin %in% itin_ID_2, ]
###
###
data_Plebejus <-
  data %>% filter(sp_latin == "Plebejus argus") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Plebejus_BR3 <-data_Plebejus[data_Plebejus$IDitin %in% itin_ID_3, ]
###BR1
yearly_presence_plebejus_BR1 <- colSums(data_Plebejus_BR1[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_BR1_df <- enframe(yearly_presence_plebejus_BR1, name = "year", value = "count")
presence_94_2024_plebejus_BR1_df <- yearly_presence_plebejus_BR1_df[-c(1:3), ]
presence_94_2024_plebejus_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de plebejus 
plebe_chi2 <-data.frame()
plebe_chi2 <- presence_94_2024_plebejus_BR1_df
names(plebe_chi2)[names(plebe_chi2) == "count"] <- "n1"
names(plebe_chi2)[names(plebe_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_plebejus_BR2 <- colSums(data_Plebejus_BR2[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_BR2_df <- enframe(yearly_presence_plebejus_BR2, name = "year", value = "count")
presence_94_2024_plebejus_BR2_df <- yearly_presence_plebejus_BR2_df[-c(1:3), ]
presence_94_2024_plebejus_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

plebe_chi2$n2 <- presence_94_2024_plebejus_BR2_df$count
plebe_chi2$N2 <- presence_94_2024_plebejus_BR2_df$No_of_IT

###BR3
yearly_presence_plebejus_BR3 <- colSums(data_Plebejus_BR3[,-1])
# Convert named numeric vector to data frame
yearly_presence_plebejus_BR3_df <- enframe(yearly_presence_plebejus_BR3, name = "year", value = "count")
presence_94_2024_plebejus_BR3_df <- yearly_presence_plebejus_BR3_df[-c(1:3), ]
presence_94_2024_plebejus_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

plebe_chi2$n3 <- presence_94_2024_plebejus_BR3_df$count
plebe_chi2$N3 <- presence_94_2024_plebejus_BR3_df$No_of_IT

save(plebe_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/plebe_chi2.RData")

###
#Vanessa cardui
#######################
data_Vanessa <-
  data %>% filter(sp_latin == "Vanessa cardui") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Vanessa_BR1 <-data_Vanessa[data_Vanessa$IDitin %in% itin_ID_1, ]
###
data_Vanessa <-
  data %>% filter(sp_latin == "Vanessa cardui") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Vanessa_BR2 <-data_Vanessa[data_Vanessa$IDitin %in% itin_ID_2, ]
###
data_Vanessa <-
  data %>% filter(sp_latin == "Vanessa cardui") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Vanessa_BR3 <-data_Vanessa[data_Vanessa$IDitin %in% itin_ID_3, ]

###BR1
yearly_presence_vanessa_BR1 <-colSums(data_Vanessa_BR1[,-1])
#convertir aquest vector en un data frame
yearly_presence_vanessa_BR1_df <- enframe(yearly_presence_vanessa_BR1, name = "year", value = "count")
#eliminem les 3 primeres files
presence_94_2024_vanessa_BR1_df <- yearly_presence_vanessa_BR1_df[-c(1:3), ]
#Afegir la columna de sampling years per calcular la ocupancia
presence_94_2024_vanessa_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de vanessa 
vane_chi2 <-data.frame()
vane_chi2 <- presence_94_2024_vanessa_BR1_df
names(vane_chi2)[names(vane_chi2) == "count"] <- "n1"
names(vane_chi2)[names(vane_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_vanessa_BR2 <-colSums(data_Vanessa_BR2[,-1])
#convertir aquest vector en un data frame
yearly_presence_vanessa_BR2_df <- enframe(yearly_presence_vanessa_BR2, name = "year", value = "count")
#eliminem les 3 primeres files
presence_94_2024_vanessa_BR2_df <- yearly_presence_vanessa_BR2_df[-c(1:3), ]
#Afegir la columna de sampling years per calcular la ocupancia
presence_94_2024_vanessa_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

vane_chi2$n2 <- presence_94_2024_vanessa_BR2_df$count
vane_chi2$N2 <- presence_94_2024_vanessa_BR2_df$No_of_IT

###BR3
yearly_presence_vanessa_BR3 <-colSums(data_Vanessa_BR3[,-1])
#convertir aquest vector en un data frame
yearly_presence_vanessa_BR3_df <- enframe(yearly_presence_vanessa_BR3, name = "year", value = "count")
#eliminem les 3 primeres files
presence_94_2024_vanessa_BR3_df <- yearly_presence_vanessa_BR3_df[-c(1:3), ]
#Afegir la columna de sampling years per calcular la ocupancia
presence_94_2024_vanessa_BR3_df$No_of_IT <- yearly_counts_BR3_df$count


vane_chi2$n3 <- presence_94_2024_vanessa_BR3_df$count
vane_chi2$N3 <- presence_94_2024_vanessa_BR3_df$No_of_IT

save(vane_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/vane_chi2.RData")
############

#Pseudophilotes panoptes
############################
data_Pseudophilotes <-
  data %>% filter(sp_latin == "Pseudophilotes panoptes") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pseudophilotes_BR1 <-data_Pseudophilotes[data_Pseudophilotes$IDitin %in% itin_ID_1, ]
###
data_Pseudophilotes <-
  data %>% filter(sp_latin == "Pseudophilotes panoptes") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pseudophilotes_BR2 <-data_Pseudophilotes[data_Pseudophilotes$IDitin %in% itin_ID_2, ]
###
data_Pseudophilotes <-
  data %>% filter(sp_latin == "Pseudophilotes panoptes") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pseudophilotes_BR3 <-data_Pseudophilotes[data_Pseudophilotes$IDitin %in% itin_ID_3, ]
###
###BR1
yearly_presence_pseudophilotes_BR1 <-colSums(data_Pseudophilotes_BR1[,-1])
yearly_presence_pseudophilotes_BR1_df <- enframe(yearly_presence_pseudophilotes_BR1, name = "year", value = "count")
presence_94_2024_pseudophilotes_BR1_df <- yearly_presence_pseudophilotes_BR1_df
presence_94_2024_pseudophilotes_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de pseudophilotes 
pseudo_chi2 <-data.frame()
pseudo_chi2 <- presence_94_2024_pseudophilotes_BR1_df
names(pseudo_chi2)[names(pseudo_chi2) == "count"] <- "n1"
names(pseudo_chi2)[names(pseudo_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_pseudophilotes_BR2 <-colSums(data_Pseudophilotes_BR2[,-1])
yearly_presence_pseudophilotes_BR2_df <- enframe(yearly_presence_pseudophilotes_BR2, name = "year", value = "count")
presence_94_2024_pseudophilotes_BR2_df <- yearly_presence_pseudophilotes_BR2_df
presence_94_2024_pseudophilotes_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

pseudo_chi2$n2 <- presence_94_2024_pseudophilotes_BR2_df$count
pseudo_chi2$N2 <- presence_94_2024_pseudophilotes_BR2_df$No_of_IT

###BR3
yearly_presence_pseudophilotes_BR3 <-colSums(data_Pseudophilotes_BR3[,-1])
yearly_presence_pseudophilotes_BR3_df <- enframe(yearly_presence_pseudophilotes_BR3, name = "year", value = "count")
presence_94_2024_pseudophilotes_BR3_df <- yearly_presence_pseudophilotes_BR3_df
presence_94_2024_pseudophilotes_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

pseudo_chi2$n3 <- presence_94_2024_pseudophilotes_BR3_df$count
pseudo_chi2$N3 <- presence_94_2024_pseudophilotes_BR3_df$No_of_IT

save(pseudo_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/pseudo_chi2.RData")
############

####Aglais io
#############################
data_Aglais <-
  data %>% filter(sp_latin == "Aglais io") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Aglais_BR1 <-data_Aglais[data_Aglais$IDitin %in% itin_ID_1, ]
###
data_Aglais <-
  data %>% filter(sp_latin == "Aglais io") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Aglais_BR2 <-data_Aglais[data_Aglais$IDitin %in% itin_ID_2, ]
###
###
data_Aglais <-
  data %>% filter(sp_latin == "Aglais io") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Aglais_BR3 <-data_Aglais[data_Aglais$IDitin %in% itin_ID_3, ]
###
###BR1
yearly_presence_aglais_BR1 <-colSums(data_Aglais_BR1[,-1])
yearly_presence_aglais_BR1_df <- enframe(yearly_presence_aglais_BR1, name = "year", value = "count")
presence_94_2024_aglais_BR1_df <- yearly_presence_aglais_BR1_df[-c(1:3), ]
presence_94_2024_aglais_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de AGLAIS 
agla_chi2 <-data.frame()
agla_chi2 <- presence_94_2024_aglais_BR1_df
names(agla_chi2)[names(agla_chi2) == "count"] <- "n1"
names(agla_chi2)[names(agla_chi2) == "No_of_IT"] <- "N1" 

####BR2
yearly_presence_aglais_BR2 <-colSums(data_Aglais_BR2[,-1])
yearly_presence_aglais_BR2_df <- enframe(yearly_presence_aglais_BR2, name = "year", value = "count")
presence_94_2024_aglais_BR2_df <- yearly_presence_aglais_BR2_df[-c(1:3), ]
presence_94_2024_aglais_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

agla_chi2$n2 <- presence_94_2024_aglais_BR2_df$count
agla_chi2$N2 <- presence_94_2024_aglais_BR2_df$No_of_IT

###BR3
yearly_presence_aglais_BR3 <-colSums(data_Aglais_BR3[,-1])
yearly_presence_aglais_BR3_df <- enframe(yearly_presence_aglais_BR3, name = "year", value = "count")
presence_94_2024_aglais_BR3_df <- yearly_presence_aglais_BR3_df[-c(1:3), ]
presence_94_2024_aglais_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

agla_chi2$n3 <- presence_94_2024_aglais_BR3_df$count
agla_chi2$N3 <- presence_94_2024_aglais_BR3_df$No_of_IT

save(agla_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/agla_chi2.RData")
############

####Melanargia occitanica
#############################
data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Melanargia_BR1 <-data_Melanargia[data_Melanargia$IDitin %in% itin_ID_1, ]
###
###
data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Melanargia_BR2 <-data_Melanargia[data_Melanargia$IDitin %in% itin_ID_2, ]
###
###
data_Melanargia <-
  data %>% filter(sp_latin == "Melanargia occitanica") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Melanargia_BR3 <-data_Melanargia[data_Melanargia$IDitin %in% itin_ID_3, ]
###
###BR1
yearly_presence_melanargia_BR1 <-colSums(data_Melanargia_BR1[,-1])
yearly_presence_melanargia_BR1_df <- enframe(yearly_presence_melanargia_BR1, name = "year", value = "count")
presence_94_2024_melanargia_BR1_df <- yearly_presence_melanargia_BR1_df[-1, ]
presence_94_2024_melanargia_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de melanargia
mela_chi2 <-data.frame()
mela_chi2 <- presence_94_2024_melanargia_BR1_df
names(mela_chi2)[names(mela_chi2) == "count"] <- "n1"
names(mela_chi2)[names(mela_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_melanargia_BR2 <-colSums(data_Melanargia_BR2[,-1])
yearly_presence_melanargia_BR2_df <- enframe(yearly_presence_melanargia_BR2, name = "year", value = "count")
presence_94_2024_melanargia_BR2_df <- yearly_presence_melanargia_BR2_df[-1, ]
presence_94_2024_melanargia_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

mela_chi2$n2 <- presence_94_2024_melanargia_BR2_df$count
mela_chi2$N2 <- presence_94_2024_melanargia_BR2_df$No_of_IT

###BR3
yearly_presence_melanargia_BR3 <-colSums(data_Melanargia_BR3[,-1])
yearly_presence_melanargia_BR3_df <- enframe(yearly_presence_melanargia_BR3, name = "year", value = "count")
presence_94_2024_melanargia_BR3_df <- yearly_presence_melanargia_BR3_df[-1, ]
presence_94_2024_melanargia_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

mela_chi2$n3 <- presence_94_2024_melanargia_BR3_df$count
mela_chi2$N3 <- presence_94_2024_melanargia_BR3_df$No_of_IT

save(mela_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/mela_chi2.RData")
############

####Pararge aegeria
############################
data_Pararge <-
  data %>% filter(sp_latin == "Pararge aegeria") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pararge_BR1 <-data_Pararge[data_Pararge$IDitin %in% itin_ID_1, ]
###
###
data_Pararge <-
  data %>% filter(sp_latin == "Pararge aegeria") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pararge_BR2 <-data_Pararge[data_Pararge$IDitin %in% itin_ID_2, ]
###
###
data_Pararge <-
  data %>% filter(sp_latin == "Pararge aegeria") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Pararge_BR3 <-data_Pararge[data_Pararge$IDitin %in% itin_ID_3, ]
###
###BR1
yearly_presence_pararge_BR1 <-colSums(data_Pararge_BR1[,-1])
yearly_presence_pararge_BR1_df <- enframe(yearly_presence_pararge_BR1, name = "year", value = "count")
presence_94_2024_pararge_BR1_df <- yearly_presence_pararge_BR1_df[-c(1:3), ]
presence_94_2024_pararge_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de pararge 
para_chi2 <-data.frame()
para_chi2 <- presence_94_2024_pararge_BR1_df
names(para_chi2)[names(para_chi2) == "count"] <- "n1"
names(para_chi2)[names(para_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_pararge_BR2 <-colSums(data_Pararge_BR2[,-1])
yearly_presence_pararge_BR2_df <- enframe(yearly_presence_pararge_BR2, name = "year", value = "count")
presence_94_2024_pararge_BR2_df <- yearly_presence_pararge_BR2_df[-c(1:3), ]
presence_94_2024_pararge_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

para_chi2$n2 <- presence_94_2024_pararge_BR2_df$count
para_chi2$N2 <- presence_94_2024_pararge_BR2_df$No_of_IT

###BR3
yearly_presence_pararge_BR3 <-colSums(data_Pararge_BR3[,-1])
yearly_presence_pararge_BR3_df <- enframe(yearly_presence_pararge_BR3, name = "year", value = "count")
presence_94_2024_pararge_BR3_df <- yearly_presence_pararge_BR3_df[-c(1:3), ]
presence_94_2024_pararge_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

para_chi2$n3 <- presence_94_2024_pararge_BR3_df$count
para_chi2$N3 <- presence_94_2024_pararge_BR3_df$No_of_IT
###
save(para_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/para_chi2.RData")

####Pyronia cecilia
###########################
data_PyroniaCeci <-
  data %>% filter(sp_latin == "Pyronia cecilia") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaCeci_BR1 <-data_PyroniaCeci[data_PyroniaCeci$IDitin %in% itin_ID_1, ]
###
###
data_PyroniaCeci <-
  data %>% filter(sp_latin == "Pyronia cecilia") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaCeci_BR2 <-data_PyroniaCeci[data_PyroniaCeci$IDitin %in% itin_ID_2, ]
###
###
data_PyroniaCeci <-
  data %>% filter(sp_latin == "Pyronia cecilia") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaCeci_BR3 <-data_PyroniaCeci[data_PyroniaCeci$IDitin %in% itin_ID_3, ]
###
yearly_presence_pyroniaceci_BR1 <-colSums(data_PyroniaCeci_BR1[,-1])
yearly_presence_pyroniaceci_BR1_df <- enframe(yearly_presence_pyroniaceci_BR1, name = "year", value = "count")
presence_94_2024_pyroniaceci_BR1_df <- yearly_presence_pyroniaceci_BR1_df[-c(1:3), ]
presence_94_2024_pyroniaceci_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de pyronia cecilia 
pyroceci_chi2 <-data.frame()
pyroceci_chi2 <- presence_94_2024_pyroniaceci_BR1_df
names(pyroceci_chi2)[names(pyroceci_chi2) == "count"] <- "n1"
names(pyroceci_chi2)[names(pyroceci_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_pyroniaceci_BR2 <-colSums(data_PyroniaCeci_BR2[,-1])
yearly_presence_pyroniaceci_BR2_df <- enframe(yearly_presence_pyroniaceci_BR2, name = "year", value = "count")
presence_94_2024_pyroniaceci_BR2_df <- yearly_presence_pyroniaceci_BR2_df[-c(1:3), ]
presence_94_2024_pyroniaceci_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

pyroceci_chi2$n2 <- presence_94_2024_pyroniaceci_BR2_df$count
pyroceci_chi2$N2 <- presence_94_2024_pyroniaceci_BR2_df$No_of_IT

###BR3
yearly_presence_pyroniaceci_BR3 <-colSums(data_PyroniaCeci_BR3[,-1])
yearly_presence_pyroniaceci_BR3_df <- enframe(yearly_presence_pyroniaceci_BR3, name = "year", value = "count")
presence_94_2024_pyroniaceci_BR3_df <- yearly_presence_pyroniaceci_BR3_df[-c(1:3), ]
presence_94_2024_pyroniaceci_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

pyroceci_chi2$n3 <- presence_94_2024_pyroniaceci_BR3_df$count
pyroceci_chi2$N3 <- presence_94_2024_pyroniaceci_BR3_df$No_of_IT

save(pyroceci_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/pyroceci_chi2.RData")

####Pyronia bathseba
###########################
data_PyroniaBath <-
  data %>% filter(sp_latin == "Pyronia bathseba") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaBath_BR1 <-data_PyroniaBath[data_PyroniaBath$IDitin %in% itin_ID_1, ]
###

###
data_PyroniaBath <-
  data %>% filter(sp_latin == "Pyronia bathseba") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaBath_BR2 <-data_PyroniaBath[data_PyroniaBath$IDitin %in% itin_ID_2, ]
###
###
data_PyroniaBath <-
  data %>% filter(sp_latin == "Pyronia bathseba") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_PyroniaBath_BR3 <-data_PyroniaBath[data_PyroniaBath$IDitin %in% itin_ID_3, ]
###
###BR1
yearly_presence_pyroniabath_BR1 <-colSums(data_PyroniaBath_BR1[,-1])
yearly_presence_pyroniabath_BR1_df <- enframe(yearly_presence_pyroniabath_BR1, name = "year", value = "count")
presence_94_2024_pyroniabath_BR1_df <- yearly_presence_pyroniabath_BR1_df
presence_94_2024_pyroniabath_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de pyronia bathseba 
pyrobath_chi2 <-data.frame()
pyrobath_chi2 <- presence_94_2024_pyroniabath_BR1_df
names(pyrobath_chi2)[names(pyrobath_chi2) == "count"] <- "n1"
names(pyrobath_chi2)[names(pyrobath_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_pyroniabath_BR2 <-colSums(data_PyroniaBath_BR2[,-1])
yearly_presence_pyroniabath_BR2_df <- enframe(yearly_presence_pyroniabath_BR2, name = "year", value = "count")
presence_94_2024_pyroniabath_BR2_df <- yearly_presence_pyroniabath_BR2_df
presence_94_2024_pyroniabath_BR2_df$No_of_IT <- yearly_df$count

pyrobath_chi2$n2 <- presence_94_2024_pyroniabath_BR2_df$count
pyrobath_chi2$N2 <- presence_94_2024_pyroniabath_BR2_df$No_of_IT
###

###BR3
yearly_presence_pyroniabath_BR3 <-colSums(data_PyroniaBath_BR3[,-1])
yearly_presence_pyroniabath_BR3_df <- enframe(yearly_presence_pyroniabath_BR3, name = "year", value = "count")
presence_94_2024_pyroniabath_BR3_df <- yearly_presence_pyroniabath_BR3_df
presence_94_2024_pyroniabath_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

pyrobath_chi2$n3 <- presence_94_2024_pyroniabath_BR3_df$count
pyrobath_chi2$N3 <- presence_94_2024_pyroniabath_BR3_df$No_of_IT
###
save(pyrobath_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/pyrobath_chi2.RData")

####Anthocharis euphenoides
##########################

data_Anthocharis <-
  data %>% filter(sp_latin == "Anthocharis euphenoides") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Anthocharis_BR1 <-data_Anthocharis[data_Anthocharis$IDitin %in% itin_ID_1, ]
###
###
data_Anthocharis <-
  data %>% filter(sp_latin == "Anthocharis euphenoides") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Anthocharis_BR2 <-data_Anthocharis[data_Anthocharis$IDitin %in% itin_ID_2, ]
###
###
data_Anthocharis <-
  data %>% filter(sp_latin == "Anthocharis euphenoides") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Anthocharis_BR3 <-data_Anthocharis[data_Anthocharis$IDitin %in% itin_ID_3, ]
###
yearly_presence_anthocharis_BR1 <-colSums(data_Anthocharis_BR1[,-1])
yearly_presence_anthocharis_BR1_df <- enframe(yearly_presence_anthocharis_BR1, name = "year", value = "count")
presence_94_2024_anthocharis_BR1_df <- yearly_presence_anthocharis_BR1_df
presence_94_2024_anthocharis_BR1_df$No_of_IT <- yearly_counts_BR1_df$count

#creamos df de anthocharis
antho_chi2 <-data.frame()
antho_chi2 <- presence_94_2024_anthocharis_BR1_df
names(antho_chi2)[names(antho_chi2) == "count"] <- "n1"
names(antho_chi2)[names(antho_chi2) == "No_of_IT"] <- "N1" 

###BR2
yearly_presence_anthocharis_BR2 <-colSums(data_Anthocharis_BR2[,-1])
yearly_presence_anthocharis_BR2_df <- enframe(yearly_presence_anthocharis_BR2, name = "year", value = "count")
presence_94_2024_anthocharis_BR2_df <- yearly_presence_anthocharis_BR2_df
presence_94_2024_anthocharis_BR2_df$No_of_IT <- yearly_counts_BR2_df$count

antho_chi2$n2 <- presence_94_2024_anthocharis_BR2_df$count
antho_chi2$N2 <- presence_94_2024_anthocharis_BR2_df$No_of_IT

###BR3
yearly_presence_anthocharis_BR3 <-colSums(data_Anthocharis_BR3[,-1])
yearly_presence_anthocharis_BR3_df <- enframe(yearly_presence_anthocharis_BR3, name = "year", value = "count")
presence_94_2024_anthocharis_BR3_df <- yearly_presence_anthocharis_BR3_df
presence_94_2024_anthocharis_BR3_df$No_of_IT <- yearly_counts_BR3_df$count

antho_chi2$n3 <- presence_94_2024_anthocharis_BR3_df$count
antho_chi2$N3 <- presence_94_2024_anthocharis_BR3_df$No_of_IT
###
save(antho_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/antho_chi2.RData")

list_chi2 <- list(cela = cela_chi2, lyca = lyca_chi2, plebe = plebe_chi2, 
                  pseudo = pseudo_chi2, cyani =  cyani_chi2, vane = vane_chi2, 
                  agla = agla_chi2, antho = antho_chi2, mela = mela_chi2, 
                  para = para_chi2, pyrobath = pyrobath_chi2, pyroceci = pyroceci_chi2) 

save(list_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")


#####################################
#####################################
BR1 <- data_frame(
  species=colext_Results_df_BR1$species,
  C = colext_Results_df_BR1$C_BR1,
  E = colext_Results_df_BR1$E_BR1)

BR2 <- data_frame(
  species=colext_Results_df_BR2$species,
  C = colext_Results_df_BR2$C_BR2,
  E = colext_Results_df_BR2$E_BR2)

BR3 <- data_frame(
  species=colext_Results_df_BR3$species,
  C = colext_Results_df_BR3$C_BR3,
  E = colext_Results_df_BR3$E_BR3)

list_colext_regionsbioclima <- list(BR1 = BR1, BR2 = BR2, BR3 = BR3)

save(list_colext_regionsbioclima, 
     file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# Aglais io in Mediterranea Humida (2)

j <- 1 # Lycaena virgaureae

data_Sp <- 
  data %>% filter(sp_latin == "Lycaena virgaureae") %>% group_by(Any, IDitin) %>% count() %>%
  pivot_wider(names_from = Any, values_from = n) %>% ungroup() %>%
  mutate(across(!IDitin, negate(is.na))) %>%
  mutate(across(!IDitin, as.numeric))

data_Sp_BioReg <-data_Sp[data_Sp$IDitin %in% itin_ID_List[[j]], ]

# Define the full set of years you want
all_years <- as.character(1994:2024)
# Find which years are missing in your data frame
missing <- setdiff(all_years, names(data_Sp_BioReg))
# Add each missing year as a column filled with 0
data_Sp_BioReg[missing] <- 0
# Optionally sort columns chronologically
data_Sp_BioReg <- data_Sp_BioReg[, c("IDitin", all_years)]

# data_Sp_BioReg_94 <- data_Sp_BioReg[,-c(2:4)]
year_cols <- grep("^[0-9]{4}$", colnames(data_Sp_BioReg))  # columns that are 4-digit years
keep_cols <- year_cols[as.numeric(colnames(data_Sp_BioReg)[year_cols]) >= 1994]
keep_cols <- c(1,keep_cols)

data_Sp_BioReg_94 <- data_Sp_BioReg[, keep_cols, drop = FALSE]

data_Sp_BioReg_94_DEF <- data_Sp_BioReg_94

itin_ID_BioReg_Sp <- data_Sp_BioReg_94$IDitin 

Itinerary_Matrix_BioReg <- Itinerary_List[[j]]

yearly_counts_BioReg_df <- yearly_counts_BioReg_List[[j]]

n_BR = n_List[[j]]

N_BR = N_List[[j]]

M_BR = M_List[[j]]

Intinerary_Matrix_Sp_BioReg <-Itinerary_Matrix_BioReg[Itinerary_Matrix_BioReg$SITE_ID %in% itin_ID_BioReg_Sp, ]

# Ordenem els dos data frames segons ID del itinerari (1a columna). 
data_Sp_BioReg_94_DEF <- data_Sp_BioReg_94_DEF[
  order(data_Sp_BioReg_94_DEF[[1]]),]

Intinerary_Matrix_Sp_BioReg <- Intinerary_Matrix_Sp_BioReg[
  order(Intinerary_Matrix_Sp_BioReg[[1]]),]

data_Sp_BioReg_94_DEF[Intinerary_Matrix_Sp_BioReg == 0] <- 2

# Eliminim la primera columna
years <- colnames(data_Celastrina_BR1_94_DEF)[-1]
data_Sp_BioReg_94_EF <-data_Sp_BioReg_94_DEF[,-1]

metapo <- sapply(seq_along(years), function(i) { # 
  
  subset <- data_Sp_BioReg_94_EF[, 1:i, drop = FALSE]
  # 1) Ha tenido al menos un 1 en algún momento hasta el año i
  has_presence_before <- apply(subset == 1, 1, any)
  # 2) En el año i NO tiene un 2 (es decir, fue muestreado)
  not_unsampled_this_year <- subset[, i] != 2
  # Itinerarios que cumplen ambas
  sum(has_presence_before & not_unsampled_this_year)
})

yearly_presence_Sp_BioReg <- colSums(data_Sp_BioReg[,-1])

yearly_presence_Sp_BioReg_df <- enframe(yearly_presence_Sp_BioReg, name = "year", value = "count")

presence_94_2024_Sp_BioReg_df <- yearly_presence_Sp_BioReg_df

presence_94_2024_Sp_BioReg_df$year <- as.numeric(presence_94_2024_Sp_BioReg_df$year)

presence_94_2024_Sp_BioReg_df <- presence_94_2024_Sp_BioReg_df[
  presence_94_2024_Sp_BioReg_df$year >= 1994, ]

#prepararmos el dataframe para el calculo de las ocupancias#
presence_94_2024_Sp_BioReg_df$No_of_IT <- yearly_counts_BioReg_df$count

sp_chi2 <- presence_94_2024_Sp_BioReg_df
#Agregamos n i N (de la BR corresponent) al dataframe de l'especie
sp_chi2$count <- presence_94_2024_Sp_BioReg_df$count
names(sp_chi2)[names(sp_chi2) == "count"] <- n_BR 

sp_chi2$No_of_IT <- presence_94_2024_Sp_BioReg_df$No_of_IT
names(sp_chi2)[names(sp_chi2) == "No_of_IT"] <- N_BR

sp_chi2$M <- metapo
names(sp_chi2)[names(sp_chi2) == "M"] <- M_BR

# Define the full set of years you want
all_years <- as.character(1994:2024)

# Find which years are missing in your data frame
missing <- setdiff(all_years, names(data_Sp_BioReg))

# Add each missing year as a column filled with 0
data_Sp_BioReg[missing] <- 0

# Optionally sort columns chronologically
data_Sp_BioReg <- data_Sp_BioReg[, c("IDitin", all_years)]

list_chi2 <- list(cela = cela_chi2, lyca = lyca_chi2, plebe = plebe_chi2, 
                  pseudo = pseudo_chi2, cyani =  cyani_chi2, vane = vane_chi2, 
                  agla = agla_chi2, antho = antho_chi2, mela = mela_chi2, 
                  para = para_chi2, pyrobath = pyrobath_chi2, pyroceci = pyroceci_chi2) 

save(list_chi2, file="/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")
