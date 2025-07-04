ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

install.packages("readxl")
library(readxl)
library(dplyr)

library(ggplot2)
library(tidyr)

library(island)

# Test Colonitzacion Extincio
source("~/PROJECT_JOANA_TFG/CODI-R/Funcio_test_simulacio_ce.R")
# Test d'equilibri
source("~/PROJECT_JOANA_TFG/CODI-R/Funcio_test_simulacio_eq.R")
 
# Cal carregar la llista de tots els itineraris de les 12 species i 
# el data frame de resultats colext_Results_df. 
load(file = "~/PROJECT_JOANA_TFG/DADES/list_multiple.RData") 
load(file = "~/PROJECT_JOANA_TFG/DADES/colext_Results_df.RData")

#############################################
#Calcul c i e simulades

list_vectores_total <- list()
for(i in 1:length(list_multiple)){
  ncols <- sapply(list_multiple[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length( list_multiple[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  list_vectores_total[[i]] <- list_vectors
}
  
####################### Exemple species 9 ######################################
list_itin <- list_multiple[[9]]
list_vectors <- list_vectores_total[[9]]
nsims <- 100
second_column_values <- sapply(list_itin, function(df) df[1, 2])

new_data <- list()
    
i <- 36    # Exemple Iditin = 36 of Melanargia occitanica

c = colext_Results_df$C[9]
e = colext_Results_df$E[9]
NLL = colext_Results_df$NLL[9]    

second_column_values <- sapply(list_itin, function(df) df[1, 2])        
nlls <- c()
for(n in 1:500){
  for (i in 1:length(list_itin)) {
      years         <- colnames(list_itin[[i]])[2:(ncol(list_itin[[i]]) - 1)]
      years_numeric <- as.numeric(years)
      dt            <- diff(years_numeric)
      tps           <- cetotrans(c, e, dt)
      times         <- length(dt)
    
      # Creacio del data frame a simular (input de PA_simulation)
      itin_sim_df        <- data.frame(sp_latin = list_itin[[i]]$sp_latin)
      itin_sim_df[years] <- c(1:length(years))
      itin_sim_df[1,2]   <- second_column_values[i]
      itin_sim_df$IDitin <- list_itin[[i]]$IDitin
    
      itin_sim <- PA_simulation(itin_sim_df, #datos itinerario 
                                2,        #columna inicial
                                tps,      #probabilidades de transicion
                                times )   #number of transitions to simulate
  
      Final_Time_Column <- length(years) + 1   
      itin_sim_df[1, 3:Final_Time_Column] <- itin_sim[1, 1:times]
    
      new_data[[i]] <- itin_sim_df
    }
    
    # iv. Estimar col-ext con irregular_multiple
    col_ext_res_sim <- irregular_multiple_datasets(new_data, list_vectors, 
                                                   0.0001, 0.0001, CI = TRUE)
    # v. Salvar la NLL en nlls
    nlls <- c(nlls, col_ext_res_sim$NLL)  
}

# 3. Calcular el p-valor de las observaciones
Fn <- ecdf(nlls) #Calcula la e.c.d.f.

pValor_9 <- Fn(NLL)     
##########################################################################################################
# L'especie 9: Melanargia occitanica:
c = colext_Results_df$C[9]
e = colext_Results_df$E[9]
NLL = colext_Results_df$NLL[9]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[3]]
p_val_9 <- test_simulacio_ce (list_multiple[[9]], list_vectores_total[[9]], 
                               c, e, 500, NLL)
##########################################################################################################
# L'especie 10: Pararge aegeria: 
c = colext_Results_df$C[10]
e = colext_Results_df$E[10]
NLL = colext_Results_df$NLL[10]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
p_val_10 <- test_simulacio_ce2 (list_multiple[[10]], list_vectores_total[[10]], 
                                c, e, 500, NLL)

# Totes les especies en tots els itineraris
list_vectores_total <- list()
for(i in 1:length(list_multiple)){
  ncols <- sapply(list_multiple[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length( list_multiple[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  list_vectores_total[[i]] <- list_vectors
}

p_val_sp <- vector()
for(i in 1:12){
  print(colext_Results_df$species[i])
  
  list_itin    <- list_multiple[[i]]
  list_vectors <- list_vectores_total[[i]]
  c = colext_Results_df$C[i]
  e = colext_Results_df$E[i]
  NLL = colext_Results_df$NLL[i]
  # list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
  p_val_sp[i] <- test_simulacio_ce (list_itin, list_vectors, c, e, 500, NLL)
  
  print(c("Done!!!"))
}

##########################################################################################################
# Calcul del test per totes les especies de BR1 que estan en sp_labels

# BR1
# Cal carregar la llista d'itineraris de la BR1 i el data frame de resultats de BR1
# colext_Results_df_BR1 de cada especie. 

load(file = "~/PROJECT_JOANA_TFG/DADES/filtered_list_BR1.RData") 
load(file = "~/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1.RData")

sp_labels_BR1 = c(1,2,3,4,5,6,7,8,10,11)

list_vectores_total_BR1 <- list()
for(i in sp_labels_BR1){
  ncols <- sapply(filtered_list_BR1[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length(filtered_list_BR1[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  list_vectores_total_BR1[[i]] <- list_vectors
}

p_val_sp_BR1  = vector()
for(i in sp_labels_BR1){
  print(colext_Results_df_BR1$species[i])
  
  list_itin    <- filtered_list_BR1[[i]]
  list_vectors <- list_vectores_total_BR1[[i]]
  c = colext_Results_df_BR1$C_BR1[i]
  e = colext_Results_df_BR1$E_BR1[i]
  NLL = colext_Results_df_BR1$NLL_BR1[i]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
  p_val_sp_BR1[i] <- test_simulacio_ce (list_itin, list_vectors, c, e, 500, NLL)
  
  print(c(i, ": Done!!!"))
}

# BR2
# Cal carregar la llista d'itineraris de la BR2 i el data frame de resultats de BR2
# colext_Results_df_BR2 de cada especie. 

load(file = "~/PROJECT_JOANA_TFG/DADES/filtered_list_BR2.RData") 
load(file = "~/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2.RData")

sp_labels_BR2 = c(1,3,4,6,7,8,9,10,11,12)
list_vectores_total_BR2 <- list()

for(i in sp_labels_BR2){
  ncols <- sapply(filtered_list_BR2[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length(filtered_list_BR2[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  list_vectores_total_BR2[[i]] <- list_vectors
}

p_val_sp_BR2  = vector()
for(i in sp_labels_BR2){
  print(colext_Results_df_BR2$species[i])
  
  list_itin    <- filtered_list_BR2[[i]]
  list_vectors <- list_vectores_total_BR2[[i]]
  c = colext_Results_df_BR2$C_BR2[i]
  e = colext_Results_df_BR2$E_BR2[i]
  NLL = colext_Results_df_BR2$NLL_BR2[i]
  # list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
  p_val_sp_BR2[i] <- test_simulacio_ce (list_itin, list_vectors, c, e, 500, NLL)
  
  print(c("Done!!!"))
}

# BR3
# Cal carregar la llista d'itineraris de la BR3 i el data frame de resultats de BR3
# colext_Results_df_BR3 de cada especie. 
load(file = "~/PROJECT_JOANA_TFG/DADES/filtered_list_BR3.RData") 
load(file = "~/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3.RData")

sp_labels_BR3 = c(1,3,4,6,7,8,9,10,11,12)

list_vectores_total_BR3 <- list()
for(i in sp_labels_BR3){
  ncols <- sapply(filtered_list_BR3[[i]], ncol)  
  list_vectors <- list()
  for(j in 1:length(filtered_list_BR3[[i]] ) ){
    list_vectors[[j]] <- 2:(ncols[j] - 1)
  }
  list_vectores_total_BR3[[i]] <- list_vectors
}

p_val_sp_BR3  = vector()
for(i in sp_labels_BR3){
  print(colext_Results_df_BR3$species[i])
  
  list_itin    <- filtered_list_BR3[[i]]
  list_vectors <- list_vectores_total_BR3[[i]]
  c = colext_Results_df_BR3$C_BR3[i]
  e = colext_Results_df_BR3$E_BR3[i]
  NLL = colext_Results_df_BR3$NLL_BR3[i]
  # list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
  p_val_sp_BR3[i] <- test_simulacio_ce (list_itin, list_vectors, c, e, 500, NLL)
  
  print(c("Done!!!"))
}

########## Exemples Lycaena i Aglais io ########################################
i <- 2 # Lycaena
list_itin    <- filtered_list_BR1[[i]]
list_vectors <- list_vectores_total_BR1[[i]]
c = colext_Results_df_BR1$C_BR1[i]
e = colext_Results_df_BR1$E_BR1[i]
NLL = colext_Results_df_BR1$NLL_BR1[i]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
p_val_sp_BR1[i] <- test_simulacio_ce (list_itin, list_vectors, c, e, 500, NLL)

i <- 7 # Aglais io
list_itin    <- filtered_list_BR1[[i]]
list_vectors <- list_vectores_total_BR1[[i]]
c = colext_Results_df_BR1$C_BR1[i]
e = colext_Results_df_BR1$E_BR1[i]
NLL = colext_Results_df_BR1$NLL_BR1[i]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
p_val_sp_BR1[i] <- test_simulacio_ce (list_itin, list_vectors, c, e, 500, NLL)
################################################################################

### Test d'equilibri ############################################################################################
# L'especie 9: Melanargia occitanica:
c = colext_Results_df$C[9]
e = colext_Results_df$E[9]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[3]]
p_val_9_eq <- test_simulacio_eq (list_multiple[[9]], list_vectores_total[[9]], 
                                 c, e, 500)
# L'especie 10: Pararge aegeria: 
c = colext_Results_df$C[10]
e = colext_Results_df$E[10]
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
p_val_10_eq <- test_simulacio_eq (list_multiple[[10]], list_vectores_total[[10]], 
                                  c, e, 500)
################################################################################
