ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

install.packages("readxl")
library(readxl)
library(dplyr)

library(ggplot2)
library(tidyr)

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
  
  

#######################################################
# Pseudocodigo test bondad ajuste

c = colext_Results_df$C[9]
e = colext_Results_df$E[9]
NLL = colext_Results_df[9]$NLL
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[3]]
p_val_9 <- test_simulation_ce (list_multiple[[9]], list_vectores_total[[9]], c, e, 100, NLL)

c = colext_Results_df[10]$C
e = colext_Results_df[10]$E
NLL = colext_Results_df[10]$NLL
# list_cols: Determinar les listes de les posicions que contenen P/A a partir de list_multiple[[5]]
p_val_10 <- test_simulation_ce (list_multiple[[10]], list_vectores_total, c, e, 100, NLL)






test_simulacio_ce <- function(list_itin, #Lista con los itinerarios de una especie
                              list_vectores_total, #Lista con las columnas que contienen P/A.
                              c, e, 
                              nsims, #Numero simulaciones
                              NLL # NLL de las observaciones
){# 0. busquem el valor de la segona columna que conte la P/A de l'sp per cada itinerari
   second_column_values <- sapply(list_itin, function(df) df[1, 2])
  # 1. Definir vector de NLLs
  nlls <- c()
  # 2. Para cada simulación:
  for(n in 1:nsims){
    # i. Definir lista para meter datos simulados
    new_data <- list()
    # ii. Para cada itinerario, simular datos:
    for (i in 1:length(list_itin){
      # a. Obtener años del esquema de muestreo.
      years <- colnames(list_itin[[i]])[list_vectores_total[i]]
      # b. Obtener probabilidades de transición
      dt <- lag(years)
      tps <- cetotrans(c, e, dt)
      # c. Simulación
      itin_sim <- PA_simulation(x = second_column_values[i], #datos itinerario 
                                2, #columna inicial
                                tps #probabilidades de transicion 
      ) #COMPROBAR QUE NO FALTA NINGUN ARG
      
      # d. Nombrar las columnas de la simulación tal y como estaban en el original
      colnames(itin_sim) <- years
      
      # e. Salvar nuevos datos en lista
      new_data[[i]] <- itin_sim #COMPROBAR QUE LA COLUMNA INICIAL SE HA INCLUIDO
    }
    # iii. Calcular la nueva lista con las columnas que tienen P/A, ya que ha cambiado
    
    ???
      
      # iv. Estimar col-ext con irregular_multiple
      
      col_ext_res_sim <- irregular_multiple_datasets()
      
      # v. Salvar la NLL en nlls
      nlls <- c(nlls, col_ext_res_sim$NLL)
  } 
  
  # 3. Calcular el p-valor de las observaciones
  Fn <- ecdf(nlls) #Calcula la e.c.d.f.
  
  Fn(NLL) 
}

