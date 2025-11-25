library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_Occu_Sp_BioReg.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

count_extinction_pattern <- function(row, n = 3) {
  v <- as.numeric(row)
  
  count <- 0
  run <- 0
  
  for (i in seq_along(v)) {
    if (v[i] == 0) {
      run <- run + 1
    } else {
      # if run ends here, check if it qualifies
      if (run >= n) {
        # check if preceded by a 1
        if (i - run - 1 >= 1 && v[i - run - 1] == 1) {
          count <- count + 1
        }
      }
      run <- 0
    }
  }
  
  # check if a run ends at the last element
  if (run >= n) {
    if (length(v) - run >= 1 && v[length(v) - run] == 1) {
      count <- count + 1
    }
  }
  
  count
}

Local_Extinction_Pattern <- function(ocupancia_012, T_n)
{
  # Input:
  #      . ocupancia_012 es la matriu ocupancies (2 no mostrejat, 1 presencia, 0 absencia):  
  #       It  1994  ...  ...  ... ... 2024
  #       12   2     2    1    0   1   0
  #      . T_n:  patro de 0 seguits a comptabilitzar
  #  
  ocupancia_012$local_extinctions <- apply(ocupancia_012, 1, count_extinction_pattern, n = T_n)
  
  N <- sum(ocupancia_012$local_extinctions)
  
  return(N) #Numero d'extincions observades
}


count_extinctions_row <- function(row, n = 3) {
  v <- as.numeric(row)
  
  r <- rle(v == 0)   # TRUE only for zeros
  sum(r$values & (r$lengths >= n))
}

Local_Extinction_Counts <- function(ocupancia_012, T_n)
{
# Input:
#      . ocupancia_012 es la matriu ocupancies (2 no mostrejat, 1 presencia, 0 absencia):  
#       It  1994  ...  ...  ... ... 2024
#       12   2     2    1    0   1   0
#      . T_n:  patro de 0 seguits a comptabilitzar
#  
ocupancia_012$local_extinctions <- apply(ocupancia_012, 1, count_extinctions_row, n = T_n)

N <- sum(ocupancia_012$local_extinctions)

return(N) #Numero d'extincions observades
}

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

Sp = c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 
BioReg = c("Regio Alpina i Subalpina", 
           "Regio Mediterranea humida", 
           "Regio Mediterranea arida")

# Prova: Celastrina a BR1
data_ocupancia <- data_Celastrina_BR1_94_DEF

c   = 0.58
e   = 0.40
T = 1 / (c + e)
T_n = round(4 * T)

N1 <- Local_Extinction_Counts(data_ocupancia, T_n)
N2 <- Local_Extinction_Pattern(data_ocupancia, T_n)

data_ocupancia$local_extinctions <- apply(data_ocupancia, 1, count_extinction_pattern, n = T_n)

n_Extincions <- data.frame()
row.names(n_Extincions) <- Sp
col.names(n_Extincions) <- BioReg

for (i in 1:12 ) {
  for (j in 1:3) {
    # Dades ocupancia species i en cada regions.
    data_ocupancia <- as.data.frame(list_Occu_Sp_BioReg[[i]][[j]])
    
    c = list_colext_regionsbioclima[[j]]$C[i]
    e = list_colext_regionsbioclima[[j]]$E[i]
    
    T = 1 / (c + e)
    T_n = round(4 * T)
    
    n_Extincions[i,j] <- Local_Extinction_Counts(data_ocupancia, T_n)
  }
}
    