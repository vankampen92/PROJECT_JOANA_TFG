library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_sp_BR_Occ_012.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

Sp = c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 
BioReg = c("Regio Alpina i Subalpina", 
           "Regio Mediterranea humida", 
           "Regio Mediterranea arida")

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

count_extinctions_row <- function(row, n = 3) {
  v <- as.numeric(row)
  
  r <- rle(v == 0)   # TRUE only for zeros
  sum(r$values & (r$lengths >= n))
}

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
  
  ocupancia_012$local_extinctions <- apply(ocupancia_012, 1, count_extinction_pattern, n = T_n)
  
  N <- sum(ocupancia_012$local_extinctions)
  
  return(N) #Numero d'extincions observades
}


############################## Provatina: Celastrina a BR1#####################################
data_ocupancia <- data_Celastrina_BR1_94_DEF

c   = 0.58
e   = 0.40
T = 1 / (c + e)
T_n = round(4 * T)

N1 <- Local_Extinction_Counts(data_ocupancia, T_n)
N2 <- Local_Extinction_Pattern(data_ocupancia, T_n)

data_ocupancia$local_extinctions <- apply(data_ocupancia, 1, count_extinction_pattern, n = T_n)
###############################################################################################

# Compta el numero de vegades que s'ha observat un exintion pattern del tipus (1 0 0 ... 0 0) 
# en el conjunt d'itineraris que composa la metapoblacio d'una especie en una bioregio
n_Extincions <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions <- as.data.frame(n_Extincions)

n_Extincions_per_IT <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions_per_IT <- as.data.frame(n_Extincions_per_IT)

for (i in 1:12 ) {
  
  data_nNM <- as.data.frame(my_list[[i]])
  
  for (j in 1:3) {
    # Dades ocupancia species i en cada regions.
  
    print(paste("Calculating Extinctions for Species", Sp[i], "in", BioReg[j]))
    # Pause until user presses Enter
    readline(prompt = "Press [Enter] to continue...")
      
    FES <- 1 # Only if "FES" is changed to 0, the plot is not done!!!
    
    # Controlar especies que no hi son presents en alguna regio bioclimatic
    if (j == 3 && i == 2) { # Lyca (Sp i=2) no hi es present en regio j=3 
      print(paste("No data for species", Sp[i], "in:", BioReg[j], 
                  "No occupancy plot possible!!!"))
      FES = 0
    }
    if (j == 2 && i == 2) { # Lyca (Sp i=2) no hi es present en regio j=2 
      print(paste("No data for species", Sp[i], "in:", BioReg[j], 
                  "No occupancy plot possible!!!"))
      FES = 0
    }
    if (j == 3 && i == 5) { # Cyani (Sp i=5) no hi es present en regio j=3  
      print(paste("No data for species", Sp[i], "in", BioReg[j], 
                  "No occupancy plot possible!!!"))  
      FES = 0
    }
    
    if ( FES == 1 ) {
  
      data_ocupancia <- as.data.frame(sp_BR_Occ_012[[i]][[j]])
    
      c = list_colext_regionsbioclima[[j]]$C[i]
      e = list_colext_regionsbioclima[[j]]$E[i]
    
      T = 1 / (c + e)       # Temps caracteristic
      T_n = ceiling(4 * T)
      print(paste("Temps caracteristic:", T))
      print(paste("Patro d'extincio (1 0 0 ... 0) T_n =", T_n))

      if(j == 1) n_Extincions_per_IT$T_1[i] = T
      if(j == 3) n_Extincions_per_IT$T_2[i] = T
      if(j == 3) n_Extincions_per_IT$T_3[i] = T
      
      # M Nombre d'itineraris que defineixen l'Sp i en BR j
      MM = 4 + (j-1)*3  # Numero de columna on hi ha M1, M2, o M3: metapoblacio potencial)
      M = data_nNM[31, MM]
      if (M > 5) {
        n_Extincions[i,j] <- Local_Extinction_Pattern(data_ocupancia, T_n) 
        n_Extincions_per_IT[i,j] <- n_Extincions[i,j] / M 
      }
    }
  }
}
    