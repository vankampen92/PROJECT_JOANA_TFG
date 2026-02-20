# This script automatize the plotting of occupancies (with shading)
library(ggplot2)
library(dplyr)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/my_list_chi2.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")

# ---  Crear carpeta donde guardar los gráficos ---
# dir.create("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/grafics_ocupancies", showWarnings = FALSE)

Sp = c("Celastrina Argiolus", "Lycaena Vigaureae", "Plebejus argus", 
       "Psedophilotes panoptes", "Cyaniris semiargus", "Vanessa cardui", 
       "Aglais io", "Anthocharis euphenoides", "Melanargia occitanica", 
       "Pararge aegeria", "Pyronia bathseba", "Pyronia cecilia") 

BioReg = c("Regió Alpina i Subalpina", 
           "Regió Mediterrània humida", 
           "Regió Mediterrània àrida")

ocupancies <- data.frame(
  year = c(1994:2024),
  p_1  = numeric(31),
  p_2  = numeric(31),
  p_3  = numeric(31)
)

load("~/PROJECT_JOANA_TFG/DADES/my_list_chi2.RData")

llista_sp_ocupancies <- list()

results_1 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_2 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

results_3 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)


for (i in 1:12 ) {
  # Dades ocupancia species i en les tres regions. 
  data_ocupancia <- as.data.frame(my_list[[i]])
  
  for (j in 1:3) {
  
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
      
      # Col on hi ha el nombre d'itineraris ocupats a la regio j
      nn = 2 + (j-1)*3  
      # Col on hi ha el nombre d'itineraris mostrejats regio j
      MM = 4 + (j-1)*3  # (M1, M2, o M3: metapoblacio potencial)
      
      ocupancies[,j+1] = data_ocupancia[, nn]/data_ocupancia[, MM]
      
      if( j == 1) {
        model <- lm(p_1 ~ year, data = ocupancies)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_1 <- rbind(results_1,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 2 ) {
        model <- lm(p_2 ~ year, data = ocupancies)  
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_2 <- rbind(results_2,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 3) {
        model <- lm(p_3 ~ year, data = ocupancies)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        results_3 <- rbind(results_3,
                       data.frame(species = Sp[i],
                                  slope = slope,
                                  p_value = pval))
      }
    }
  }
  
  llista_sp_ocupancies[[i]] <- ocupancies
}

results_1$trend <- ifelse(results_1$p_value < 0.05 & results_1$slope > 0,
                        "Significant increase",
                        ifelse(results_1$p_value < 0.05 & results_1$slope < 0,
                               "Significant decrease",
                               "No significant trend"))

results_2$trend <- ifelse(results_2$p_value < 0.05 & results_2$slope > 0,
                          "Significant increase",
                          ifelse(results_2$p_value < 0.05 & results_2$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

results_3$trend <- ifelse(results_3$p_value < 0.05 & results_3$slope > 0,
                          "Significant increase",
                          ifelse(results_3$p_value < 0.05 & results_3$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

# Build reduced dataframe
ocupancia_year_p <- data.frame(
  year = numeric(),
  p    = numeric()
)

results_1 <- data.frame(
  species = character(),
  slope = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)
results_2 <- results_1
results_3 <- results_1

Occupancia_M_Filtering_Function <- function(data_ocupancia, j, M_min = 10)
{
  # Column index for n in region j
  nn <- 2 + (j - 1) * 3
  
  # Column index for M in region j
  MM <- 4 + (j - 1) * 3
  
  # Build reduced dataframe
  ocupancia_M <- data.frame(
    year = data_ocupancia[, 1],
    n    = data_ocupancia[, nn],
    M    = data_ocupancia[, MM]
  )
  
  # Filter rows where M > M_min
  ocupancia_M <- ocupancia_M[ocupancia_M$M > M_min, ]
  
  # Directly compute output
  ocupancia_year_p <- data.frame(
    year = ocupancia_M$year,
    p    = ocupancia_M$n / ocupancia_M$M
  )
  
  return(ocupancia_year_p)
}

for (i in 1:12 ) {
  # Dades ocupancia species i en les tres regions. 
  data_ocupancia <- as.data.frame(my_list[[i]])
  
  for (j in 1:3) {
    
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
      
      # Contrium data_ocupancia_filtrat (M > 10) 
      # function (data_occupancia, M, j) i et retorna una data_ocupancia_M_Filtrada 
      
      ocupancies_year_p <- Occupancia_M_Filtering_Function(data_ocupancia, j, 10)
      
      if (nrow(ocupancies_year_p) >= 5) {
        
        model <- lm(p ~ year, data = ocupancies_year_p)
        slope <- coef(summary(model))["year", "Estimate"]
        pval  <- coef(summary(model))["year", "Pr(>|t|)"]
        
      } else {
        
        slope <- NA
        pval  <- NA
      }
      
      if( j == 1) {
        results_1 <- rbind(results_1,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 2 ) {
        results_2 <- rbind(results_2,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
      if( j == 3) {
        results_3 <- rbind(results_3,
                           data.frame(species = Sp[i],
                                      slope = slope,
                                      p_value = pval))
      }
    }
  }
}

results_1$trend <- ifelse(results_1$p_value < 0.05 & results_1$slope > 0,
                          "Significant increase",
                          ifelse(results_1$p_value < 0.05 & results_1$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

results_2$trend <- ifelse(results_2$p_value < 0.05 & results_2$slope > 0,
                          "Significant increase",
                          ifelse(results_2$p_value < 0.05 & results_2$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))

results_3$trend <- ifelse(results_3$p_value < 0.05 & results_3$slope > 0,
                          "Significant increase",
                          ifelse(results_3$p_value < 0.05 & results_3$slope < 0,
                                 "Significant decrease",
                                 "No significant trend"))



