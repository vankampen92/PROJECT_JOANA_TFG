# Chi2 automatic... 

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

p_values_df <- data.frame(matrix(nrow = 12, ncol = length(1994:2024)))
preferencia_df <- data.frame(matrix(nrow = 12, ncol = length(1994:2024)))

colnames(p_values_df) <- as.character(1994:2024)
rownames(p_values_df) <- c("cela", "lyca", "plebe", 
                           "pseudo", "cyani", "vane", 
                           "agla", "antho", "mela", 
                           "para", "pyrobath", "pyroceci")

colnames(preferencia_df) <- as.character(1994:2024)
rownames(preferencia_df) <- c("cela", "lyca", "plebe", 
                           "pseudo", "cyani", "vane", 
                           "agla", "antho", "mela", 
                           "para", "pyrobath", "pyroceci")

for (i in 1:12) {
  df <- as.data.frame(list_chi2[[i]])
  
  for (j in 1:31) {
    FES = 1 
    # Example values
  
    N1 <- df$N1[j]; N2 <- df$N2[j]; N3 <- df$N3[j]   # total itineraries per zone
    n1 <- df$n1[j]; n2 <- df$n2[j]; n3 <- df$n3[j]   # presences per zone
    
    if ( n1 > 0 && n2 == 0 && n3 == 0 ) FES = 0
    if ( n1 == 0 && n2 > 0 && n3 == 0 ) FES = 0
    if ( n1 == 0 && n2 == 0 && n3 > 0 ) FES = 0
    
    if (FES == 1) {
    
      min_position <- which.min(c(n1/N1, n2/N2, n3/N3))
     
      # Build the contingency table
      chi2_presence_matrix <- matrix(
        c(n1, n2, n3, N1 - n1, N2 - n2, N3 - n3),
        nrow = 2,
        byrow = TRUE
      )
    
    # colnames(chi2_presence_matrix) <- c("ZAL", "ZMH", "ZMA")
    # rownames(chi2_presence_matrix) <- c("Present", "Absent")
    
      chi2_presence_matrix
    
      # Perform Chi-square test of independence
      res <- chisq.test(chi2_presence_matrix)
    
      p_values_df[i,j] <- res$p.value
    
      if( !is.na(res$p.value) && res$p.value < 0.05) {
      if( min_position == 3 ) preferencia_df[i,j] <-  1.0
      if( min_position == 2 ) preferencia_df[i,j] <- -1.0
      if( min_position == 1 ) preferencia_df[i,j] <-  2.0
      }
      else {
        if ( is.na(res$p.value) ) {
          preferencia_df[i,j] <- 0.0
        }
        else {
          preferencia_df[i,j] <- 3.0
        }
      }
    }
  }
}

# Corregir lyca, vane (sp 6), and para:
# vane:
p_values_df[6,1] <- 1.0
p_values_df[6,2] <- 1.0
p_values_df[6,3] <- 1.0
p_values_df[6, 2003 - 1994 + 1] <- 1.0
p_values_df[6, 2004 - 1994 + 1] <- 1.0
p_values_df[6, 2009 - 1994 + 1] <- 1.0
# para:
p_values_df[10,1] <- 1.0
p_values_df[10,2] <- 1.0
p_values_df[10,3] <- 1.0
# lyca: Corregida 
p_values_df[2,] <- 0.0

# vane (no preferencia):
preferencia_df[6,1] <- 3.0
preferencia_df[6,2] <- 3.0
preferencia_df[6,3] <- 3.0
preferencia_df[6, 2003 - 1994 + 1] <- 3.0
preferencia_df[6, 2004 - 1994 + 1] <- 3.0
preferencia_df[6, 2009 - 1994 + 1] <- 3.0

# para (no preferencia):
preferencia_df[10,1]  <- 3.0
preferencia_df[10,2] <- 3.0
preferencia_df[10,3] <- 3.0

# lyca: Corregida 
preferencia_df[2,] <- 1.0


# Example values
N1 <- 5; N2 <- 7; N3 <- 5   # total itineraries per zone
n1 <- 1; n2 <- 0; n3 <- 0   # presences per zone

# Build the contingency table
presence_matrix <- matrix(
  c(n1, n2, n3, N1 - n1, N2 - n2, N3 - n3),
  nrow = 2,
  byrow = TRUE
)

colnames(presence_matrix) <- c("Zone1", "Zone2", "Zone3")
rownames(presence_matrix) <- c("Present", "Absent")

presence_matrix

# Perform Chi-square test of independence
chisq.test(presence_matrix)