# Chi2 automatic... 

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

p_values_df <- data.frame(matrix(nrow = 12, ncol = length(1994:2024)))
colnames(p_values_df) <- as.character(1994:2024)
rownames(p_values_df) <- c("cela", "lyca", "plebe", 
                           "pseudo", "cyani", "vane", 
                           "agla", "antho", "mela", 
                           "para", "pyrobath", "pyroceci")
for (i in 1:12) {
  df <- list_chi2[[i]]
  for (j in 1:31) {
    # Example values
  
    N1 <- df[j,]$N1; N2 <- df[j,]$N2; N3 <- df[j,]$N3   # total itineraries per zone
    n1 <- df[j,]$n1; n2 <- df[j,]$n2; n3 <- df[j,]$n3   # presences per zone
    
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
    
   #  if( res$p.value <= 0.05) {
   #    if( min_position == 3 ) p_values_df[i,j] <- 2.0
   #    if( min_position == 2 ) p_values_df[i,j] <- -1.0
   #    if( min_position == 1 ) p_values_df[i,j] <- 1.0
   #  }
   #  else
   #  p_values_df[i,j] <- 3.0
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

# vane (no preferencia):
p_values_df[6,1] <- 3.0
p_values_df[6,2] <- 3.0
p_values_df[6,3] <- 3.0
p_values_df[6, 2003 - 1994 + 1] <- 3.0
p_values_df[6, 2004 - 1994 + 1] <- 3.0
p_values_df[6, 2009 - 1994 + 1] <- 3.0

# para (no preferencia):
p_values_df[10,1] <- 3.0
p_values_df[10,2] <- 3.0
p_values_df[10,3] <- 3.0

# lyca: Corregir!!! 


# Example values
N1 <- 1; N2 <- 7; N3 <- 3   # total itineraries per zone
n1 <- 1; n2 <- 7; n3 <- 3   # presences per zone

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