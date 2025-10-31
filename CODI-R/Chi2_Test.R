#
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

for (i in 1:12) {
  df <- list_chi2[[i]]
  for (j in 1:31) {
    # Example values
  
    N1 <- df[j,]$N1; N2 <- df[j,]$N2; N3 <- df[j,]$N3   # total itineraries per zone
    n1 <- df[j,]$n1; n2 <- df[j,]$n2; n3 <- df[j,]$n3   # presences per zone
    
    # Build the contingency table
    presence_matrix <- matrix(
      c(n1, n2, n3, N1 - n1, N2 - n2, N3 - n3),
      nrow = 2,
      byrow = TRUE
    )
    
    colnames(presence_matrix) <- c("ZAL", "ZMH", "ZMA")
    rownames(presence_matrix) <- c("Present", "Absent")
    
    presence_matrix
    
    # Perform Chi-square test of independence
    chisq.test(presence_matrix)
  }
}

# Example values
N1 <- 30; N2 <- 40; N3 <- 50   # total itineraries per zone
n1 <- 10; n2 <- 15; n3 <- 25   # presences per zone

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