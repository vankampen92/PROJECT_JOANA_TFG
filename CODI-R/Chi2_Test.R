# Chi2 automatic... 

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

# Funcio R: 
chi2_2p2 <- function(n1, N1, n2, N2) {
  poc <- as.vector(c(n1/N1, n2/N2))
  # Build the contingency table
  presence_matrix <- matrix(
    c(n1, n2, N1 - n1, N2 - n2),
    nrow = 2,
    byrow = TRUE
  )
  
  # Perform Chi-square test of independence
  res <- chisq.test(presence_matrix)
  p_val <- res$p.value
  
  if( is.na(p_val) ) {
    if (poc[1] == poc[2]) p_val <- 1
    else {
      print(c("n1 = ", n1, "N1 =",  N1, "n2 = ", n2, "N2 = ", N2))
      readline(prompt = "p-valor is NA. Fuck!!! Press [Enter] to continue...")
    }
  }
    
  return(p_val)
}

p_values_df    <- data.frame(matrix(nrow = 12, ncol = length(1994:2024)))
preferencia_df <- data.frame(matrix(nrow = 12, ncol = length(1994:2024)))

colnames(p_values_df) <- as.character(1994:2024)
rownames(p_values_df) <- c("Pseudophilotes panoptes", "Cyaniris semiargus",
                           "Plebejus argus",  "Aglais io", "Melanargia occitanica", 
                           "Anthocharis euphenoides", "Vanessa cardui", 
                            "Lycaena virgaureae", "Pararge aegeria",
                           "Celastrina argiolus", "Pyronia bathseba", 
                           "Pyronia cecilia")

colnames(preferencia_df) <- as.character(1994:2024)
rownames(preferencia_df) <- c("Pseudophilotes panoptes", "Cyaniris semiargus",
                              "Plebejus argus",  "Aglais io", "Melanargia occitanica", 
                              "Anthocharis euphenoides", "Vanessa cardui", 
                              "Lycaena virgaureae", "Pararge aegeria",
                              "Celastrina argiolus", "Pyronia bathseba", 
                              "Pyronia cecilia")
for (i in 1:12) {
  df <- as.data.frame(list_chi2[[i]])
  
  for (j in 1:31) {
    FES = 1 
    # Example values
  
    N1 <- df$N1[j]; N2 <- df$N2[j]; N3 <- df$N3[j]   # total itineraries per zone
    n1 <- df$n1[j]; n2 <- df$n2[j]; n3 <- df$n3[j]   # presences per zone
    
    pocu = as.vector(c(n1/N1, n2/N2, n3/N3))
    
    if ( n1 * n2 * n3 == 0 ) {
      FES = 0
      
      # readline(prompt = "Alguna occupancia es zero!!! Press [Enter] to continue...")
      if ( n1 == 0 && n2 == 0 && n3 == 0 ) preferencia_df[i,j] = 0.0
      
      if ( n1 > 0 && n2 == 0 && n3 == 0 ) preferencia_df[i,j] = 1.0
      if ( n1 == 0 && n2 > 0 && n3 == 0 ) preferencia_df[i,j] = 2.0
      if ( n1 == 0 && n2 == 0 && n3 > 0 ) preferencia_df[i,j] = 3.0
      
      if ( n1 > 0 && n2 > 0 && n3 == 0 ) {
        p_val = chi2_2p2 (n1, N1, n2, N2)
        if (p_val < 0.05) {
          if (pocu[2] > pocu[1]) preferencia_df[i,j] = 2.0
          else                   preferencia_df[i,j] = 1.0  
        }
        else {
          preferencia_df[i,j] = 12.0
        }
      }
      if ( n1 == 0 && n2 > 0 && n3 > 0 ) {
        p_val = chi2_2p2 (n2, N2, n3, N3)
        if (p_val < 0.05) {
          if (pocu[3] > pocu[2]) preferencia_df[i,j] = 3.0
          else                   preferencia_df[i,j] = 2.0  
        }
        else {
          preferencia_df[i,j] = 23.0
        }
      }
      if ( n1 > 0 && n2 == 0 && n3 > 0 ) {
        p_val = chi2_2p2 (n1, N1, n3, N3)
        if (p_val < 0.05) {
          if (pocu[3] > pocu[1]) preferencia_df[i,j] = 3.0
          else                   preferencia_df[i,j] = 1.0  
        }
        else {
          preferencia_df[i,j] = 13.0
        }
      }
    }
    
    if (FES == 1) {
      min_position <- which.min(c(n1/N1, n2/N2, n3/N3))
     
      # Build the contingency table
      chi2_presence_matrix <- matrix(
        c(n1, n2, n3, N1 - n1, N2 - n2, N3 - n3),
        nrow = 2,
        byrow = TRUE
      )
      
      # Perform Chi-square test of independence
      res <- chisq.test(chi2_presence_matrix)
      p_values_df[i,j] <- res$p.value

      if( !is.na(res$p.value) && res$p.value < 0.05 ) {
        
        if( min_position == 3 ) {
          p_val = chi2_2p2 (n1, N1, n2, N2)
          if (p_val < 0.05) {
            if (pocu[2] > pocu[1]) preferencia_df[i,j] = 2.0
            else                   preferencia_df[i,j] = 1.0  
          }
          else {
            preferencia_df[i,j] = 12.0
          }
        }
        
        if( min_position == 2 ) {
          p_val <- chi2_2p2 (n1, N1, n3, N3)
          if (p_val < 0.05) {
            if (pocu[3] > pocu[1]) preferencia_df[i,j] = 3.0
            else                   preferencia_df[i,j] = 1.0  
          }
          else {
            preferencia_df[i,j] = 13.0
          }
        }
        
        if( min_position == 1 ) {
          p_val = chi2_2p2 (n2, N2, n3, N3)
          if (p_val < 0.05) {
            if (pocu[3] > pocu[2]) preferencia_df[i,j] = 3.0
            else                   preferencia_df[i,j] = 2.0  
          }
          else {
            preferencia_df[i,j] = 23.0
          } 
        }
      }
      else if (!is.na(res$p.value)) {
        preferencia_df[i,j] <-  123.0 # No Preferencia
      }
      else {
        preferencia_df[i,j] <-  333.0
      }
    }
  }
}

# vane (no preferencia):
preferencia_df[6,1] <- 123.0
preferencia_df[6,2] <- 123.0
preferencia_df[6,3] <- 123.0
preferencia_df[6, 2003 - 1994 + 1] <- 123.0
preferencia_df[6, 2004 - 1994 + 1] <- 123.0
preferencia_df[6, 2009 - 1994 + 1] <- 123.0

# para (no preferencia):
preferencia_df[10,1] <- 123.0
preferencia_df[10,2] <- 123.0
preferencia_df[10,3] <- 123.0

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

# 1  Blau fosc
# 12 Blau clar
# 2  Verd fosc
# 123 Verd clar
# 23 Vermell clar
# 3  Vermell fosc
# 13 Lila oscuro

# Perform Chi-square test of independence
chisq.test(presence_matrix)

# Graphix: chi2 test de les preferencies
library(ggplot2)
library(tidyr)
library(dplyr)

# Read your data (adjust path as needed)
# df <- read.csv("your_file.csv", row.names = 1, check.names = FALSE)

# Preserve species order exactly as in the data
species_order <- rownames(preferencia_df)

# Move rownames into a column
preferencia_df <- preferencia_df %>%
  mutate(Species = factor(rownames(preferencia_df), levels = species_order))

# Reshape from wide to long format
df_long <- preferencia_df %>%
  pivot_longer(
    cols = -Species,
    names_to = "Year",
    values_to = "Label"
  )

# Convert Year to numeric
df_long$Year <- as.numeric(df_long$Year)

# Define your color mapping
label_colors <- c(
  "1"   = "darkblue",
  "12"  = "lightblue",
  "2"   = "darkgreen",
  "123" = "lightgreen",
  "23"  = "lightcoral",
  "3"   = "darkred",
  "13"  = "magenta",
  "0"   = "grey"
)

# Plot
ggplot(df_long, aes(x = Year, y = Species, fill = factor(Label))) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = label_colors, na.value = "white") +
  scale_x_continuous(breaks = seq(1994, 2024, by = 2)) +
  theme_minimal(base_size = 12) +
  labs(x = "Year", y = "Species", fill = "Label") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic"),
    legend.position = "right"
  )

library(ggplot2)
library(tidyr)
library(dplyr)

# Preserve species order as in the data
species_order <- rev(rownames(preferencia_df))

df_long$Species <- factor(df_long$Species, levels = species_order)

# Move rownames into a column and keep order
preferencia_df <- preferencia_df %>%
  mutate(Species = factor(rownames(preferencia_df), levels = species_order))

# Select only year columns (those that are 4-digit numbers)
year_cols <- grep("^\\d{4}$", names(preferencia_df), value = TRUE)

# Reshape data to long format
df_long <- preferencia_df %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Label"
  )

# Convert year names to numeric
df_long$Year <- as.numeric(df_long$Year)

# Define colors for each label
label_colors <- c(
  "1"   = "darkblue",
  "12"  = "lightblue",
  "2"   = "darkgreen",
  "123" = "lightgreen",
  "23"  = "lightcoral",
  "3"   = "darkred",
  "13"  = "magenta",
  "0" = "grey"
)

# Plot
ggp <- ggplot(df_long, aes(x = Year, y = forcats::fct_rev(Species), fill = factor(Label))) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = label_colors, na.value = "white") +
  scale_x_continuous(breaks = seq(1994, 2024, by = 2)) +
  scale_y_discrete(limits = levels(df_long$Species)) +  
  theme_minimal(base_size = 12) +
  labs(x = "Year", y = "", fill = "Region") +
  coord_fixed(ratio = 1.25) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic"),
    legend.position = "right"
  )

ggsave("~/PROJECT_JOANA_TFG/GRAFICS/preferencia_chi2_regio.png", 
       plot = ggp, width = 10, height = 6, dpi = 300)


