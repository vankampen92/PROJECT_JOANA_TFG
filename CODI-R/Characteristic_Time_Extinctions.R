library(ggplot2)
library(dplyr)
library(tidyverse)

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_sp_BR_Occ_012.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_colext_regionsbioclima.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/list_chi2.RData")

###Per carregar el delta de temps caracteristic carrego els df amb els valors per
# cada bioregio

load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1_ordenado.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2_ordenado.RData")
load("/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3_ordenado.RData")


Sp = c( "Pseudophilotes panoptes", "Cyaniris semiargus",   "Plebejus argus",
        "Aglais io", "Melanargia occitanica", "Anthocharis euphenoides", "Vanessa cardui",
        "Lycaena vigaureae","Pararge aegeria","Celastrina argiolus",  
        "Pyronia bathseba", "Pyronia cecilia") 
BioReg = c("Regió Alpina i Subalpina", 
           "Regió Mediterrània Humida", 
           "Regió Mediterrània Àrida")


temps_br1 <- colext_Results_df_BR1_ordenado %>%
  transmute(
    Species = Sp,
    Region = "Regió Alpina i Subalpina",
    valor = Temps_Ca,
    error = Delta_Temps_Ca,
    low = Temps_Ca - Delta_Temps_Ca,
    up = Temps_Ca + Delta_Temps_Ca
  )

temps_br2 <- colext_Results_df_BR2_ordenado %>%
  transmute(
    Species = Sp,
    Region = "Regió Mediterrània Humida",
    valor = Temps_Ca,
    error = Delta_Temps_Ca,
    low = Temps_Ca - Delta_Temps_Ca,
    up = Temps_Ca + Delta_Temps_Ca
  )

temps_br3 <- colext_Results_df_BR3_ordenado %>%
  transmute(
    Species = Sp,
    Region = "Regió Mediterrània Àrida",
    valor = Temps_Ca,
    error = Delta_Temps_Ca,
    low = Temps_Ca - Delta_Temps_Ca,
    up = Temps_Ca + Delta_Temps_Ca
  )

df_temps <- bind_rows(temps_br1, temps_br2, temps_br3)
#Mantenim l'ordre de les especies
df_temps$Species <- factor(df_temps$Species, levels = species_order)

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
  
  count_end <- 0  # extinction that reaches the end
  count     <- 0  # extinction followed by recolonization
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
      count_end <- count_end + 1
    }
  }
  
  return(c(count = count, count_end = count_end))
  
  return(Extinction_Counts)
}

Local_Extinction_Pattern <- function(ocupancia_012, T_n)
{
  # Input:
  #      . ocupancia_012 es la matriu ocupancies (2 no mostrejat, 1 presencia, 0 absencia):  
  #       It  1994  ...  ...  ... ... 2024
  #       12   2     2    1    0   1   0
  #      . T_n:  patro de 1, 0 0 0 0 seguits a comptabilitzar
  
  ocupancia_012$local_extinctions <- apply(ocupancia_012, 1, count_extinction_pattern, n = T_n)
  
  res <- apply(ocupancia_012, 1, count_extinction_pattern, n = T_n)
  counts  <- res["count", ]
  
  N <- sum(counts)
  
  return(N) #Numero d'extincions observades (amb recolonitzacio)
}

Local_Extinction_2024 <- function(ocupancia_012, T_n)
{
  # Input:
  #      . ocupancia_012 es la matriu ocupancies (2 no mostrejat, 1 presencia, 0 absencia):  
  #       It  1994  ...  ...  ... ... 2024
  #       12   2     2    1    0   1   0
  #      . T_n:  patro de 1, 0 0 0 0 seguits a comptabilitzar
  
  res <- apply(ocupancia_012, 1, count_extinction_pattern, n = T_n)
  counts_end  <- res["count_end", ]
  
  N <- sum(counts_end)
  
  return(N) #Numero d'extincions observades fins al 2024
}

# Compta el numero de vegades que s'ha observat un exintion pattern del tipus (1 0 0 ... 0 0) 
# en el conjunt d'itineraris que composa la metapoblacio d'una especie en una bioregio

################

# Compta el numero de vegades que s'ha observat un exintion pattern del
# tipus (1 0 0 ... 0 0)
# en el conjunt d'itineraris que composa la metapoblacio d'una especie
# en una bioregio

n_Extincions <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions <- as.data.frame(n_Extincions)

n_Extincions_2024 <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions_2024 <- as.data.frame(n_Extincions_2024)

n_Extincions_per_IT <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions_per_IT <- as.data.frame(n_Extincions_per_IT)

n_Extincions_per_IT <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions_per_IT <- as.data.frame(n_Extincions_per_IT)

n_Extincions_per_IT_Total <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions_per_IT_Total <- as.data.frame(n_Extincions_per_IT_Total)

n_Extincions_per_IT_2024 <- matrix(
  nrow = length(Sp),
  ncol = length(BioReg),
  dimnames = list(Sp, BioReg)
)
n_Extincions_per_IT_2024 <- as.data.frame(n_Extincions_per_IT_2024)

for (i in 1:12 ) {
  
  data_nNM <- as.data.frame(my_list[[i]])
  
  for (j in 1:3) {
    # Dades ocupancia species i en cada regions.
    
    print(paste("Calculating Extinctions for Species", Sp[i], "in",
                BioReg[j]))
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
      if(j == 2) n_Extincions_per_IT$T_2[i] = T
      if(j == 3) n_Extincions_per_IT$T_3[i] = T
      
      # M Nombre d'itineraris que defineixen l'Sp i en BR j
      MM = 4 + (j-1)*3  # Numero de columna on hi ha M1, M2, o M3:
      # metapoblacio potencial)
      M = data_nNM[31, MM]
      if (M > 0) {
        n_Extincions[i,j] <- Local_Extinction_Pattern(data_ocupancia, T_n)
        n_Extincions_2024[i,j] <- Local_Extinction_2024(data_ocupancia, T_n)
        
        n_Extincions_per_IT[i,j] <- n_Extincions[i,j] / M
        n_Extincions_per_IT_2024[i,j] <- n_Extincions_2024[i,j] / M
        
        n_Extinctions_Total[i,j] <- n_Extincions[i,j] + n_Extincions_2024[i,j] 
        n_Extincions_per_IT_Total[i,j] <- n_Extincions_Total[i,j] / M
      }
    }
  }
}

### PANEL 3 GRAFICS NOMBRE D-EXTINCIONS PER ITINERARI##########################################

#Transformem rownames en una columna "species"
library(tibble)
n_Extincions_per_IT <- n_Extincions_per_IT %>%
  rownames_to_column(var = "Species")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# 1. Definición de colores
species_colors <- c(
  "Celastrina argiolus" = "#FF4A00",
  "Lycaena vigaureae" = "#F2E600",
  "Plebejus argus" = "#4C6A7F",
  "Pseudophilotes panoptes" = "#0B1E8A",
  "Cyaniris semiargus" = "#0A2DBF",
  "Vanessa cardui" = "#D4B000",
  "Aglais io" = "#8A2BE2",
  "Anthocharis euphenoides" = "#8ED1DC",
  "Melanargia occitanica" = "#1CA3D1",
  "Pararge aegeria" = "#8B0000",
  "Pyronia bathseba" = "#C71585",
  "Pyronia cecilia" = "#F48FB1"
)

# 2. Definición del orden
species_order <- c(
  "Pseudophilotes panoptes", "Cyaniris semiargus",   "Plebejus argus",
 "Aglais io", "Melanargia occitanica", "Anthocharis euphenoides", "Vanessa cardui",
  "Lycaena vigaureae","Pararge aegeria","Celastrina argiolus",  
   "Pyronia bathseba", "Pyronia cecilia"
)

# 3. Preparar datos y aplicar factor
df_long <- n_Extincions_per_IT %>%
  pivot_longer(
    cols = 2:4,
    names_to = "Region",
    values_to = "Value"
  )

df_long$Species <- factor(df_long$Species, levels = species_order)

# 4. Generar gráfico con Zoom (0.5) y Cursivas
panel_final <- ggplot(df_long, aes(x = Species, y = Value, fill = Species)) +
  geom_col(width = 0.7) +
  facet_wrap(~Region, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_colors) +
  coord_cartesian(ylim = c(0, 1.25)) +
  labs(title = "", x = "Espècies", y = "n.º d'extincions/ itinerari", fill = "Espècies") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(face = "italic"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

print(panel_final)
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/Extincions/Panel_Extincions.png", plot = panel_final, width = 8, height = 10, dpi = 300)



##################################################
#PANELL DEL TEMPS CARACTERISTIC

# 1. Definir el orden basado en la columna Species de tu DF actual

df_temps$Species <- factor(df_temps$Species, levels = species_order)

#NOu panell temps amb barres d'error

panell_temps <- ggplot(df_temps, aes(x = Species, y = valor, fill = Species)) +
  
  geom_col(width = 0.7) +
  
  geom_errorbar(
    aes(ymin = low, ymax = up),
    width = 0.2,
    linewidth = 0.5
  ) +
  
  facet_wrap(~Region, ncol = 1, scales = "free_y") +
  
  scale_fill_manual(values = species_colors) +
  
  labs(
    title = "",
    x = "Espècies",
    y = "Temps característic",
    fill = ""
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(face = "italic"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

print(panell_temps)
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/Temps/Panel_Temps.png",
       plot = panell_temps,
       width = 8,
       height = 10,
       dpi = 300)


print(panell_temps)
ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/Extincions/panell_temps.png",
       plot = panell_temps, width = 8, height = 10, dpi = 300)

