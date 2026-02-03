GColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

install.packages("readxl")
install.packages("cowplot")
# library(cowplot)

library(readxl)
library(dplyr)

library(ggplot2)
library(tidyr)

SSI <- list()
HPI <- list()
especies_ordered <- c(
  "Pseudophilotes panoptes",
  "Cyaniris semiargus",
  "Plebejus argus",
  "Aglais io",
  "Melanargia occitanica",
  "Anthocharis euphenoides",
  "Vanessa cardui",
  "Lycaena virgaureae",
  "Pararge aegeria",
  "Celastrina argiolus",
  "Pyronia bathseba",
  "Pyronia cecilia") 

my_colors <- c(
  "Pseudophilotes panoptes" = "darkblue",
  "Cyaniris semiargus" = "mediumblue",
  "Plebejus argus" = "steelblue4",
  "Aglais io" = "blueviolet",
  "Melanargia occitanica" = "deepskyblue1",
  "Anthocharis euphenoides" = "cadetblue2",
  "Vanessa cardui" = "gold2",
  "Lycaena virgaureae" = "yellow",
  "Pararge aegeria" = "red4",
  "Celastrina argiolus" = "orangered",
  "Pyronia bathseba" = "violetred",
  "Pyronia cecilia" = "palevioletred1"
)

# Definición del orden personalizado de las especies

SSI <- c(1.430, 2.263, 2.403, 0.831, 1.314, 0.652, 0.813, 2.552, 0.942, 0.593, 0.764, 0.796)
HPI <- c(0.577, 0.183, 0.129, 0.544, 0.354, 0.667, 0.019, 0.707, 0.111, 0.044, 0.408, 0.707)
Mobilitat <- c(1, 2, 1, 1, 2, 3, 4, 2, 3, 2, 2, 2)

# Lectura de les taules de dades (dataframes ordenants) amb les colonitzacions i les extinctions
# de totes les especies. 
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_ordenado.RData")
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1_ordenado.RData")
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2_ordenado.RData")
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3_ordenado.RData")

colext_Results_df_ordenado$SSI <- SSI 
colext_Results_df_ordenado$HPI <- HPI 
colext_Results_df_ordenado$Mobilitat <- Mobilitat 

#############################################################################
# Creating a legend-only plot according to my colors:
legend_df <- data.frame(
  species = factor(
    names(my_colors),
    levels = names(my_colors)
  ),
  x = 1,
  y = 1
)

p_legend <- ggplot(
  legend_df,
  aes(x = x, y = y, color = species)
) +
  geom_point(size = 4) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(face = "italic", size = 10)
  )

# install.packages("cowplot")
library(cowplot)

legend_only <- get_legend(p_legend)

ggsave(
  "~/PROJECT_JOANA_TFG/GRAFICS/legend_species.pdf",
  legend_only,
  width = 4,
  height = 6
)

#############################################################################
#Calculs per bioregio Alpina i subalpina
#(extraim melanargia i pyronia ceci perque s'han observat en molts poc itineraris alpins)

colext_Results_df_BR1_ordenado$SSI <- SSI 
colext_Results_df_BR1_ordenado$HPI <- HPI 
colext_Results_df_BR1_ordenado$Mobilitat <- Mobilitat

melapyro <- c("Melanargia occitanica", "Pyronia cecilia")

#Crear una versión filtrada de tu orden personalizado
especies_ordered_BR1 <- especies_ordered[!especies_ordered %in% melapyro]

# Crear una versión filtrada de tus colores
my_colors_BR1 <- my_colors[names(my_colors) %in% especies_ordered_BR1]


gg_SSI_C_BR1 <- 
  ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
         aes(x = SSI, y = C_BR1, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR1, ymax = C_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Alpina and Subalpine Region",
       x = "SSI",
       y = "Colonization",
       color = "species"
  ) +
  # Usar la escala de colores manual con tus vectores filtrados
  scale_color_manual(values = my_colors_BR1, limits = especies_ordered_BR1) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(gg_SSI_C_BR1)
##

##
gg_SSI_E_BR1 <- 
  ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
         aes(x = SSI, y = E_BR1, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR1, ymax = E_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Alpine and Subalpine Region",
       x = "SSI",
       y = "Extinction",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR1) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(gg_SSI_E_BR1)
##

#
gg_HPI_C_BR1 <- 
  ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
         aes(x = HPI, y = C_BR1, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR1, ymax = C_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Regio Alpina i subalpina",
       x = "HPI",
       y = "Colonització",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR1) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_C_BR1)
###


gg_HPI_E_BR1 <- 
  ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
         aes(x = HPI, y = E_BR1, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR1, ymax = E_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Regio Alpina i subalpina",
       x = "HPI",
       y = "Extinció",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR1) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_E_BR1)
###

#####################################################################
#####################################################################
#Calculs per Bioregio mediterrania humida

colext_Results_df_BR2_ordenado$SSI <- SSI 
colext_Results_df_BR2_ordenado$HPI <- HPI 
colext_Results_df_BR2_ordenado$Mobilitat <- Mobilitat 

#Cyaniris semiargus: El seu rang altitudinal va dels 500 als 2.400 m,
#encara que és poc freqüent trobar-la per sota dels 1.000 m."
#CBMS nomes calcula la tendencia de la regio alpina

#Lycaena virgaurear: "El seu rang altitudinal va dels 500 als 2.400 m, 
#encara que és poc freqüent trobar-la per sota dels 1.000 m."
#CBMS nomes calcula la tendencia de la regio alpina

lyca <- c("Lycaena virgaureae")
lyca_cyan <- c("Lycaena virgaureae", "Cyaniris semiargus")

#Crear una versión filtrada de tu orden personalizado
especies_ordered_BR2 <- especies_ordered[!especies_ordered %in% lyca_cyan]

# Crear una versión filtrada de tus colores
my_colors_BR2 <- my_colors[names(my_colors) %in% especies_ordered_BR2]


###
gg_SSI_C_BR2 <- 
  ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
         aes(x = SSI, y = C_BR2, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR2, ymax = C_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Humid Mediterranian Region",
       x = "SSI",
       y = "Colonization",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR2) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(gg_SSI_C_BR2)
##

##
gg_SSI_E_BR2 <- 
  ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
         aes(x = SSI, y = E_BR2, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR2, ymax = E_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Humid Mediterranean Region",
       x = "SSI",
       y = "Extinction",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR2) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(gg_SSI_E_BR2)

###

###
gg_HPI_C_BR2 <- 
  ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
         aes(x = HPI, y = C_BR2, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR2, ymax = C_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Regió mediterrània humida",
       x = "HPI",
       y = "Colonització",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR2) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_C_BR2)
###

###
gg_HPI_E_BR2 <- 
  ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
         aes(x = HPI, y = E_BR2, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR2, ymax = E_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Humid Mediterranean Region",
       x = "HPI",
       y = "Extinció",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR2) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_E_BR2)

#############################################################################
#############################################################################
#Calcul dels grafics Bioregio Mediterrania Arida

colext_Results_df_BR3_ordenado$SSI <- SSI 
colext_Results_df_BR3_ordenado$HPI <- HPI 
colext_Results_df_BR3_ordenado$Mobilitat <- Mobilitat 

#Cyaniris semiargus: El seu rang altitudinal va dels 500 als 2.400 m,
#encara que és poc freqüent trobar-la per sota dels 1.000 m."
#CBMS nomes calcula la tendencia de la regio alpina

#Lycaena virgaurear: "El seu rang altitudinal va dels 500 als 2.400 m, 
#encara que és poc freqüent trobar-la per sota dels 1.000 m."
#CBMS nomes calcula la tendencia de la regio alpina

lyca <- c("Lycaena virgaureae")
lyca_cyan <- c("Lycaena virgaureae", "Cyaniris semiargus")

#Crear una versión filtrada de tu orden personalizado
especies_ordered_BR3 <- especies_ordered[!especies_ordered %in% lyca_cyan]

# Crear una versión filtrada de tus colores
my_colors_BR3 <- my_colors[names(my_colors) %in% especies_ordered_BR3]


###
gg_SSI_C_BR3 <- 
  ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
         aes(x = SSI, y = C_BR3, color = species))+
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR3, ymax = C_up_BR3), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Arid Mediterranian Region",
       x = "SSI",
       y = "Colonitzation",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR3) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(gg_SSI_C_BR3)
##

##
gg_SSI_E_BR3 <- 
  ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
         aes(x = SSI, y = E_BR3, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR3, ymax = E_up_BR3), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Arid Mediterrenean Region",
       x = "SSI",
       y = "Extinction",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR3) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(gg_SSI_E_BR3)

###

###
gg_HPI_C_BR3 <- 
  ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
         aes(x = HPI, y = C_BR3, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR3, ymax = C_up_BR3), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Regió mediterrània àrida",
       x = "HPI",
       y = "Colonització",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR3) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_C_BR3)
###

###
gg_HPI_E_BR3 <- 
  ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
         aes(x = HPI, y = E_BR3, color = species)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR3, ymax = E_up_BR3), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Regió mediterrània àrida",
       x = "HPI",
       y = "Extinció",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR3) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_E_BR3)

save(colext_Results_df_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_ordenado.RData")
save(colext_Results_df_BR1_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1_ordenado.RData")
save(colext_Results_df_BR2_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2_ordenado.RData")
save(colext_Results_df_BR3_ordenado, file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3_ordenado.RData")

##############################################
##############################################
#Calcul grafics mobilitat

df <- data.frame(
  Species = factor(c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11", "S12")),
  Mobility = factor(c(1,2,1,3,1,1,1,3,3,4,1,4)),  # As a factor
  Colonization = c(0.2, 0.25, 0.5, 0.45, 0.55, 0.7, 0.75, 0.65, 0.68, 0.9, 0.92, 0.88),
  SE = c(0.05, 0.04, 0.06, 0.05, 0.07, 0.03, 0.04, 0.05, 0.04, 0.06, 0.05, 0.07)
)


library(ggplot2)

Mobilitat_C <- ggplot(colext_Results_df_ordenado, aes(x = Mobilitat, y = C, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low, ymax = C_up),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(x = "Mobility Ability",
       y = "Colonization Rate",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_C)
###

###
Mobilitat_E <- ggplot(colext_Results_df_ordenado, aes(x = Mobilitat, y = E, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low, ymax = E_up),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(x = "Mobility Ability",
       y = "Extinction Rate",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_E)
###
###
###
Mobilitat_C_BR1 <- ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
                          aes(x = Mobilitat, y = C_BR1, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low_BR1, ymax = C_up_BR1),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "Alpine and Subalpine Region",
       x = "Mobility Index",
       y = "Colonization Rate",
       color = "species") +
  scale_color_manual(values = my_colors_BR1, limits = especies_ordered_BR1) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_C_BR1)
###
###
Mobilitat_E_BR1 <- ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
                          aes(x = Mobilitat, y = E_BR1, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low_BR1, ymax = E_up_BR1),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "Alpine and Subalpine Region",
       x = "Mobility Index",
       y = "Extinction Rate",
       color = "species") +
  scale_color_manual(values = my_colors_BR1, limits = especies_ordered_BR1) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_E_BR1)
###
###
###
Mobilitat_C_BR2 <- ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = C_BR2, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low_BR2, ymax = C_up_BR2),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "Humid Mediterranean Region",
       x = "Mobility Index",
       y = "Colonization Rate",
       color = "species") +
  scale_color_manual(values = my_colors_BR2, limits = especies_ordered_BR2) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_C_BR2)
###

Mobilitat_E_BR2 <- ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = E_BR2, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low_BR2, ymax = E_up_BR2),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "Humid Mediterranean Region",
       x = "Mobility Index",
       y = "Extinction Rate",
       color = "species") +
  scale_color_manual(values = my_colors_BR2, limits = especies_ordered_BR2) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_E_BR2)

###
Mobilitat_C_BR3 <- ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = C_BR3, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low_BR3, ymax = C_up_BR3),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "Arid Mediterrenean Region",
       x = "Mobility Index",
       y = "Colonization Rate",
       color = "species") +
  scale_color_manual(values = my_colors_BR3, limits = especies_ordered_BR3) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_C_BR3)
###

Mobilitat_E_BR3 <- ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = E_BR3, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low_BR3, ymax = E_up_BR3),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "Arid Mediterrenean Region",
       x = "Mobility Index",
       y = "Extinction Rate",
       color = "species") +
  scale_color_manual(values = my_colors_BR3, limits = especies_ordered_BR3) +
  theme(
    legend.position = "none",
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 20
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 20
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)
  )

print(Mobilitat_E_BR3)