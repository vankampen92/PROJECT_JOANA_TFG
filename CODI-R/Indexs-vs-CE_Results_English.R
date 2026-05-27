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
  geom_point(size = 3) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR1, ymax = C_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "SSI",
       y = "c",
       color = "species"
  ) +
  # Usar la escala de colores manual con tus vectores filtrados
  scale_color_manual(values = my_colors_BR1, limits = especies_ordered_BR1) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4), 
    
   # Para crear el recuadro dell grafico 
  #  panel.border = element_rect(
  #    color = "gray80",
  #    fill = NA,
   #   linewidth = 0.8
   # ),
    
    # ---- Titles ---- #
  
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

print(gg_SSI_C_BR1)
##

##
gg_SSI_E_BR1 <- 
  ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
         aes(x = SSI, y = E_BR1, color = species)) +
  geom_point(size = 3) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR1, ymax = E_up_BR1), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "SSI",
       y = "e",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR1) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
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
  geom_point(size = 3) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR2, ymax = C_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "SSI",
       y = "c",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR2) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 17
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

print(gg_SSI_C_BR2)
##

##
gg_SSI_E_BR2 <- 
  ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
         aes(x = SSI, y = E_BR2, color = species)) +
  geom_point(size = 3) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR2, ymax = E_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "SSI",
       y = "e",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR2) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

print(gg_SSI_E_BR2)

###

###
gg_HPI_C_BR2 <- 
  ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
         aes(x = HPI, y = C_BR2, color = species)) +
  geom_point(size = 3) + #El color se define por 'species' en aes()
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
  geom_point(size = 3) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR3, ymax = C_up_BR3), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "SSI",
       y = "c",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR3) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

print(gg_SSI_C_BR3)
##

##
gg_SSI_E_BR3 <- 
  ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
         aes(x = SSI, y = E_BR3, color = species)) +
  geom_point(size = 3) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low_BR3, ymax = E_up_BR3), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "",
       x = "SSI",
       y = "e",
       color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered_BR3) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 22
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
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
###MOBILITAT EN LES 3 BIOREGIONS
###
Mobilitat_C_BR1 <- ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
                          aes(x = Mobilitat, y = C_BR1, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low_BR1, ymax = C_up_BR1),
                width = 0.005,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "",
       x = "Mobilitat",
       y = "c",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # ----  Control de las líneas del fondo ----
    # Líneas principales: las hacemos más oscuras (gray80) y gruesas (0.6)
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    
    # Líneas secundarias: las hacemos muy tenues (gray95) y finas (0.3)
    # Nota: si prefieres borrarlas por completo para que no saturen, usa: element_blank()
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4), 
    # ------------------------------------------------------
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

#print(Mobilitat_C_BR1)
###
###
Mobilitat_E_BR1 <- ggplot(filter(colext_Results_df_BR1_ordenado, !species %in% melapyro),
                          aes(x = Mobilitat, y = E_BR1, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low_BR1, ymax = E_up_BR1),
                width = 0.005,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "",
       x = "Mobilitat",
       y = "e",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4), 
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )


###
###
###
Mobilitat_C_BR2 <- ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = C_BR2, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low_BR2, ymax = C_up_BR2),
                width = 0.005,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "",
       x = "Mobilitat",
       y = "c",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

#print(Mobilitat_C_BR2)
###

Mobilitat_E_BR2 <- ggplot(filter(colext_Results_df_BR2_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = E_BR2, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low_BR2, ymax = E_up_BR2),
                width = 0.005,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "",
       x = "Mobilitat",
       y = "e",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

#print(Mobilitat_E_BR2)

###
Mobilitat_C_BR3 <- ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = C_BR3, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = C_low_BR3, ymax = C_up_BR3),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "",
       x = "Mobilitat",
       y = "c",
       color = "species") +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

#print(Mobilitat_C_BR3)
###

Mobilitat_E_BR3 <- ggplot(filter(colext_Results_df_BR3_ordenado, !species %in% lyca_cyan),
                          aes(x = Mobilitat, y = E_BR3, color = species)) +
  geom_point(position = position_dodge(width = 0.4), size  = 3) +
  geom_errorbar(aes(ymin = E_low_BR3, ymax = E_up_BR3),
                width = 0.2,              
                linewidth = 0.8,                
                position = position_dodge(width = 0.4)) + 
  theme_minimal() +
  labs(title = "",
       x = "Mobilitat",
       y = "e",
       color = NULL) +
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.position = "none",
    
    # Lineas del grafico principales y secundarias en diferente color y grosor
    panel.grid.major = element_line(color = "gray80", linewidth = 0.6),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.4),
    
    # ---- Titles ----
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 19
    ),
    
    # ---- Axis titles ----
    axis.title.x = element_text(
      face = "bold",
      size = 17
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 17
    ),
    
    # ---- Tick labels ----
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
  )

#print(Mobilitat_E_BR3)

################## panell_final_indexos - c i e per bioregio####################


install.packages("patchwork")
library(patchwork)
library(grid)
library(cowplot)

### Regio bioclimatica Alpina i Subalpina (AS)
Panel_AS <- (
  Mobilitat_C_BR1 + Mobilitat_E_BR1) / ( gg_SSI_C_BR1 + gg_SSI_E_BR1 ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.key.width = unit(1.2, "cm")
  ) &
  guides(
    color = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  )

##fem un grafic en el que deixarem nomes la llegenda per despres afegirla a la 
## la par de abaix dels 4 grafics de la bioreg A.S
legend_plot_BR1 <- ggplot(
  colext_Results_df_BR1_ordenado,
  aes(x = SSI, y = C_BR1, color = species)
) +
  geom_point(position = position_dodge(width = 0.4), size=5) +
  scale_color_manual(
    values = my_colors,
    limits = especies_ordered
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "italic")
  )

legend_1 <- cowplot::get_legend(legend_plot_BR1)


#Montem el panell de 4 grafics
Panel_AS <- (
  Mobilitat_C_BR1 + Mobilitat_E_BR1
) / (
  gg_SSI_C_BR1 + gg_SSI_E_BR1
)

final_plot_AS <- cowplot::plot_grid(
  Panel_AS,
  legend_1,
  ncol = 1,
  rel_heights = c(1, 0.22)
)

# Añadimos margen derecho específicamente a todo el panel 
# El orden de margin es: t (top), r (right), b (bottom), l (left)
Panel_AS <- Panel_AS & theme(
  plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5, unit = "pt")
)
# ---------------------------
print(final_plot_AS)

ggsave("/home/dalonso/PROJECT_JOANA_TFG/GRAFICS/grafics_indexs_cie/Panel_Indexos_AS.png",
       plot = final_plot_AS,
       width = 9,
       height = 6,
       dpi = 300)

################################################################

### Panell Indexos amb c i e en regio Mediterrania Humida (MH)

Panel_MH <- (
  Mobilitat_C_BR2 + Mobilitat_E_BR2) / ( gg_SSI_C_BR2 + gg_SSI_E_BR2 ) 
    

##fem un grafic en el que deixarem nomes la llegenda per despres afegirla a la 
## la par de abaix dels 4 grafics de la bioreg A.S
legend_plot_BR2 <- ggplot(
  colext_Results_df_BR2_ordenado,
  aes(x = SSI, y = C_BR2, color = species)
) +
  geom_point(position = position_dodge(width = 0.4), size=5) +
  scale_color_manual(
    values = my_colors,
    limits = especies_ordered
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "italic")
  )

legend_2 <- cowplot::get_legend(legend_plot_BR2)


#Montem el panell de 4 grafics
Panel_MH <- (
  Mobilitat_C_BR2 + Mobilitat_E_BR2
) / (
  gg_SSI_C_BR2 + gg_SSI_E_BR2
)

final_plot_MH <- cowplot::plot_grid(
  Panel_MH,
  legend_2,
  ncol = 1,
  rel_heights = c(1, 0.22)
)

# Añadimos margen derecho específicamente a todo el panel 
# El orden de margin es: t (top), r (right), b (bottom), l (left)
Panel_MH <- Panel_MH & theme(
  plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5, unit = "pt")
)

print(final_plot_MH)
# ---------------------------
######################################

### Panell Indexos amb c i e en regio Mediterrania Arida (MA)

Panel_MA <- (
  Mobilitat_C_BR3 + Mobilitat_E_BR3) / ( gg_SSI_C_BR3 + gg_SSI_E_BR3 ) 

##fem un grafic en el que deixarem nomes la llegenda per despres afegirla a la 
## la par de abaix dels 4 grafics de la bioreg A.S
legend_plot_BR3 <- ggplot(
  colext_Results_df_BR3_ordenado,
  aes(x = SSI, y = C_BR3, color = species)
) +
  geom_point(position = position_dodge(width = 0.4), size=5) +
  scale_color_manual(
    values = my_colors,
    limits = especies_ordered
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "italic")
  )

legend_3 <- cowplot::get_legend(legend_plot_BR3)


#Montem el panell de 4 grafics
Panel_MA <- (
  Mobilitat_C_BR3 + Mobilitat_E_BR3
) / (
  gg_SSI_C_BR3 + gg_SSI_E_BR3
)

final_plot_MA <- cowplot::plot_grid(
  Panel_MA,
  legend_3,
  ncol = 1,
  rel_heights = c(1, 0.22)
)

# Añadimos margen derecho específicamente a todo el panel 
# El orden de margin es: t (top), r (right), b (bottom), l (left)
Panel_MA <- Panel_MA & theme(
  plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5, unit = "pt")
)

print(final_plot_MA)
# ---------------------------




