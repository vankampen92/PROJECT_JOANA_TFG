ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")
#Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")
data <- ColExtDades

install.packages("readxl")
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
Mobilitat <- c(1, 2, 1, 1, 4, 3, 3, 2, 3, 2, 2, 2)

# Lectura de les taules de dades (dataframes ordenants) amb les colonitzacions i les extinctions
# de totes les especies. 
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_ordenado.RData")
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR1_ordenado.RData")
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR2_ordenado.RData")
load(file = "/home/dalonso/PROJECT_JOANA_TFG/DADES/colext_Results_df_BR3_ordenado.RData")

colext_Results_df_ordenado$SSI <- SSI 
colext_Results_df_ordenado$HPI <- HPI 
colext_Results_df_ordenado$Mobilitat <- Mobilitat 

#Afegir columna de occupancia teorica
colext_Results_df_ordenado$occ_teorica <- colext_Results_df_ordenado$C / (colext_Results_df_ordenado$C + colext_Results_df_ordenado$E) 
colext_Results_df_BR1_ordenado$occ_teorica <- colext_Results_df_BR1_ordenado$C_BR1 / (colext_Results_df_BR1_ordenado$C_BR1 + colext_Results_df_BR1_ordenado$E_BR1) 
colext_Results_df_BR2_ordenado$occ_teorica <- colext_Results_df_BR2_ordenado$C_BR2 / (colext_Results_df_BR2_ordenado$C_BR2 + colext_Results_df_BR2_ordenado$E_BR2) 
colext_Results_df_BR3_ordenado$occ_teorica <- colext_Results_df_BR3_ordenado$C_BR3 / (colext_Results_df_BR3_ordenado$C_BR3 + colext_Results_df_BR3_ordenado$E_BR3) 

colext_Results_df_ordenado$AIC <- (4 + 2*colext_Results_df_ordenado$NLL)

NLL2 <- vector()
NNL2 <- (colext_Results_df_BR1_ordenado$NLL_BR1 + colext_Results_df_BR2_ordenado$NLL_BR2 + colext_Results_df_BR3_ordenado$NLL_BR3)
AIC2 <- vector()
AIC2 <- 12 + 2*(NNL2)

#Afegir columna del AIC del segon model amb 6 parametres al dataframe general
colext_Results_df_ordenado$AIC2 <- AIC2
#Calcular la diferencia entre els dos models:
colext_Results_df_ordenado$delta_AIC <- (colext_Results_df_ordenado$AIC2 - colext_Results_df_ordenado$AIC)
#Alla on sigui un valor negatiu indicara que el model amb 6 parametres es millor
#(perque hem fet AIC2-AIC1) (AIC = AIC general; amb 2 parametres c i e ) AIC2 = AIC amb 6 parametres (c,e i BR1, BR2, BR3) 

#per eliminar especies concretes del grafic, es fa un vector y despres es posa dins del filter
lyca_cyan <- c("Lycaena virgaureae", "Cyaniris semiargus")

# Horizontal bar plot
gg_deltaAIC <-
  ggplot(filter(colext_Results_df_ordenado, !species %in% lyca_cyan),
         aes(y = species, x = delta_AIC ,fill = species )) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = my_colors) +
  labs(title = "ΔAIC (Model2 - Model1) by Species",
       x = "ΔAIC", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

print(gg_deltaAIC) # <--- Asegúrate de que no haya líneas en blanco entre el ')' de theme_minimal y el 'print'
#MODEL 1 <- (amb 2 parametres)
#MODEL 2 <- (amb 6 parametres)
##valors negatius de delta AIC indiquen que Model 2 es millor

#S'han de treure les especies que no s'ha pogut calcular l'AIC2, com la l. virgaureae q
#unicament esta a itineraris alpins. (Absent a BR2 I BR3)
# i la cyaniris semiargus, que el seu rang altitudinal va dels 500 als 2.400 m, 
#encara que és poc freqüent trobar-la per sota dels 1.000 m. (Absent a BR3)

gg_SSI_C_total <- 
  ggplot(colext_Results_df_ordenado, aes(x = SSI, y = C, color = especies_ordered)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low, ymax = C_up), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(
    x = "SSI",
    y = "Colonització",
    color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_SSI_C_total)
##

##
gg_SSI_E_total <- 
  ggplot(colext_Results_df_ordenado, aes(x = SSI, y = E, color = especies_ordered)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(
    x = "SSI",
    y = "Extinció",
    color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_SSI_E_total)
##

##
gg_HPI_C_total <- 
  ggplot(colext_Results_df_ordenado, aes(x = HPI, y = C, color = especies_ordered)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low, ymax = C_up), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(
    x = "HPI",
    y = "Colonització",
    color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_C_total)
###


gg_HPI_E_total <- 
  ggplot(colext_Results_df_ordenado, aes(x = HPI, y = E, color = especies_ordered)) +
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = E_low, ymax = E_up), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(
    x = "HPI",
    y = "Extinció",
    color = "species" # Etiqueta para la leyenda de colores
  ) +
  # Usar la escala de colores manual con tu vector my_colors
  # Los 'limits' aquí aseguran que el orden de la leyenda sea el de orden_personalizado_spp_colex
  scale_color_manual(values = my_colors, limits = especies_ordered) +
  theme(
    legend.text = element_text(face = "italic"), # Texto de la leyenda en cursiva
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Títulos de ejes en negrita
  )
print(gg_HPI_E_total)

#############################################################################
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
  labs(title = "Regio Alpina i subalpina",
       x = "SSI",
       y = "Colonització",
       color = "species"
  ) +
  # Usar la escala de colores manual con tus vectores filtrados
  scale_color_manual(values = my_colors_BR1, limits = especies_ordered_BR1) +
  theme(
    legend.text = element_text(face = "italic"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
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
  labs(title = "Regio Alpina i subalpina",
    x = "SSI",
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
         aes(x = SSI, y = C_BR2, color = species))+
  geom_point(size = 2) + #El color se define por 'species' en aes()
  # Añadir barras de error horizontales para los intervalos de confianza de 'C'
  geom_errorbar(aes(ymin = C_low_BR2, ymax = C_up_BR2), width = 0.005, size = 0.8) +
  theme_minimal() +
  labs(title = "Regió mediterrània humida",
    x = "SSI",
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
  labs(title = "Regió mediterrània humida",
       x = "SSI",
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
  labs(title = "Regió mediterrània humida",
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
  labs(title = "Regió mediterrània àrida",
       x = "SSI",
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
  labs(title = "Regió mediterrània àrida",
       x = "SSI",
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

