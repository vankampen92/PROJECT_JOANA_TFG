# PROJECT JOANA TFG, UdG
# COLONITZACIÓ I EXTINCIÓ POTENCIAL DE 12 ESPÈCIES DE PAPALLONES DE CATALUNYA (CBMS)

## Uploading the Data in R:
```r
# Required libraries:
library(tidyverse)
library(island)
library(data.table)
install.packages("stargazer")
library(stargazer)

# Cargamos los datos:: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")

data <- ColExtDades
```
We create lists to store intermediate and final results, and store species latin names: 
```r
list_multiple <- list()

colext_Sp_Results <- list()
No_of_TRANSITIONS <- vector()

# Crear Specie_Latin_Names amb el nom de cada especie
Species_Latin_Names <- unique(data$sp_latin)
```
