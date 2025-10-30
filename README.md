# PROJECT_JOANA_TFG
# CBMS: COLONITZACIÓ I EXTINCIÓ POTENCIAL DE 12 ESPÈCIES DE PAPALLONES DE CATALUNYA 

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
