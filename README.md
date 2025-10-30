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
