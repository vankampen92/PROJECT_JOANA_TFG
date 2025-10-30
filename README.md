# PROJECT_JOANA_TFG
# CBMS: COLONITZACIÓ I EXTINCIÓ POTENCIAL DE 12 ESPÈCIES DE PAPALLONES DE CATALUNYA 

## Uploading the Data in R:
```r
# (just to check whether this change is visible when Joana does git pull!!!)
library(tidyverse)
library(island)
library(data.table)
install.packages("stargazer")
library(stargazer)

# Cargamos los datos
# BEGIN: Exemples inicials... 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2023.csv")

# Including 2024 year: 
ColExtDades <- read.csv(file="/home/dalonso/PROJECT_JOANA_TFG/DADES/CBMS_colext_2024.csv")

data <- ColExtDades
```
