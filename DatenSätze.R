# File um mögliche Projekte / Datensätze zu sammeln
#install.packages("AmesHousing")
library(AmesHousing)
library(tidyverse)
ames<- make_ames()
glimpse(ames)
