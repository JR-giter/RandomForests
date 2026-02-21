# This is a kind of storage room for possible data records and links
#install.packages("AmesHousing")
library(AmesHousing)
library(dplyr)
library(tidyverse)
ames<- make_ames()
glimpse(ames)
head(ames) |> select("Gr_Liv_Area", "Garage_Cars", "Overall_Qual", "Year_Built", "Garage_Area")
ames$Garage_Cars
