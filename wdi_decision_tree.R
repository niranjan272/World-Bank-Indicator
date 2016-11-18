setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)
library(plyr)
library(fpc)

#load hdi index file
countries_hdi_index  <- read.csv("..\\..\\world-development-indicators\\hdi_index.csv")
head(countries_hdi_index,5)
count(countries_hdi_index,"hdi_index")

indicator_hdi_index <- merge(indicator_pivot_continent,countries_hdi_index,by = "CountryName", all.x = T)
ncol(indicator_hdi_index)
