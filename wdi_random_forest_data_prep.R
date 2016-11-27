setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
set.seed(42)

#load hdi index file
countries_hdi_index  <- read.csv("..\\..\\world-development-indicators\\hdi_index.csv")
head(countries_hdi_index,5)
count(countries_hdi_index,"hdi_index")

#Merge indicator data with hdi index
indicator_hdi_index <- merge(indicator_pivot_continent,countries_hdi_index,by = "CountryName", all.x = T)
ncol(indicator_hdi_index)

#Data for Random Forest 
indicator_hdi_index_filtered <- subset(indicator_hdi_index[,c(1,2,3,4,820,852,1212,1349)],Year == 2009)
colnames(indicator_hdi_index_filtered)
#write.csv(indicator_hdi_index_filtered,"..\\..\\world-development-indicators\\indicator_hdi_filtered.csv")
head(indicator_hdi_index_filtered,5)
colnames(indicator_hdi_index_filtered)[5] <- "GNI_per_capita"
colnames(indicator_hdi_index_filtered)[6] <- "Literacy_Rate"
colnames(indicator_hdi_index_filtered)[7] <- "Life_Expectancy"
colnames(indicator_hdi_index_filtered)




