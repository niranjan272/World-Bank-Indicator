setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)
library(fpc)
#Load File
indicator_pivot  <- read.csv("..\\..\\world-development-indicators\\indiacator_pivot_names.csv")

#Load Series data
data_series <- read.csv("..\\..\\world-development-indicators\\Series.csv")
head(data_series)
data_series$Topic_Main <-NA
data_series$Topic_Main <- substr(data_series$Topic,1,regexpr(':',data_series$Topic)-1)
data_series_filtered <- data_series[c("Topic","Topic_Main")]
head(data_series_filtered,1)
colnames(indicator_pivot)

#Load Country- Continent Mapping
country_continent_mapping <- read.csv("..\\..\\world-development-indicators\\continent_country_mapping.csv")
head(country_continent_mapping)


#Merge Indicaotr Data with Continent and Country mapping
indicator_pivot_continent <- merge(indicator_pivot,country_continent_mapping,by = "CountryName")
ncol(indicator_pivot_continent)
#Reordering Columns
indicator_pivot_continent <- indicator_pivot_continent[,c(1348,1:1347)]
colnames(indicator_pivot_continent)[c(1,2,3,4,5)]

#Agricultural land (% of land area)
agricultural_land <- subset(indicator_pivot_continent[c(1,2,3,4,9)])
colnames(agricultural_land)

agricultural_land_clustering <- kmeans(agricultural_land[,4:5],4, nstart = 20)
agricultural_land_clustering
agricultural_land_clustering$cluster <- as.factor(agricultural_land_clustering$cluster)

#Plot Cluster Data
clusplot(agricultural_land[,4:5],agricultural_land_clustering$cluster,colour = T,shade = T)
plotcluster(wdi_gini_index[,3:4],wdi_gini_index_clustering$cluster)
plot(wdi_gini_index$Year, wdi_gini_index$GINI.index..World.Bank.estimate.,col=wdi_gini_index_clustering$cluster)
#
#

ggplot(wdi_gini_index,aes(x = Year, y = wdi_gini_index$Agricultural.land....of.land.area., 
                          colour = wdi_gini_index$CountryName)) +
  geom_point(stat = "identity",size = 5)
