setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)
library(plyr)
library(fpc)
#Load File
indicator_pivot  <- read.csv("..\\..\\world-development-indicators\\indiacator_pivot_names.csv",na.strings = "..")
indicator_pivot_desc <- str(indicator_pivot)

#Run data cleaning code (wdi_data_clean.R)

#Load Country- Continent Mapping
country_continent_mapping <- read.csv("..\\..\\world-development-indicators\\continent_country_mapping.csv")
str(country_continent_mapping)
head(country_continent_mapping)


#Merge Indicaotr Data with Continent and Country mapping
indicator_pivot_continent <- merge(indicator_pivot,country_continent_mapping,by = "CountryName",all.x = T)
ncol(indicator_pivot_continent)


#Reordering Columns
indicator_pivot_continent <- indicator_pivot_continent[,c(1348,1:1347)]
colnames(indicator_pivot_continent)[c(1,2,3,4,5)]
count(indicator_pivot_continent,"Continent")

#Run data cleaning code (wdi_data_clean.R)

#checking country names
data_county_name  <- indicator_pivot_continent[,1:2]
write.csv(data_county_name,"..\\..\\world-development-indicators\\data_county_name.csv")
indicator_pivot_continent <- na.omit(indicator_pivot_continent)
any(is.na(indicator_pivot_continent))

#Agricultural land (% of land area)
agricultural_land <- subset(indicator_pivot_continent[c(1,2,3,4,9)])
colnames(agricultural_land)

agricultural_land_clustering <- kmeans(agricultural_land[,4:5],4, nstart = 20)
agricultural_land_clustering
agricultural_land_clustering$cluster <- as.factor(agricultural_land_clustering$cluster)

#Plot Cluster Data
clusplot(agricultural_land[,4:5],agricultural_land_clustering$cluster,colour = T,shade = T)
plotcluster(wdi_gini_index[,3:4],wdi_gini_index_clustering$cluster)
plot(wdi_gini_index$Year, wdi_gini_index$G7INI.index..World.Bank.estimate.,col=wdi_gini_index_clustering$cluster)



#Electricity Consuption
# % of Population
electricity_access <- subset(indicator_pivot_continent[c(1,2,3,4,352,353,354)], 
                             Year == 1990 | Year == 2000 | Year == 2010 | Year == 2012 )
colnames(electricity_access)
count(electricity_access,"Year")
ggplot(electricity_access, aes(x = electricity_access[,7], y = factor(Year),colour = electricity_access$Continent)) + 
  geom_point() + xlab("Access to electricity (% of population)") + ylab("Year")

#Rular Population
ggplot(electricity_access, aes(x = electricity_access[,5], y = factor(Year),colour = electricity_access$Continent)) + 
  geom_point() + xlab("Access to electricity (% of rural population)") + ylab("Year")

#Urban Population
ggplot(electricity_access, aes(x = electricity_access[,6], y = factor(Year),colour = electricity_access$Continent)) + 
  geom_point() + xlab("Access to electricity (% of urban population)") + ylab("Year")

#Electricity Consumption Clustering
list_country  = c('AFG' ,	'ARG' ,	'BRA' , 	'CHI' ,	'ETH' ,	'DEU' ,	'IND' ,	'PAK' ,	'RUS' ,	'THA' ,	'GBR' , 'USA')
list_year  = c('1990','2000','2010','2012')
electricty_access_country_year <- subset(electricity_access, CountryCode %in% list_country & Year  %in% list_year)
head(electricty_access_country_year,5)
electricity_access_clustering <- kmeans(electricty_access_country_year[,c(4,7)],4,nstart = 20)

#clusplot(electricty_access_country_year[,c(4,7)],electricity_access_clustering$cluster,colour = T,shade = T)
center_cluster = as.data.frame(electricity_access_clustering$centers)
center_cluster
ggplot(electricty_access_country_year,aes(x = factor(Year), y = electricty_access_country_year$Access.to.electricity....of.population.,
                              colour  = factor(electricity_access_clustering$cluster))) + geom_point() +
geom_point(data = center_cluster, aes(x=factor(Year),y= center_cluster$Access.to.electricity....of.population.,colour  = 'Center')) + 
geom_point(data = center_cluster, aes(x=factor(Year),y= center_cluster$Access.to.electricity....of.population.,colour = 'Center'), 
 size=5.2,alpha=.3) + ylab("Access to electricity (% of population)") + xlab("Year")


ggplot(electricty_access_country_year,aes(x = factor(CountryName), y = electricty_access_country_year$Access.to.electricity....of.population.,
                                   colour  = factor(electricity_access_clustering$cluster))) + geom_point() 

