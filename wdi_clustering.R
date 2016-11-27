setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)
library(maps)
library(ggrepel)
library(gridExtra)

#Filter for G20 Countries Excluding European Union
list_countries <- c('Australia','Canada','Saudi Arabia','United States','India','Russia',
                    'South Africa','Turkey','Argentina','Brazil','Mexico','France','Germany',
                    'Italy','United Kingdom','China','Indonesia','Japan','South Korea')

list_year  <- c(2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001)

#Extract Data for Goods and Service Exports
data_exports_goods_services <- subset(indicator_pivot_continent[,c(1,2,3,4,688)],
                                      CountryName %in% list_countries &
                                        Year %in% list_year) 
colnames(data_exports_goods_services)
colnames(data_exports_goods_services)[5] <- "Exports_Goods_Services"

#Clean data 
data_exports_goods_services <- subset(data_exports_goods_services, Exports_Goods_Services != 0)
count(data_exports_goods_services,c("Year","CountryName"))

#find the average for four years
average_exports <- function(x,y){
  data <- x
  print(colnames(data))
  years_average <- y
  print(years_average)
  data_subset <- subset(data, data$Year %in% years_average)
  data_subset_avg <- aggregate(data_subset,by = list(data_subset$CountryName),FUN = mean, na.rm = T)
  data_subset_avg <- data_subset_avg[,c(1,5,6)]
  colnames(data_subset_avg)
  colnames(data_subset_avg)[1] <- "CountryName"
  return(data_subset_avg)
  #write.csv(data_subset_avg,"data_subset_avg.csv")
}

#2012-2009
data_2012_2009 <- average_exports(data_exports_goods_services,c(2012,2011,2010,2009))
data_2012_2009[,2] <- 1

#2008-2005
data_2008_2005 <- average_exports(data_exports_goods_services,c(2008,2007,2006,2005))
data_2008_2005[,2] <- 2

#2004- 2001
data_2004_2001 <- average_exports(data_exports_goods_services,c(2004,2003,2002,2001))
data_2004_2001[,2] <- 3

#Combine Datasets
data_exports_goods_services <- rbind(data_2012_2009,data_2008_2005,data_2004_2001)
count(data_exports_goods_services,"Year")
colnames(data_exports_goods_services)

#Determine optimal number of clusters
any(is.na(data_exports_goods_services))
wss <- (nrow(data_exports_goods_services[,c(2,3)])-1)*sum(apply(data_exports_goods_services[,c(2,3)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_exports_goods_services[,c(2,3)],centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Optial Number of clusters are 4
#Apply kmeans 
exports_kmeans <- kmeans(data_exports_goods_services[,c(2,3)],4,iter.max = 10,nstart = 10)
exports_kmeans$centers
exports_kmeans
ggplot(data_exports_goods_services,aes(x = Exports_Goods_Services, y = factor(Year)),
       colour = factor(exports_kmeans$cluster)) + geom_point(colour = exports_kmeans$cluster,size = 2)+
  geom_text_repel(aes(label= data_exports_goods_services$CountryName),
                  color = 'Black',box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.5, "lines")) + ylab("Years") + 
  xlab("Exports of Goods and Services(% of GDP)") + 
  ggtitle(expression(atop("Exports of Goods and Services(% of GDP) over Year",
                          atop(italic("Years =>  3 = 2001-2004 , 2 = 2005-2008 , 1 = 2009-2012"), ""))))


###############################################
##################  Imports ###################
###############################################
#Extract Data for Goods and Service Imports
data_imports_goods_services <- subset(indicator_pivot_continent[,c(1,2,3,4,711)],
                                      CountryName %in% list_countries &
                                        Year %in% list_year) 
#write.csv(data_imports_goods_services,"data_import_services_goods.csv")
colnames(data_imports_goods_services)
colnames(data_imports_goods_services)[5] <- "Imports_Goods_Services"

#Clean data 
data_imports_goods_services <- subset(data_imports_goods_services, Imports_Goods_Services != 0)

#find the average for four years
average_imports <- function(x,y){
  data <- x
  print(colnames(data))
  years_average <- y
  print(years_average)
  data_subset <- subset(data, data$Year %in% years_average)
  data_subset_avg <- aggregate(data_subset,by = list(data_subset$CountryName),FUN = mean, na.rm = T)
  data_subset_avg <- data_subset_avg[,c(1,5,6)]
  colnames(data_subset_avg)
  colnames(data_subset_avg)[1] <- "CountryName"
  return(data_subset_avg)
  #write.csv(data_subset_avg,"data_subset_avg.csv")
}

#2012-2009
data_imports_2012_2009 <- average_imports(data_imports_goods_services,c(2012,2011,2010,2009))
data_imports_2012_2009[,2] <- 1

#2008-2005
data_imports_2008_2005 <- average_imports(data_imports_goods_services,c(2008,2007,2006,2005))
data_imports_2008_2005[,2] <- 2

#2004- 2001
data_imports_2004_2001 <- average_imports(data_imports_goods_services,c(2004,2003,2002,2001))
data_imports_2004_2001[,2] <- 3

#Combine Datasets
data_imports_goods_services <- rbind(data_imports_2012_2009,data_imports_2008_2005,data_imports_2004_2001)
count(data_imports_goods_services,"Year")
colnames(data_imports_goods_services)

#Determine optimal number of clusters
any(is.na(data_imports_goods_services))
wss <- (nrow(data_imports_goods_services[,c(2,3)])-1)*sum(apply(data_imports_goods_services[,c(2,3)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_imports_goods_services[,c(2,3)],centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Optial Number of clusters are 4

imports_kmeans <- kmeans(data_imports_goods_services[,c(2,3)],4,iter.max = 10,nstart = 10)
imports_kmeans
imports_kmeans$centers
ggplot(data_imports_goods_services,aes(x = Imports_Goods_Services, y = factor(Year)),
       colour = factor(imports_kmeans$cluster)) + geom_point(colour = imports_kmeans$cluster,size = 2)+
  geom_text_repel(aes(label= data_imports_goods_services$CountryName),
                  color = 'Black',box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.5, "lines")) + ylab("Years") + 
  xlab("Imports of Goods and Services(% of GDP)") + 
  ggtitle(expression(atop("Imports of Goods and Services(% of GDP) over Year",
                          atop(italic("Years =>  3 = 2001-2004 , 2 = 2005-2008 , 1 = 2009-2012"), ""))))
  
#Export Data for Plants, Animal, Birds, Mammals
data_threatened <- subset(indicator_pivot_continent[,c(1,2,3,4,407,416,417,418)],Year == 2015) 
write.csv(data_threatened,"data_threatened.csv")  
colnames(data_threatened)  
colnames(data_threatened)[5] <- "Bird_Species_Threatened"  
colnames(data_threatened)[6] <- "Fish_Species_Threatened"  
colnames(data_threatened)[7] <- "Plant_Species_Threatened"  
colnames(data_threatened)[8] <- "Mammal_Species_Threatened"  

#Determine optimal number of clusters
any(is.na(data_threatened))
wss <- (nrow(data_threatened[,c(5,6,7,8)])-1)*sum(apply(data_threatened[,c(5,6,7,8)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_threatened[,c(5,6,7,8)],centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Kmeans
threatened_species <- kmeans(data_threatened[,c(4,5,6,7,8)],4,iter.max = 10,nstart = 10)
threatened_species$centers

#plot CLustering output
plot(data_threatened[,c(5,6,7,8)], col = threatened_species$cluster)
points(threatened_species$centers[,c("Bird_Species_Threatened","Fish_Species_Threatened",
                                     "Plant_Species_Threatened","Mammal_Species_Threatened")],
       col =  1:4, pch = 8, cex = 2)


#Cluster plot of threatend bird species
threatened_species$centers
ggplot(data_threatened,aes(x = Continent, y = Bird_Species_Threatened),
       colour = factor(threatened_species$cluster)) + 
  geom_point(colour = threatened_species$cluster,size = 2) +
  xlab("Continent") + 
  ggtitle("Birds Species Threatened") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))


  
