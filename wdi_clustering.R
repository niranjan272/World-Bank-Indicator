setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)

#Filter for G20 Countries
list_countries <- c('Australia','Canada','Saudi Arabia','United States','India','Russia',
                    'South Africa','Turkey','Argentina','Brazil','Mexico','France','Germany',
                    'Italy','United Kingdom','China','Indonesia','Japan','South Korea')

#Extract Data for Goods and Service Exports
data_exports_goods_services <- subset(indicator_pivot_continent[,c(1,2,3,4,688)],Year == 2008) 
write.csv(data_exports_goods_services,"data_export_services_goods.csv")
colnames(data_exports_goods_services)
colnames(data_exports_goods_services)[5] <- "Exports_Goods_Services"

#Clean data 
data_exports_goods_services <- subset(data_exports_goods_services, Exports_Goods_Services != 0)
#write.csv(data_exports_goods_services,"data_export_services_goods_clean.csv")

#Determine optimal number of clusters
any(is.na(data_exports_goods_services))
wss <- (nrow(data_exports_goods_services[,c(4,5)])-1)*sum(apply(data_exports_goods_services[,c(4,5)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_exports_goods_services[,c(4,5)],centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Optial Number of clusters are 4
#Apply kmeans 
exports_kmeans <- kmeans(data_exports_goods_services[,c(4,5)],4,iter.max = 10,nstart = 10)
ggplot(data_exports_goods_services,aes(x = Exports_Goods_Services, y= CountryName),
       colour = exports_kmeans$cluster) +
  geom_point()
clusplot(data_exports_goods_services[,c(4,5)],exports_kmeans$cluster,colour = T,shade = T)  
