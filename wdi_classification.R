setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)
head(indicator_pivot_continent,5)
child_employement <- indicator_pivot_continent[,c(1,2,3,4,1189,1062,1109,1103,1093,1126,1127)]
colnames(child_employement)
write.csv(child_employement,"..\\..\\child_employement.csv")
aggregate_child_employement <- subset(aggregate(child_employement$Children.in.employment..total....of.children.ages.7.14.~ Continent + Year,
                                                child_employement,mean),Year  >= 1994 & Year  <= 2013 )

group_columns =  c("Continent","Year")
data_columns  =  colnames(child_employement[,5:11])
aggregate_child_employement <- subset(ddply(child_employement,c("Continent","Year"),function(x) colMeans(x[data_columns])),
                                      Year  >= 1994 & Year  <= 2013)

colnames(aggregate_child_employement)
child_employement_clustering <- kmeans(aggregate_child_employement[,2:9],4,nstart = 20)
clusplot(aggregate_child_employement[,2:9],child_employement_clustering$cluster,colour = T, shade = T )

write.csv(aggregate_child_employement,"aggregate_child_employement.csv")
count(aggregate_child_employement,c("Continent","Year"))
colnames(aggregate_child_employement)
ggplot(aggregate_child_employement,aes(x =Continent ,y = Year)) + geom_point()
