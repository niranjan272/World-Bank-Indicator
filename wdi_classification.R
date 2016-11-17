setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
head(indicator_pivot_continent,5)
child_employement <- indicator_pivot_continent[,c(1,2,3,4,1189,1062,1109,1103,1093,1126,1127)]
colnames(child_employement)
write.csv(child_employement,"..\\..\\child_employement.csv")
aggregate_child_employement <- subset(aggregate(child_employement$Children.in.employment..total....of.children.ages.7.14.~ Continent + Year,
                                                child_employement,mean),Year  >= 1994 & Year  <= 2013 )

group_columns =  c("Continent","Year")
data_columsn  =  colnames(child_employement)[5,6,7,8,9,10,11]
aggregate_child_employement <- ddply(child_employement,c("Continent","Year"),function(x) colMeans(child_employement[c(5,6,7,8,9,10,11)]))

write.csv(aggregate_child_employement,"aggregate_child_employement.csv")
count(aggregate_child_employement,c("Continent","Year"))
colnames(aggregate_child_employement)
ggplot(aggregate_child_employement,aes(x =`child_employement$Children.in.employment..total....of.children.ages.7.14.`,y = Year,
                                       colour = Continent)) + geom_point()
