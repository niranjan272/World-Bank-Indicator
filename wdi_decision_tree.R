setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(cluster)
library(plyr)
library(fpc)
library(rpart)
library(stringr)
library(caTools)
library(e1071)
set.seed(42)

#load hdi index file
countries_hdi_index  <- read.csv("..\\..\\world-development-indicators\\hdi_index.csv")
head(countries_hdi_index,5)
count(countries_hdi_index,"hdi_index")

#Merge indicator data with hdi index
indicator_hdi_index <- merge(indicator_pivot_continent,countries_hdi_index,by = "CountryName", all.x = T)
ncol(indicator_hdi_index)

#Data for decision tree
indicator_hdi_index_filtered <- subset(indicator_hdi_index[,c(1,2,3,4,1237,1240,1349)],Year == 2009)
colnames(indicator_hdi_index_filtered)
#write.csv(indicator_hdi_index_filtered,"..\\..\\world-development-indicators\\indicator_hdi_filtered.csv")
head(indicator_hdi_index_filtered,5)

#Split data
id_split <- sample(seq(1,2),size = nrow(indicator_hdi_index_filtered),replace = T, prob = c(0.7,0.3))
hdi_index_train <- indicator_hdi_index_filtered[id_split == 1,]
hdi_index_test  <- indicator_hdi_index_filtered[id_split == 2,]
nrow(hdi_index_test)
nrow(hdi_index_train)
write.csv(hdi_index_test,"..\\..\\world-development-indicators\\hdi_index_test.csv")
colnames(hdi_index_test)

#deicion tree
head(hdi_index_train,5)
hdi_index_train_tree <- rpart(hdi_index ~ Urban.population....of.total. +
                                Rural.population....of.total.population.,
                               data = hdi_index_train,method = "class")

plot(hdi_index_train_tree,uniform = T,main = "HDI Index")
text(hdi_index_train_tree,use.n = T,all = T,cex = 0.8)
summary(hdi_index_train_tree)
hdi_index_prediction <- predict(hdi_index_train_tree,hdi_index_test[,5:6])
write.csv(hdi_index_prediction,"..\\..\\world-development-indicators\\hdi_index_prediction.csv")


#Naive Bayes
data_naive_bayes <- subset(indicator_hdi_index[,c(1,2,3,4,820,852,1212,1349)],Year == 2008)
colnames(data_naive_bayes)
colnames(data_naive_bayes)[5] <- "GNI_per_capita"
colnames(data_naive_bayes)[6] <- "Literacy_Rate"
colnames(data_naive_bayes)[7] <- "Life_Expectancy"
colnames(data_naive_bayes)
naive_bayes_indicator <- naiveBayes(hdi_index ~ GNI_per_capita+ Literacy_Rate + Life_Expectancy,
                                    data = data_naive_bayes)

#Military Expenditure for South Asian Countries
years_filter  <- c(2010,2009,2008,2007,2006)
indicator_hdi_military <- subset(indicator_hdi_index[,c(1,2,3,4,643)],Year %in% years_filter & 
                                   Continent == "South Asia")
write.csv(indicator_hdi_military,"..\\..\\world-development-indicators\\indicator_hdi_military.csv")
colnames(indicator_hdi_military)[5] <- "Military_Expenditure"
colnames(indicator_hdi_military)
ggplot(indicator_hdi_military,aes(x = Year , y = Military_Expenditure, colour = CountryName)) + 
  geom_line() + 
  scale_y_continuous(breaks = seq(0,4,0.5)) + 
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme_classic() + 
  ggtitle("Military Expenditure - South Asian Countries") +
  ylab("Military Expenditure (% of GDP)")
