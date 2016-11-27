setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(randomForest)
library(ROCR)
library(caret)
set.seed(42)

#Filter data for GNI, literacy rate and life expectancy
hdi_index_filter = c(1,2,3)
hdi_index_year   = c(2013,2012,2011,2010,2009,2008,2007,2006,2005)
hdi_index_random_forest <- subset(indicator_hdi_index[,c(1,2,3,4,820,852,1212,1349)],
                       Year %in% hdi_index_year  & hdi_index %in% hdi_index_filter)
colnames(hdi_index_random_forest)[5] <- "GNI_per_capita"
colnames(hdi_index_random_forest)[6] <- "Literacy_Rate"
colnames(hdi_index_random_forest)[7] <- "Life_Expectancy"
colnames(hdi_index_random_forest)
hdi_index_random_forest_1 <- hdi_index_random_forest
#write.csv(hdi_index_random_forest,"main_file.csv")
nrow(hdi_index_random_forest_1)

#Data Clean
hdi_index_random_forest_1$col_sum <- 0
hdi_index_random_forest_1$col_sum <- rowSums(hdi_index_random_forest_1[,5:7])
hdi_index_random_forest_1 <- subset(hdi_index_random_forest_1,col_sum != 0)
nrow(hdi_index_random_forest_1)

#Take average of the values
average_parameter <- function(x){
  colnames(x)
  x <- x[!duplicated(x),]
  x_filter <- x[,c(1,5,6,7,8)] 
  data_average <- aggregate(x_filter,by = list(x_filter$CountryName), FUN = mean, na.rm = T)
  data_average <- data_average[!duplicated(data_average),]
  #write.csv(data_average,"data_average.csv")
  colnames(data_average)
  return(data_average)
}

hdi_index_random_forest <- average_parameter(hdi_index_random_forest_1)

#Split data
id_split <- sample(seq(1,2),size = nrow(hdi_index_random_forest),replace = T, prob = c(0.7,0.3))
train <- hdi_index_random_forest[id_split == 1,]
test  <- hdi_index_random_forest[id_split == 2,]
nrow(hdi_index_random_forest)
nrow(train)
nrow(test)
write.csv(test,"test.csv")
write.csv(train,"train.csv")
colnames(train)


#train dataset using random forest
train_random_forest <- randomForest(train[,c(3,4,5)],
                                    as.factor(train[,6]),ntree = 50)
summary(train_random_forest)
print(train_random_forest)
importance(train_random_forest)
conf  <- train_random_forest$confusion
conf
#predict results
test_random_forest <- predict(train_random_forest,test[,c(3,4,5)])
write.csv(test_random_forest,"test_random_forest.csv")
varImpPlot(train_random_forest,type=2)
