setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(plyr)
library(party)
library(rpart)
library(rattle)

#Select data for consumer price index and inflation consumer price
list_columns <- c(1,2,3,4,467,468)
data_financial_parameters <- subset(indicator_pivot_continent[,list_columns], Year >= 2010)
write.csv(data_financial_parameters,"data_financial_parameters.csv")
colnames(data_financial_parameters)
colnames(data_financial_parameters)[5] <- "Consumer_Price_Index"
colnames(data_financial_parameters)[6] <- "Inflation_Consumer_Price"

#Clean data
data_financial_parameters$col_sum <- 0
data_financial_parameters$col_sum <- rowSums(data_financial_parameters[,5:6])
data_financial_parameters <- subset(data_financial_parameters,col_sum != 0)
data_financial_parameters <- data_financial_parameters[ !rowSums(data_financial_parameters[,colnames(data_financial_parameters)[(5:6)]]==0)>=1, ]
nrow(data_financial_parameters)

data_financial_rpart <- rpart(Inflation_Consumer_Price ~ Consumer_Price_Index,
                              data = data_financial_parameters)
printcp(data_financial_rpart)
plot(data_financial_rpart)
text(data_financial_rpart,use.n = T, all = T,cex = 0.8)

fancyRpartPlot(data_financial_rpart$finalModel)

