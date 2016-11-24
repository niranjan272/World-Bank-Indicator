setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)

list_columns <- c(1,2,3,4,547,548,846,850,866,869,892,907,914,928,930,932,
                  949,1063,1073,1076,1085,1088,1094,1098,1110,1158,1218,1230,593)
list_year <- c(2006:2014)
#Filter Data for CPIA ratings
data_cpia <- subset(indicator_pivot_continent[,list_columns])
colnames(data_cpia)
write.csv(data_cpia,"..\\..\\world-development-indicators\\cpia_data.csv")
ncol(data_cpia)

#Data clean
data_cpia$col_sum <- 0
data_cpia$col_sum <- rowSums(data_cpia[,5:26])
write.csv(data_cpia,"data_cpia.csv")
