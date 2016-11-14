setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
data_corr <- read.csv("..\\..\\world-development-indicators\\Indicators_Pivot_corr.csv")
nrow(data_corr)
ncol(data_corr)
for(i in nrow(data_corr)){
  for (j in ncol(data_corr)){
    if(data_corr[i,j] >= 0.7 && data_corr[i,j] <= 0.9999)
  }
}
