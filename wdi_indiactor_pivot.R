setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(reshape2)
#Reshapre indicator data
indicator_pivot <- reshape(data_read,
                           timevar = "IndicatorName",
                           idvar = c("CountryName","CountryCode","Year"),
                           direction = "wide")

head(indicator_pivot,5)