setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(stringr)
library(plyr)
#Read Indicators Data
data_read <- read.csv("..\\..\\world-development-indicators\\Indicators.csv")
head(data_read)
str(data_read)

#Read Series
data_series <- read.csv("..\\..\\world-development-indicators\\Series.csv")
str(data_series)

#Filter Data Series 
data_series <- data_series[1:2]
colnames(data_series)[1] <- "IndicatorCode"
head(data_series,5)
#introduce New Column
data_series$Topic_Main <-NA
data_series$Topic_Main <- substr(data_series$Topic,1,regexpr(':',data_series$Topic)-1)
head(data_series,5)

#Combine Indicator and Series data
data_indicators_series <- merge(data_read,data_series,by = "IndicatorCode")
head(data_indicators_series,5)
count(data_indicators_series,"Topic_Main")
unique(data_indicators_series$Topic_Main)
#Seggregate Data as per topic
#Environment
data_environment <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Environment')
head(data_environment,5)

#Economic Policy and Debt
data_economic <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Economic Policy & Debt')
head(data_economic,5)

#Infrastructure
data_infrastructure <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Infrastructure')
head(data_infrastructure,5)

#Financial Sector
data_financial_sector <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Financial Sector')
head(data_financial_sector,5)

#Public Sector
data_public_sector <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Public Sector')
head(data_public_sector,5)

#Private Sector
data_private_sector <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Private Sector & Trade')
head(data_private_sector,5)

#Social Protection
data_social <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Social Protection & Labor')
head(data_social,5)

#Education
data_education <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Education')
head(data_education,5)

#Health
data_health <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Health')
head(data_health,5)

#Poverty
data_poverty <- subset(data_indicators_series,data_indicators_series$Topic_Main == 'Poverty')
head(data_poverty,5)

