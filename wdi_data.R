setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
require(plyr)
library(cluster)
library(reshape2)
library(arules)

data_education_poverty <- rbind(data_education,data_poverty)
colnames(data_education_poverty)
data_education_poverty_filtered <- subset(data_education_poverty,
                      data_education_poverty$Year >1975 & data_education_poverty$Year <2015)

#swrite.csv(data_education_poverty_filtered,"..\\data_education_poverty_filtered.csv")


#CO2 Emission 
data_co2_emission <- subset(data_indicators_series,data_indicators_series$IndicatorCode == "EN.ATM.CO2E.KT" &
                              data_indicators_series$CountryCode == "IND" & data_indicators_series$Year >1999)

data_co2_emission_filtered <- data_co2_emission[c(5,6)]

min(data_co2_emission$Year)
head(data_co2_emission_filtered)




data_usa_education_poverty <- read.csv("..\\world-development-indicators\\usa_education_poverty.csv")
head(data_usa_education_poverty,5)
data_usa_education_poverty_lr <- data_usa_education_poverty[c(3,13,17,22)]
colnames(data_usa_education_poverty_lr)
head(data_usa_education_poverty_lr
     )
usa_data_regression <- lm(Adjusted.net.enrolment.rate..primary..both.sexes.... ~ .,data= data_usa_education_poverty_lr)
summary(usa_data_regression)


daya_edu_poverty_corr <- cor(data_education_poverty_filtered, use = "complete.obs")
