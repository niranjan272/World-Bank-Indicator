library(cluster)
setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
data_try  <- read.csv("..\\..\\world-development-indicators\\health_world.csv")
head(data_try)
unique(data_try$IndicatorName)
colnames(data_try)
#count(data_try,data_try$IndicatorName)
data_try1 <- reshape(data_try,
                     timevar = "IndicatorName",
                     idvar = c("CountryName","CountryCode","Year"),
                     direction = "wide")
colnames(data_try1)[4] <- "Health_Expenditure"
colnames(data_try1)[5] <- "Life_Expectancy"
head(data_try1,5)
#Data Filter
data_try_filtered <- subset(data_try1,data_try1$CountryCode == "IND")
data_try_filtered_co2 <- merge(data_try_filtered,data_co2_emission_filtered, by = "Year")
colnames(data_try_filtered_co2)[6] <- "CO2_emission"
colnames(data_try_filtered)
#Linear Regression
data_linear_regression <- lm(Life_Expectancy ~ Health_Expenditure + CO2_emission,
                             data = data_try_filtered_co2)
summary(data_linear_regression)
plot(data_try_filtered_co2$Health_Expenditure,data_try_filtered_co2$Life_Expectancy)
abline(data_linear_regression)
plot(data_linear_regression,pch = 16, which = 1)


