setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(stringr)
library(reshape2)

#Filter data for life expectancy and gases emission
list_columns <- c(1,2,3,4,382,391,396,402,403,406,410,1035,1211,1212,1213,1229)
list_year <- c(2000:2011)
data_life_expectancy_emission <- subset(indicator_pivot_continent[,list_columns],Year %in% list_year)
colnames(data_life_expectancy_emission)
#write.csv(data_life_expectancy_emission,"data+life_expectancy_emission.csv")


#Filter Data for emission, life expectancy, immunization
list_columns <- c(1,2,3,4,382,980,981,1035,1212,1229)
data_life_expectancy_emission <- subset(indicator_pivot_continent[,list_columns],Year %in% list_year)
colnames(data_life_expectancy_emission) <- c('Continent','CountryName','CountryCode','Year','CO2_Emission',
                                             'Immunization_DPT','Immunization_Measles','Health_Expenditure',
                                             'Life_expectancy_at_birth','Population_Total')
colnames(data_life_expectancy_emission)
nrow(data_life_expectancy_emission)

#Data Clean
any(is.na(data_life_expectancy_emission))
data_life_expectancy_emission$col_sum <- 0
list_sum_col <- c(5,6,7,9,10)
data_life_expectancy_emission$col_sum <- rowSums(data_life_expectancy_emission[,list_sum_col])
data_life_expectancy_emission <- subset(data_life_expectancy_emission,col_sum != 0)
data_life_expectancy_emission <- data_life_expectancy_emission[ !rowSums(data_life_expectancy_emission[,colnames(data_life_expectancy_emission)[(list_sum_col)]]==0)>=1, ]
nrow(data_life_expectancy_emission)


#Function for linear regression 
life_expectancy_regression <- function(data_set, country_name){
  data_regression <- subset(data_set,data_set$CountryName  == country_name)
  data_correlation <- cor(data_regression[,c(5:10)])
  #print(data_correlation)
  data_life_expectancy_regression <- lm(Life_expectancy_at_birth ~ CO2_Emission + Immunization_DPT +
                                          Immunization_Measles + Health_Expenditure,
                                        data = data_regression)
  print(summary(data_life_expectancy_regression))
  data_significant <-summary(data_life_expectancy_regression)$coefficients[,4] <= 0.05
  data_significant$r_squared <- 0
  data_significant$CountryName  <- NA
  data_significant$CountryName  <- country_name
  data_significant$r_squared <- summary(data_life_expectancy_regression)$r.squared
  
  #plot(data_life_expectancy_regression)
  #abline(data_life_expectancy_regression)
  data_significant <- data.frame(data_significant)
  return(data_significant)
}

#List of countries
list_countries <- c('Saudi Arabia','United States','India','Russia','South Africa','Turkey','Argentina',
                    'Brazil','Mexico','Germany','United Kingdom','China',
                    'Nigeria','Nepal','Bangladesh','Haiti')

for(country_name_select in list_countries){
    countryname  = paste("'",country_name_select,"'")
    print(countryname)
    data_regression <- life_expectancy_regression(data_life_expectancy_emission,country_name_select)
    regression_data_all <- rbind(regression_data_all,data_regression)
}

head(regression_data_all,50)
regression_data_all <- subset(regression_data_all,select = - X.Intercept.)
write.csv(regression_data_all,"regression_data_all.csv")
regression_data_all <- regression_data_all[!duplicated(regression_data_all),]
count(regression_data_all,"CountryName")

#India
regression_data_all <- life_expectancy_regression(data_life_expectancy_emission,"India")

#reshape data
regression_data_all_melt <- melt(regression_data_all,id = c("CountryName","r_squared"))
head(regression_data_all_melt,5)
colnames(regression_data_all_melt)[2] <- "r_squared"
colnames(regression_data_all_melt)[3] <- "Parameter"
colnames(regression_data_all_melt)[4] <- "Value"

#Filter data and plot countrywise distribution of parameters 
regression_data_all_melt_filter <- subset(regression_data_all_melt,Value == "TRUE")
head(regression_data_all_melt_filter,30)
ggplot(regression_data_all_melt_filter,aes(x = CountryName ,y = Parameter, colour = "r_squared")) + 
  geom_point(size = 3) +
  geom_text_repel(aes(label= paste(round(regression_data_all_melt_filter$r_squared,4)*100,"%")),
                  color = 'Black',box.padding = unit(0.35, "lines"),point.padding = unit(0.5, "lines")) + 
  ggtitle("Life Expectancy,Significant Parameters - CountryWise ")
  
  
