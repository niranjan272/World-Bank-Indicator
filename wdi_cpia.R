setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)
library(plyr)
library(party)
library(rpart)

list_columns <- c(1,2,3,4,866,869,907,914,930,1158,1230,593)
list_year <- c(2006,2013)
list_colnames <- c("Continent","CountryName",'CountryCode','Year','per_female_pri_edu',
                   'gross_enrolment_ratio_pri','per_female_sec_edu','gross_enrolment_ratio_sec',
                   'per_female_ter_edu','female_labour_force','female_population_percentage',
                   'CPIA_gender_equality')

#Filter Data for CPIA ratings
data_cpia <- subset(indicator_pivot_continent[,list_columns],Year %in% list_year)
colnames(data_cpia)
write.csv(data_cpia,"..\\..\\world-development-indicators\\cpia_data.csv")
ncol(data_cpia)

#Data clean
data_cpia$col_sum <- 0
data_cpia$col_sum <- rowSums(data_cpia[,5:10])
#write.csv(data_cpia,"data_cpia.csv")
data_cpia  <- subset(data_cpia,col_sum != 0)
data_cpia  <- subset(data_cpia,CPIA.gender.equality.rating..1.low.to.6.high. != 0)
nrow(data_cpia)
data_cpia <- data_cpia[ !rowSums(data_cpia[,colnames(data_cpia)[(5:10)]]==0)>=1, ]
nrow(data_cpia)
data_cpia <- subset(data_cpia,select = -col_sum)

#change colnames
colnames(data_cpia) <- list_colnames
colnames(data_cpia)

#Decision Tree
data_cpia_decision_tree <- ctree(CPIA_gender_equality ~ per_female_pri_edu +gross_enrolment_ratio_pri +
                                   per_female_sec_edu +gross_enrolment_ratio_sec + 
                                   gross_enrolment_ratio_sec + per_female_ter_edu + female_labour_force +
                                   female_population_percentage + CPIA_gender_equality,
                                 data = data_cpia)
plot(data_cpia_decision_tree)

data_cpia_decision_tree_rpart <- rpart(CPIA_gender_equality ~ per_female_pri_edu +gross_enrolment_ratio_pri +
                                   per_female_sec_edu +gross_enrolment_ratio_sec + 
                                   gross_enrolment_ratio_sec + per_female_ter_edu + female_labour_force +
                                   female_population_percentage + CPIA_gender_equality,data = data_cpia)
printcp(data_cpia_decision_tree_rpart)
plotcp(data_cpia_decision_tree_rpart)
summary(data_cpia_decision_tree_rpart)
plot(data_cpia_decision_tree_rpart,uniform = T)
text(data_cpia_decision_tree_rpart,use.n = T, all = T, cex = 0.8)


data_cpia_decision_tree_naivebayes <- naiveBayes(CPIA_gender_equality ~ per_female_pri_edu +gross_enrolment_ratio_pri +
                                         per_female_sec_edu +gross_enrolment_ratio_sec + 
                                         gross_enrolment_ratio_sec + per_female_ter_edu + female_labour_force +
                                         female_population_percentage + CPIA_gender_equality,data = data_cpia)
summary(data_cpia_decision_tree_naivebayes)
print(data_cpia_decision_tree_naivebayes)
