setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")
library(ggplot2)

list_columns <- c(1,2,3,4,c(586:606))
list_year <- c(2006:2014)
#Filter Data for CPIA ratings
data_cpia <- subset(indicator_pivot_continent[,list_columns], Year %in% list_year)

#Column name change
colnames(data_cpia) <- c("Continent","ConutryName","CountryCode","Year",
                         'CPIA_business_regulatory_environment_rating','CPIA_debt_policy_rating',
                         'CPIA_economic_management_cluster_average',
                         'CPIA_policy_and_institutions_for_environmental_sustainability_rating',
                         'CPIA_quality_of_budgetary_and_financial_management_rating',
                         'CPIA_financial_sector_rating','CPIA_fiscal_policy_rating',
                         'CPIA_gender_equality_rating','CPIA_building_human_resources_rating',
                         'IDA_resource_allocation_index','CPIA_macroeconomic_management_rating',
                         'CPIA_quality_of_public_administration_rating',
                         'CPIA_equity_of_public_resource_use_rating',
                         'CPIA_property_rights_and_rule_based_governance_rating',
                         'CPIA_social_protection_rating',
                         'CPIA_public_sector_management_and_institutions_cluster_average',
                         'CPIA_efficiency_of_revenue_mobilization_rating',
                         'CPIA_policies_for_social_inclusion_equity_cluster_average',
                         'CPIA_structural_policies_cluster_average','CPIA_trade_rating','CPIA_transparency'
)
colnames(data_cpia)
write.csv(data_cpia,"..\\..\\world-development-indicators\\cpia_data.csv")
ncol(data_cpia)

#Data clean
data_cpia$col_sum <- 0
data_cpia$col_sum <- rowSums(data_cpia[,5:26])
write.csv(data_cpia,"data_cpia.csv")
