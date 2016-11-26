setwd("E:\\Fall'16\\03 BUAN 6356\\Project\\Codes\\World-Bank-Indicator\\")

#Country Name Correction
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", 
                "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, 
                Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", 
                "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands",
                "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea",
                "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", 
                "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", 
                "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", 
                "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent",
                "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", 
                "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", 
                "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  indicator_pivot[indicator_pivot$CountryName==c,"CountryName"] = correction[c]
}

#Filter dataset for 1976-2014
list_year_filter <- c(1976:2014)  
indicator_pivot_continent   <- subset(indicator_pivot_continent, Year  %in% list_year_filter)
count(indicator_pivot_continent,"Year")
head(indicator_pivot_continent,5)


