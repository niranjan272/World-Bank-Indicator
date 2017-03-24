library(arules) # MBA
library(arulesViz)

set.seed(42)
setwd("C:/DropboxLink/Data/_Teaching/UTD/Courses/MIS6356/Datasets/Demo")

assocs = read.transactions("assocs.csv", format = "single", sep = ",", cols = c("CUSTOMER", "PRODUCT"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.02, conf = 0.50, minlen = 1, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Limit output to 2 digits
options(digits=2)

# Show rules and summary
inspect(rules[1:50])
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules[1:50])
summary(rules)

# Plot rules
plot(rules,method="graph",interactive=FALSE,shading=NA)

#################################################################################################

assocs = read.transactions("MarketBasketAnalysisData_Transactional.csv", format = "single", sep = ",", cols = c("TID", "Product"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.01, conf = 0.25, minlen = 1, maxlen = 2))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules[1:50])
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules[1:50])
summary(rules)

# Plot rules
plot(rules,method="graph",interactive=FALSE,shading=NA)

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.01, conf = 0.25, minlen = 1, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules[1:50])
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules[1:50])
summary(rules)

# Plot rules
plot(rules,method="graph",interactive=FALSE,shading=NA)

#################################################################################################

assocs = read.transactions("Titanic.csv", format = "single", sep = ",", cols = c("PassengerID", "Characteristic"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.02, conf = 0.50, minlen = 2, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules)
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules)
summary(rules)

# Plot rules
plot(rules, interactive = TRUE)
plot(rules,method="graph",interactive=TRUE,shading=NA)
plot(rules,method="graph",interactive=TRUE)


#################################################################################################

assocs = read.transactions("Titanic_AgeClass.csv", format = "single", sep = ",", cols = c("PassengerID", "Characteristic"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.02, conf = 0.50, minlen = 1, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules)
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules)
summary(rules)

# Plot rules
plot(rules, interactive = TRUE)
plot(rules,method="graph",interactive=TRUE,shading=NA)

#################################################################################################

assocs = read.transactions("Words.csv", format = "single", sep = ",", cols = c("DocID", "Word"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.02, conf = 0.50, minlen = 1, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules)
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules)
summary(rules)

# Plot rules
plot(rules, interactive = TRUE)
plot(rules,method="graph",interactive=FALSE,shading=NA)

#################################################################################################

assocs = read.transactions("SuperstoreSales2.csv", format = "single", sep = ",", cols = c("OrderID", "ProductName"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.00025, conf = 0.01, minlen = 1, maxlen = 2))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules)
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules)
summary(rules)

assocs = read.transactions("SuperstoreSales2.csv", format = "single", sep = ",", cols = c("OrderID", "ProductSubcategory"), rm.duplicates = FALSE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.00025, conf = 0.01, minlen = 1, maxlen = 2))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Show rules and summary
inspect(rules)
summary(rules)

# Remove duplicate rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules)
summary(rules)



