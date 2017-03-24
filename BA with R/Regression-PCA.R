library(readxl) #Read excel files
library(Hmisc) #Contents and Describe
library(leaps) #Variable selection

set.seed(42)
setwd("C:/DropboxLink/Data/_Teaching/UTD/Courses/MIS6356/Datasets/Demo")

#Read the data
myraw <- read_excel("myraw.xls")
myraw$TARGET_B <- NULL  #Binary
myraw$IDCODE <- NULL  #Identifier
myraw$PETS <- NULL  #Only Y coded
myraw$PCOWNERS <- NULL #Only Y coded
myraw$GENDER <- NULL #linear relationship with MALE
head(myraw)

#Explore the data
contents(myraw)
summary(myraw)
describe(myraw)

#look for outliers
outlier_values <- boxplot.stats(myraw$AGE)$out  # outlier values.
outlier_values <- sort(outlier_values)
boxplot(myraw$AGE, main="AGE", boxwex=1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.8)

#Perform linear regression
model <- lm(TARGET_D ~ INCOME + AVGGIFT + LASTT + FIRSTT + CARDGIFT, data=myraw)
summary(model)

#Remove insignificant CARDGIFT
model <- lm(TARGET_D ~ INCOME + AVGGIFT + LASTT + FIRSTT, data=myraw)
summary(model)

#Variable selection
leaps=regsubsets(TARGET_D ~ ., data = myraw, nbest = 10)
plot(leaps, scale="adjr2")
#Suggests best model is HOMEOWNR, INCOME, CARDPROM, NUMPROM, AVGGIFT, LASTT, FIRSTT

#Perform linear regression with selected variables
model <- lm(TARGET_D ~ HOMEOWNR + INCOME + CARDPROM + NUMPROM + AVGGIFT + LASTT + FIRSTT, data=myraw)
summary(model)

#Remove insignificant HOMEOWNRU
model <- lm(TARGET_D ~ INCOME + CARDPROM + NUMPROM + AVGGIFT + LASTT + FIRSTT, data=myraw)
summary(model)

#Remove insignificant FIRSTT
model <- lm(TARGET_D ~ INCOME + CARDPROM + NUMPROM + AVGGIFT + LASTT, data=myraw)
summary(model)

#Remove insignificant NUMPROM
model <- lm(TARGET_D ~ INCOME + CARDPROM + AVGGIFT + LASTT, data=myraw)
summary(model)

#Remove insignificant LASTT
model <- lm(TARGET_D ~ INCOME + CARDPROM + AVGGIFT, data=myraw)
summary(model)

#Perform stepwise regression
myrawnomissing <- na.omit(myraw) #must remove missing values
null <- lm(TARGET_D ~ 1, data = myrawnomissing)
null
full <- lm(TARGET_D ~ ., data = myrawnomissing)
full
selectedmodel <- step(null, scope=list(lower=null, upper=full), direction="forward")
selectedmodel
model <- lm(TARGET_D ~ AVGGIFT + FIRSTT + INCOME, data = myrawnomissing) #best model
summary(model)

#Perform PCA
pcadata <- myrawnomissing
pcadata$HOMEOWNR <- NULL
#pcadata$MALE <- NULL
#pcadata$MALEMILI <- NULL
pcadata$TARGET_D <- NULL
head(pcadata)
pca <- princomp(pcadata, cor = TRUE)
summary(pca) # print variance accounted for 
pca$loadings # pca loadings 
pca$scores # the principal components
plot(pca,type="lines") # scree plot 

myrawnomissing$pc1 <- pca$scores[, 1]
myrawnomissing$pc2 <- pca$scores[, 2]
myrawnomissing$pc3 <- pca$scores[, 3]
myrawnomissing$pc4 <- pca$scores[, 4]
myrawnomissing$pc5 <- pca$scores[, 5]
myrawnomissing$pc6 <- pca$scores[, 6]
head(myrawnomissing)
model <- lm(TARGET_D ~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6, data=myrawnomissing)
summary(model)

model <- lm(TARGET_D ~ pc2 + pc3 + pc4 + pc5 + pc6, data=myrawnomissing)
summary(model)

model <- lm(TARGET_D ~ pc3 + pc4 + pc5 + pc6, data=myrawnomissing)
summary(model)

model <- lm(TARGET_D ~ pc3 + pc5 + pc6, data=myrawnomissing)
summary(model)

