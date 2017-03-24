#######################################################################################
#
#  This script demonstrates how principal components are generated
#
#######################################################################################

# Build initial dataset
x1 <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
x2 <- c(2.4, 0.7, 2.9, 2.2, 3, 2.7, 1.6, 1.1, 1.6, 0.9)
df = data.frame(x1, x2)
df

# Create the variance/covariance matrix
covar <- cov(df)
covar

# Calculate eigenvalues and eigenvectors
e <- eigen(covar)
e

trace = sum(e$values)
for(i in 1:NROW(e$values))
{
  print(paste("Eigenvalue ", i, " accounts for ", round((e$values[i]/trace)*100, digits = 2), "% of the variation"))
}

# Retain the first principal component and calculate its values
x1bar <- mean(df$x1)
x2bar <- mean(df$x2)
df$x1minusx1bar <- df$x1-x1bar
df$x2minusx2bar <- df$x2-x2bar
df

# Multiply original dimensions minus their means by the eigenvector associated with the first principal component
df$pc1 <- df$x1minusx1bar*e$vectors[1, 1] + df$x2minusx2bar*e$vectors[2, 1]
df

#######################################################################################

# Perform the same operation, this time using existing R libraries

# Build initial dataset
df2 <- df[c("x1", "x2")]
df2

# Run PCA
pca <- princomp(df2, cor=FALSE)  # false uses cov matrix, true uses correlation matrix
summary(pca) # print variance accounted for 
pca$loadings # pca loadings 
pca$scores # the principal components
plot(pca,type="lines") # scree plot 

# Merge PCA scores into mydata
df2$pc1 <- pca$scores[, 1]
df2



