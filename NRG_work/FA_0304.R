rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Segmentation/0304")
set.seed(1337)

df <- read.csv('seg_0304.csv', header=T, stringsAsFactors=FALSE)
names(df)
df_B1 <- df[, -c(1,21)]



# check missing value ======================================================
sapply(df_B1, function(x) sum(is.na(x)))


#principal component analysis, with normalization
prin_comp <- prcomp(df_B1, scale. = T)

# check the loadings
head(prin_comp$rotation[, 1:5])

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 5 components greater than 1
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

cumsum(prop_varex[1:7])

#scree plot
plot(prop_varex, xlab = "Principal Components",
     ylab = "Variance Explained",
     main = 'scree plot',
     type = "b")


#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Components",
     ylab = "Variance Explained",
     main = 'cumulative scree plot',
     type = "b")


# Exploratory Factor Analysis
fac_ana <- factanal(df_B1, factors = 8, rotation = "varimax")
fac_ana
