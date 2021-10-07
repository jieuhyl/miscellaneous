### R script for Correspondence Analysis ###

rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Correspondence Analysis")
set.seed(1337)

install.packages("gplots")
install.packages("FactoMineR")
install.packages("devtools")
devtools::install_github("kassambara/factoextra")


library("FactoMineR")
library("factoextra")
library("gplots")
library("graphics")
library("corrplot")


# read data
df <- read.csv('sample.csv', header=T, stringsAsFactors=FALSE, row.names=1)

# check missing value
sapply(df, function(x) sum(is.na(x)))

# bubble plot
# 1. convert the data as a table
dt <- as.table(as.matrix(df))
# 2. Graph
balloonplot(t(dt), main ="TITLE", xlab ="", ylab="", dotsize =4,
            label = FALSE, show.margins = FALSE, colmar = 4, colsrt = 45)

# mosaic plot
mosaicplot(t(dt), shade = TRUE, las=2,
           main = "TITLE")

# test for association
chisq <- chisq.test(df)
chisq


# Correspondence Analysis
res.ca <- CA(df, graph = FALSE)
print(res.ca)

summary(res.ca)


# screen plot
fviz_screeplot(res.ca) +
  geom_hline(yintercept=100/(ncol(df)-1), linetype=2, color="red")

# biplot
fviz_ca_biplot(res.ca) +
  theme_minimal()

# ROW
row <- get_ca_row(res.ca)

# coordinates
fviz_ca_row(res.ca)
# contribution
fviz_contrib(res.ca, choice = "row", axes = 1:2)
# quality of presentation
corrplot(row$cos2, is.corr=FALSE)
fviz_cos2(res.ca, choice = "row", axes = 1:2)


# COLUMN
col <- get_ca_col(res.ca)

# coordinates
fviz_ca_col(res.ca)
# contribution
fviz_contrib(res.ca, choice = "col", axes = 1:2)
# quality of presentation
corrplot(col$cos2, is.corr=FALSE)
fviz_cos2(res.ca, choice = "col", axes = 1:2)
