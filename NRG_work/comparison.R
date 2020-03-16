rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Segmentation/0304")
set.seed(1337)

library(tidyr)


df <- read.csv('seg_0304.csv', header=T, stringsAsFactors=FALSE)
names(df)
df[,2:23] = df[,2:23] * 2200


# check missing value ======================================================
sapply(df, function(x) sum(is.na(x)))


# alther from wide to long
df_long <- gather(df, item, score, Message.over.Wi.Fi.or.mobile.data:Business.messaging, factor_key=TRUE)


# parametric
anova1 <- aov(score ~ item, data = df_long)
summary(anova1)
TukeyHSD(anova1)
plot(TukeyHSD(anova1))


# non-parametric
kruskal.test(score ~ item, data = df_long)
