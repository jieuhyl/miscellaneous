rm(list = ls())
setwd("C:/Users/Jie.Hu/Desktop/Segmentation/0414")
set.seed(1337)


# read data
df <- read.csv('time_test.csv', header=T, stringsAsFactors=FALSE)
sapply(df, function(x) sum(is.na(x)))
df$Cluster5 <- factor(df$Cluster5)


df_sub <- subset(df, COMPLETION.TIME < 3600)


# ANOVA
# H0: mean weight loss is same for all diets
aov(COMPLETION.TIME ~ Cluster5, df_sub)
anova1 = aov(COMPLETION.TIME ~ Cluster5, df_sub)
summary(anova1)


# Multiple-pairwise comparison 
TukeyHSD(anova1)

plot(TukeyHSD(anova1), las = 1)


# kruskal wallis
kruskal.test(COMPLETION.TIME ~ Cluster5, df_sub)

boxplot(COMPLETION.TIME ~ Cluster5, df_sub)




table(df_sub$Cluster5)
# two samples t test ======================================
df <- read.csv('time_test.csv', header=T, stringsAsFactors=FALSE)
sapply(df, function(x) sum(is.na(x)))
df_sub <- subset(df, COMPLETION.TIME < 3600)
df_sub$Cluster5[df_sub$Cluster5 %in% c(2,3,4,5)] <- 6
df_sub$Cluster5 <- factor(df_sub$Cluster5)

boxplot(COMPLETION.TIME ~ Cluster5, df_sub)


# H0: mean of lung cap of smoker = of non smoker
# two sided
# assume equal var
t.test(COMPLETION.TIME ~ Cluster5, df_sub, mu = 0, alt = 'two.sided', paired = F, conf = 0.95,
       var.eq = T)

# assume non-equal var - Welch test
t.test(COMPLETION.TIME ~ Cluster5, df_sub, mu = 0, alt = 'two.sided', paired = F, conf = 0.95,
       var.eq = F)



# s1/s1 (0.5, 2)
sd(df_sub$COMPLETION.TIME[df_sub$Cluster5 == 1])
sd(df_sub$COMPLETION.TIME[df_sub$Cluster5 == 6])

# Levene test for variance
# H0: equal variances
library(car)
levene.test(COMPLETION.TIME ~ Cluster5, df_sub)
