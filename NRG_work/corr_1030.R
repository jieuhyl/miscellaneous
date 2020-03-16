rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Correlation/corr_1030")
set.seed(1337)

require(plyr)
require(ggplot2)


# read data
df <- read.csv('df1.csv', header=T, stringsAsFactors=FALSE)
df$TT_GROSS <- as.numeric(df$TT_GROSS)
df$TOTAL <- as.numeric(df$TOTAL)
df$MULTIPLE <- df$TT_GROSS/df$WKNDGRS
summary(df)

# Excellent
df1 = subset(df, NDX == 'RAT' & OPINION == 'EXCELLENT')
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX == 'RAT' & OPINION == 'EXCELLENT' & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# top 2 box
df1 = subset(df, NDX == 'RAT' & OPINION %in% c('EXCELLENT', 'VERY GOOD'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX == 'RAT' & OPINION %in% c('EXCELLENT', 'VERY GOOD') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# index rating
df1 = subset(df, NDX == 'RAT' & OPINION %in% c('INDEX RATING SCORE'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX == 'RAT' & OPINION %in% c('INDEX RATING SCORE') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# def rec
df1 = subset(df, NDX == 'REC' & OPINION %in% c('DEF. REC.'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX == 'REC' & OPINION %in% c('DEF. REC.') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# bettert than expected
df1 = subset(df, NDX == 'EXP' & OPINION %in% c('BETTER THAN EXPCTD'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX == 'EXP' & OPINION %in% c('BETTER THAN EXPCTD') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# Lead Performance 
df1 = subset(df, NDX %in% c('A01', 'C01', 'F01', 'G01', 'M01', 'V01'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX %in% c('A01', 'C01', 'F01', 'G01', 'M01', 'V01') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# Supporting Performance
df1 = subset(df, NDX %in% c('A02', 'A03', 'A04', 'A05', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 'C10', 
                            'C11', 'C12', 'C13', 'C14', 'C15', 'F02', 'F03', 'F04', 'F05', 'F06', 'F07', 'F08', 
                            'F09', 'F10', 'M02', 'M03', 'M04', 'M05', 'M06', 'M07', 'M08', 'M09', 'M10', 'V02', 
                            'V03', 'V04', 'V05', 'V06', 'V07', 'V08', 'V09', 'V10', 'V11', 'V12'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, NDX %in% c('A02', 'A03', 'A04', 'A05', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 'C10', 
                            'C11', 'C12', 'C13', 'C14', 'C15', 'F02', 'F03', 'F04', 'F05', 'F06', 'F07', 'F08', 
                            'F09', 'F10', 'M02', 'M03', 'M04', 'M05', 'M06', 'M07', 'M08', 'M09', 'M10', 'V02', 
                            'V03', 'V04', 'V05', 'V06', 'V07', 'V08', 'V09', 'V10', 'V11', 'V12') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# humor
df1 = subset(df, OPINION %in% c('HUMOR','COMEDY'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('HUMOR','COMEDY') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# adventure
df1 = subset(df, OPINION %in% c('ADVENTURE'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('ADVENTURE') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# settings
df1 = subset(df, OPINION %in% c('SETTINGS'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('SETTINGS') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# action
df1 = subset(df, OPINION %in% c('ACTION'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('ACTION') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# story
df1 = subset(df, OPINION %in% c('STORY'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('STORY') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# ending
df1 = subset(df, OPINION %in% c('ENDING'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('ENDING') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# music
df1 = subset(df, OPINION %in% c('MUSIC'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('MUSIC') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# begin
df1 = subset(df, OPINION %in% c('BEGINNING'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('BEGINNING') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# drama
df1 = subset(df, OPINION %in% c('DRAMA'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('DRAMA') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# pace
df1 = subset(df, OPINION %in% c('PACE'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('PACE') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# romance
df1 = subset(df, OPINION %in% c('ROMANCE'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('ROMANCE') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

# suspense
df1 = subset(df, OPINION %in% c('SUSPENSE'))
df_agg <- aggregate(df1[, c(1,3,4,7,8,9,10)], list(df1$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")

df2 = subset(df, OPINION %in% c('SUSPENSE') & !(GENRE %in% c('ACTION COMEDY', 'HUMN INTST CMDY', 'ROMANTIC COMEDY', 'ZANY COMEDY')))
df_agg <- aggregate(df2[, c(1,3,4,7,8,9,10)], list(df2$MVNAME), mean)
cor(df_agg$TOTAL, df_agg[-c(1,2,5)], use = "pairwise.complete.obs")


# eg 
# read data
df <- read.csv('df_corr_eg.csv', header=T, stringsAsFactors=FALSE)

cor(df$WKND_OPENING_BOX_OFFICE, df[2:4], use = "pairwise.complete.obs")
df$new = df$SCREENS/df$Total.Awareness
cor(df$WKND_OPENING_BOX_OFFICE, df[2:5], use = "pairwise.complete.obs")


ggplot(df, aes(x=df$new, y=df$WKND_OPENING_BOX_OFFICE)) + 
  geom_point()

dat <- data.frame(points.x = c(1:10), points.y = c(1:10),
                  lines.x = c(10:1), lines.y = c(1:10))

ggplot(dat, aes(points.x, points.y)) + geom_point() +
  geom_line(aes(lines.x,lines.y))

ggplot(df, aes(x=df$new, df$WKND_OPENING_BOX_OFFICE)) + geom_point() +
  geom_line(aes(df$Total.Awareness, df$WKND_OPENING_BOX_OFFICE))



