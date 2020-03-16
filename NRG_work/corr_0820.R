rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Correlation/corr_0820")
set.seed(1337)

df <- read.csv('socialindex.csv', header=T, stringsAsFactors=FALSE)

df <- subset(df, T_wk %in% c(0,1,2,3))

# aggregate by year and reptype
df_agg <- aggregate(df$SocialIndex, list(df$mvid, df$opg_mbo ,df$T_wk), mean)
names(df_agg) <- c("id", "obo", "t", "social")


# get corr
require(plyr)
func <- function(df_agg)
{
  return(data.frame(COR = cor(df_agg$obo, df_agg$social, use = "pairwise.complete.obs")))
}

ddply(df_agg, .(t), func)

