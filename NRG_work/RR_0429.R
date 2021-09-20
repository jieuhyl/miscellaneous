rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/RR/new_9")
set.seed(1337)


raw <- read.csv('RR_Leverage_Redemption.csv', header=T)


# subset
#raw <- subset(raw, QUAD35 %in% c(2))
#raw <- raw[-36]

# all titles
comps <- names(table(raw$Movie))
df <- subset(raw, Movie %in% comps)
#df[is.na(df)] <- 0

a <- 4
b <- (dim(df)[2] - 3)/2 + 3
c <- b + 1
d <- dim(df)[2]


summary(df)
#hist(df$Indifferent.Certainty, breaks = 100)
df[,a:b][df[,a:b] >= 60000] <- 60000

# total certainty and importance
total_certainty <- colMeans(df[,a:b], na.rm = T)
total_importance <- colMeans(df[,c:d], na.rm = T)

# by segments
sub_certainty <- aggregate(df[,a:b], list(df$Movie), mean, na.rm = T)
sub_importance <- aggregate(df[,c:d], list(df$Movie), mean, na.rm = T)

# get 100 index 
sub_certainty <- total_certainty / t(sub_certainty[, -1]) * 100
sub_importance <- t(sub_importance[, -1]) / total_importance  * 100

# average out
sub_index <- round((sub_certainty + sub_importance) / 2, 0)
sub_index <- round(sub_index / rowMeans(sub_index) * 100, 0)

# write csv
colnames(sub_index) <- comps
Movie <- rownames(sub_index)
Movie <- gsub(".Certainty", "" ,Movie)

sub_index <- cbind(Movie, sub_index)
rownames(sub_index) <- 1:nrow(sub_index)
sub_index <- as.data.frame(sub_index)

write.csv(sub_index, file='RR_output_Leverage_Redemption_v2.csv', row.names = FALSE)

