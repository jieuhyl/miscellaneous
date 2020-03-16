rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0115")
set.seed(1337)

# read data
df <- read.csv('us02890_weighted.csv', header=T, stringsAsFactors=FALSE)

# 
df_overall <- df[,c(379,338,567:574,737)]
df_overall <- subset(df_overall, QNETWORKSr28 %in% c(1))
df_overall <- df_overall[,-c(1,2,11)]


df_overall[df_overall != 1] = 0

# check missing value
sapply(df_overall, function(x) sum(is.na(x)))


# turf
p1 <- c()
for (i in names(df_overall)) {
  dat1_sub <- df_overall[i]
  p1[i] <- length(dat1_sub[dat1_sub != 0])/nrow(df_overall)
}
sort(p1, decreasing = T)


p2 <- c()
for (i in names(sort(p1, decreasing = T)[-1])) {
  dat2_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), i)]
  p2[i] <- length(which(rowSums(dat2_sub) != 0))/nrow(df_overall)
}
sort(p2, decreasing = T)


p3 <- c()
for (i in names(sort(p2, decreasing = T)[-1])) {
  dat3_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), i)]
  p3[i] <- length(which(rowSums(dat3_sub) != 0))/nrow(df_overall)
}
sort(p3, decreasing = T)


p4 <- c()
for (i in names(sort(p3, decreasing = T)[-1])) {
  dat4_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]),i)]
  p4[i] <- length(which(rowSums(dat4_sub) != 0))/nrow(df_overall)
}
sort(p4, decreasing = T)



turf_4 <- data.frame("Items" = c(names(sort(p1, decreasing = T)[1]),
                                 names(sort(p2, decreasing = T)[1]),
                                 names(sort(p3, decreasing = T)[1]),
                                 names(sort(p4, decreasing = T)[1])),
                     "Reach" = c((sort(p1, decreasing = T)[[1]]),
                                 (sort(p2, decreasing = T)[[1]]),
                                 (sort(p3, decreasing = T)[[1]]),
                                 (sort(p4, decreasing = T)[[1]])))

write.csv(turf_4, file="turf_outputs_TNT.csv", row.names = FALSE)

# weigths
df_overall <- df[,c(379,338,567:574,737)]
df_overall <- subset(df_overall, QPOSTINT1 %in% c(1,2))
wgt = df_overall$WEIGHT
df_overall <- df_overall[,-c(1,2,11)]


df_overall[df_overall != 1] = 0



p1 <- c()
for (i in names(df_overall)) {
  dat1_sub <- df_overall[i]
  dat1_sub$K <- rowSums(dat1_sub)
  dat1_sub$K[dat1_sub$K >=1] = 1
  p1[i] <- sum(dat1_sub$K * wgt)/nrow(df_overall)
}
sort(p1, decreasing = T)


p2 <- c()
for (i in names(sort(p1, decreasing = T)[-1])) {
  dat2_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), i)]
  dat2_sub$K <- rowSums(dat2_sub)
  dat2_sub$K[dat2_sub$K >=1] = 1
  p2[i] <- sum(dat2_sub$K * wgt)/nrow(df_overall)
}
sort(p2, decreasing = T)


p3 <- c()
for (i in names(sort(p2, decreasing = T)[-1])) {
  dat3_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), i)]
  dat3_sub$K <- rowSums(dat3_sub)
  dat3_sub$K[dat3_sub$K >=1] = 1
  p3[i] <- sum(dat3_sub$K * wgt)/nrow(df_overall)
}
sort(p3, decreasing = T)


p4 <- c()
for (i in names(sort(p3, decreasing = T)[-1])) {
  dat4_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]),i)]
  dat4_sub$K <- rowSums(dat4_sub)
  dat4_sub$K[dat4_sub$K >=1] = 1
  p4[i] <- sum(dat4_sub$K * wgt)/nrow(df_overall)
}
sort(p4, decreasing = T)



turf_4 <- data.frame("Items" = c(names(sort(p1, decreasing = T)[1]),
                                 names(sort(p2, decreasing = T)[1]),
                                 names(sort(p3, decreasing = T)[1]),
                                 names(sort(p4, decreasing = T)[1])),
                     "Reach" = c((sort(p1, decreasing = T)[[1]]),
                                 (sort(p2, decreasing = T)[[1]]),
                                 (sort(p3, decreasing = T)[[1]]),
                                 (sort(p4, decreasing = T)[[1]])))

write.csv(turf_4, file="turf_outputs_QPOSTINT1_wgt.csv", row.names = FALSE)


# turf
p1 <- c()
for (i in names(df_overall)) {
  dat1_sub <- df_overall[i]
  p1[i] <- length(dat1_sub[dat1_sub != 0])/nrow(df_overall)
}
sort(p1, decreasing = T)


p2 <- c()
for (i in names(sort(p1, decreasing = T)[-1])) {
  dat2_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), i)]
  p2[i] <- length(which(rowSums(dat2_sub) != 0))/nrow(df_overall)
}
sort(p2, decreasing = T)


p3 <- c()
for (i in names(sort(p2, decreasing = T)[-1])) {
  dat3_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), i)]
  p3[i] <- length(which(rowSums(dat3_sub) != 0))/nrow(df_overall)
}
sort(p3, decreasing = T)


p4 <- c()
for (i in names(sort(p3, decreasing = T)[-1])) {
  dat4_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]),i)]
  p4[i] <- length(which(rowSums(dat4_sub) != 0))/nrow(df_overall)
}
sort(p4, decreasing = T)



turf_4 <- data.frame("Items" = c(names(sort(p1, decreasing = T)[1]),
                                 names(sort(p2, decreasing = T)[1]),
                                 names(sort(p3, decreasing = T)[1]),
                                 names(sort(p4, decreasing = T)[1])),
                     "Reach" = c((sort(p1, decreasing = T)[[1]]),
                                 (sort(p2, decreasing = T)[[1]]),
                                 (sort(p3, decreasing = T)[[1]]),
                                 (sort(p4, decreasing = T)[[1]])))

write.csv(turf_4, file="turf_outputs_QPOSTINT1_NOwgt.csv", row.names = FALSE)
