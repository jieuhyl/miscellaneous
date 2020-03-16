rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0108")
set.seed(1337)

# read data
df_overall<- read.csv('turf_0108.csv', header=T, stringsAsFactors=FALSE)
df_overall[is.na(df_overall)] <- 0

# check missing value
sapply(df, function(x) sum(is.na(x)))


# overall
df_overall[df_overall >= 2] <- 0

df_overall[df_overall >= 3] <- 0
df_overall[df_overall %in% c(1,2)] <- 1

df_overall[df_overall != 0] <- 1




#=============================================================================
# optimal turf
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

write.csv(turf_4, file="turf_outputs_v2.csv", row.names = FALSE)

# long list ==============================
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


p5 <- c()
for (i in names(sort(p4, decreasing = T)[-1])) {
  dat5_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]), names(sort(p4, decreasing = T)[1]),i)]
  p5[i] <- length(which(rowSums(dat5_sub) != 0))/nrow(df_overall)
}
sort(p5, decreasing = T)

p6 <- c()
for (i in names(sort(p5, decreasing = T)[-1])) {
  dat6_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]), names(sort(p4, decreasing = T)[1]), names(sort(p5, decreasing = T)[1]),i)]
  p6[i] <- length(which(rowSums(dat6_sub) != 0))/nrow(df_overall)
}
sort(p6, decreasing = T)


p7 <- c()
for (i in names(sort(p6, decreasing = T)[-1])) {
  dat7_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]), names(sort(p4, decreasing = T)[1]), names(sort(p5, decreasing = T)[1]), names(sort(p6, decreasing = T)[1]),i)]
  p7[i] <- length(which(rowSums(dat7_sub) != 0))/nrow(df_overall)
}
sort(p7, decreasing = T)

p8 <- c()
for (i in names(sort(p7, decreasing = T)[-1])) {
  dat8_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]), names(sort(p4, decreasing = T)[1]), names(sort(p5, decreasing = T)[1]), names(sort(p6, decreasing = T)[1]), names(sort(p7, decreasing = T)[1]),i)]
  p8[i] <- length(which(rowSums(dat8_sub) != 0))/nrow(df_overall)
}
sort(p8, decreasing = T)

p9 <- c()
for (i in names(sort(p8, decreasing = T)[-1])) {
  dat9_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]), names(sort(p4, decreasing = T)[1]), names(sort(p5, decreasing = T)[1]), names(sort(p6, decreasing = T)[1]), names(sort(p7, decreasing = T)[1]), names(sort(p8, decreasing = T)[1]),i)]
  p9[i] <- length(which(rowSums(dat9_sub) != 0))/nrow(df_overall)
}
sort(p9, decreasing = T)

p10 <- c()
for (i in names(sort(p9, decreasing = T)[-1])) {
  dat10_sub <- df_overall[c(names(sort(p1, decreasing = T)[1]), names(sort(p2, decreasing = T)[1]), names(sort(p3, decreasing = T)[1]), names(sort(p4, decreasing = T)[1]), names(sort(p5, decreasing = T)[1]), names(sort(p6, decreasing = T)[1]), names(sort(p7, decreasing = T)[1]), names(sort(p8, decreasing = T)[1]), names(sort(p9, decreasing = T)[1]),i)]
  p10[i] <- length(which(rowSums(dat10_sub) != 0))/nrow(df_overall)
}
sort(p10, decreasing = T)


turf_10 <- data.frame("Items" = c(names(sort(p1, decreasing = T)[1]),
                                  names(sort(p2, decreasing = T)[1]),
                                  names(sort(p3, decreasing = T)[1]),
                                  names(sort(p4, decreasing = T)[1]),
                                  names(sort(p5, decreasing = T)[1]),
                                  names(sort(p6, decreasing = T)[1]),
                                  names(sort(p7, decreasing = T)[1]),
                                  names(sort(p8, decreasing = T)[1]),
                                  names(sort(p9, decreasing = T)[1]),
                                  names(sort(p10, decreasing = T)[1])),
                      "Reach" = c((sort(p1, decreasing = T)[[1]]),
                                  (sort(p2, decreasing = T)[[1]]),
                                  (sort(p3, decreasing = T)[[1]]),
                                  (sort(p4, decreasing = T)[[1]]),
                                  (sort(p5, decreasing = T)[[1]]),
                                  (sort(p6, decreasing = T)[[1]]),
                                  (sort(p7, decreasing = T)[[1]]),
                                  (sort(p8, decreasing = T)[[1]]),
                                  (sort(p9, decreasing = T)[[1]]),
                                  (sort(p10, decreasing = T)[[1]])))


write.csv(turf_10, file="turf_outputs_long_v2.csv", row.names = FALSE)
