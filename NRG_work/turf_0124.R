rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0124")
set.seed(1337)

# read data
df <- read.csv('turf_0124.csv', header=T, stringsAsFactors=FALSE)
#df <- subset(df, QBOOSTINTENT %in% c(1,2,3))

# overall
#df_overall <- df[1:19]

# us
df_overall <- subset(df, QCOUNTRY %in% c(1))
df_overall <- df_overall[2:19]

# de
df_overall <- subset(df, QCOUNTRY %in% c(2))
df_overall <- df_overall[2:19]

# cn
df_overall <- subset(df, QCOUNTRY %in% c(3))
df_overall <- df_overall[2:19]

# br
df_overall <- subset(df, QCOUNTRY %in% c(4))
df_overall <- df_overall[2:19]

# check missing value
sapply(df_overall, function(x) sum(is.na(x)))

#=============================================================================
# combination with turf
count.comb <- function(df_overall, n) {
  comb.sum <- function(cols) length(rowSums(df_overall[cols])[rowSums(df_overall[cols]) > 0])
  counts <- combn(colnames(df_overall), n, FUN = comb.sum)
  names  <- combn(colnames(df_overall), n, FUN = paste, collapse = "+")
  setNames(counts, names)
}

c1 <- round(count.comb(df_overall, 1)/nrow(df_overall), 2)
head(sort(c1, decreasing = TRUE), 22)
c11 <- data.frame(c1)
c11 <- cbind(newColName = rownames(c11), c11)
rownames(c11) <- 1:nrow(c11)
write.csv(c11, file="turf_outputs_1.csv", row.names = FALSE)

c2 <- round(count.comb(df_overall, 2)/nrow(df_overall), 2)
head(sort(c2, decreasing = TRUE), 100)
c22 <- data.frame(c2)
c22 <- cbind(newColName = rownames(c22), c22)
rownames(c22) <- 1:nrow(c22)
write.csv(c22, file="turf_outputs_2.csv", row.names = FALSE)

c3 <- round(count.comb(df_overall, 3)/nrow(df_overall), 2)
head(sort(c3, decreasing = TRUE), 100)
c33 <- data.frame(c3)
c33 <- cbind(newColName = rownames(c33), c33)
rownames(c33) <- 1:nrow(c33)
write.csv(c33, file="turf_outputs_3.csv", row.names = FALSE)

c4 <- round(count.comb(df_overall, 4)/nrow(df_overall), 2)
head(sort(c4, decreasing = TRUE), 50)
c44 <- data.frame(c4)
c44 <- cbind(newColName = rownames(c44), c44)
rownames(c44) <- 1:nrow(c44)
write.csv(c44, file="turf_outputs_4.csv", row.names = FALSE)