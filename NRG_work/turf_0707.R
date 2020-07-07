rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0707")
set.seed(1337)

library(openxlsx)

# read data
df <- read.csv('turf_0701.csv', header=T, stringsAsFactors=FALSE)
df <- subset(df, QCONCEPT_INTEREST %in% c(1,2,3))
df_overall <- df[, -c(1,12)]

#df_overall[is.na(df_overall)] <- 0

# check missing value
sapply(df_overall, function(x) sum(is.na(x)))



#=============================================================================
# combination with turf
count.comb <- function(df_overall, n) {
  comb.len <- function(cols) length(rowSums(df_overall[cols])[rowSums(df_overall[cols]) > 0])
  counts <- combn(colnames(df_overall), n, FUN = comb.len)/nrow(df_overall)
  names  <- combn(colnames(df_overall), n, FUN = paste, collapse = "+")
  setNames(counts, names)
}

freq.comb <- function(df_overall, n) {
  comb.sum <- function(cols) sum(rowSums(df_overall[cols])[rowSums(df_overall[cols]) > 0])
  freqs <- combn(colnames(df_overall), n, FUN = comb.sum)
  names  <- combn(colnames(df_overall), n, FUN = paste, collapse = "+")
  setNames(freqs, names)
}



c1 <- round(count.comb(df_overall, 1), 2)
head(sort(c1, decreasing = TRUE), 10)
c1_freq <- freq.comb(df_overall, 1)
c11 <- data.frame(c1)
c11 <- cbind(newColName = rownames(c11), c11, c1_freq)
rownames(c11) <- 1:nrow(c11)
c11$c1 = as.numeric(as.character(c11$c1))
c11$c1_freq = as.numeric(as.character(c11$c1_freq))
colnames(c11) <- c("Combination", "Reach", "Frequency")
write.csv(c11, file="turf_outputs_1.csv", row.names = FALSE)

c2 <- round(count.comb(df_overall, 2), 2)
head(sort(c2, decreasing = TRUE), 10)
c2_freq <- freq.comb(df_overall, 2)
c22 <- data.frame(c2)
c22 <- cbind(newColName = rownames(c22), c22, c2_freq)
rownames(c22) <- 1:nrow(c22)
c22$c2 = as.numeric(as.character(c22$c2))
c22$c2_freq = as.numeric(as.character(c22$c2_freq))
colnames(c22) <- c("Combination", "Reach", "Frequency")
write.csv(c22, file="turf_outputs_2.csv", row.names = FALSE)

c3 <- round(count.comb(df_overall, 3), 2)
head(sort(c3, decreasing = TRUE), 10)
c3_freq <- freq.comb(df_overall, 3)
c33 <- data.frame(c3)
c33 <- cbind(newColName = rownames(c33), c33, c3_freq)
rownames(c33) <- 1:nrow(c33)
c33$c3 = as.numeric(as.character(c33$c3))
c33$c3_freq = as.numeric(as.character(c33$c3_freq))
colnames(c33) <- c("Combination", "Reach", "Frequency")
write.csv(c33, file="turf_outputs_3.csv", row.names = FALSE)



#===============================================================
## Create new workbooks
wb <- createWorkbook() 

## Create the worksheets
addWorksheet(wb, sheetName = "c1")
addWorksheet(wb, sheetName = "c2")
addWorksheet(wb, sheetName = "c3")


## Write the data
writeData(wb, "c1", c11)
writeData(wb, "c2", c22)
writeData(wb, "c3", c33)


## Save workbook to working directory 
saveWorkbook(wb, "turf_ALL_Total_v2.xlsx", overwrite = TRUE)
