#install.packages('openxlsx')
#library(openxlsx)

rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0528/ALL")
set.seed(1337)


# read data
df1 <- read.csv('turf_0528_us.csv', header=T, stringsAsFactors=FALSE)
df2 <- read.csv('turf_0528_uk.csv', header=T, stringsAsFactors=FALSE)
df3 <- read.csv('turf_0528_br.csv', header=T, stringsAsFactors=FALSE)
df4 <- read.csv('turf_0528_de.csv', header=T, stringsAsFactors=FALSE)
df5 <- read.csv('turf_0528_in.csv', header=T, stringsAsFactors=FALSE)
df5 <- df5[sample(nrow(df5), 1500), ]

df <- do.call("rbind", list(df1, df2, df3, df4, df5))
df_overall <- df

# Total
df_overall <- df[2:25]

# FB_Creator
df_overall <- subset(df, QCOHORT4 %in% c(1))
df_overall <- df_overall[2:25]

# FB_Member	
df_overall <- subset(df, QCOHORT4 %in% c(2))
df_overall <- df_overall[2:25]

# Competitive_Creator
df_overall <- subset(df, QCOHORT4 %in% c(3))
df_overall <- df_overall[2:25]

# Competitive_Member
df_overall <- subset(df, QCOHORT4 %in% c(4))
df_overall <- df_overall[2:25]

# Non-Category_User
df_overall <- subset(df, QCOHORT4 %in% c(5))
df_overall <- df_overall[2:25]

# Priority_Segments
df_overall <- subset(df, QSEGMENTATION %in% c(1,2,3,7))
df_overall <- df_overall[2:25]



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
head(sort(c1, decreasing = TRUE), 10)
c1_freq <- count.comb(df_overall, 1)
c11 <- data.frame(c1)
c11 <- cbind(newColName = rownames(c11), c11, c1_freq)
rownames(c11) <- 1:nrow(c11)
c11$c1 = as.numeric(as.character(c11$c1))
c11$c1_freq = as.numeric(as.character(c11$c1_freq))
colnames(c11) <- c("Combination", "Reach", "Frequency")
write.csv(c11, file="turf_outputs_1.csv", row.names = FALSE)

c2 <- round(count.comb(df_overall, 2)/nrow(df_overall), 2)
head(sort(c2, decreasing = TRUE), 10)
c2_freq <- count.comb(df_overall, 2)
c22 <- data.frame(c2)
c22 <- cbind(newColName = rownames(c22), c22, c2_freq)
rownames(c22) <- 1:nrow(c22)
c22$c2 = as.numeric(as.character(c22$c2))
c22$c2_freq = as.numeric(as.character(c22$c2_freq))
colnames(c22) <- c("Combination", "Reach", "Frequency")
write.csv(c22, file="turf_outputs_2.csv", row.names = FALSE)

c3 <- round(count.comb(df_overall, 3)/nrow(df_overall), 2)
head(sort(c3, decreasing = TRUE), 10)
c3_freq <- count.comb(df_overall, 3)
c33 <- data.frame(c3)
c33 <- cbind(newColName = rownames(c33), c33, c3_freq)
rownames(c33) <- 1:nrow(c33)
c33$c3 = as.numeric(as.character(c33$c3))
c33$c3_freq = as.numeric(as.character(c33$c3_freq))
colnames(c33) <- c("Combination", "Reach", "Frequency")
write.csv(c33, file="turf_outputs_3.csv", row.names = FALSE)

c4 <- round(count.comb(df_overall, 4)/nrow(df_overall), 2)
head(sort(c4, decreasing = TRUE), 10)
c4_freq <- count.comb(df_overall, 4)
c44 <- data.frame(c4)
c44 <- cbind(newColName = rownames(c44), c44, c4_freq)
rownames(c44) <- 1:nrow(c44)
c44$c4 = as.numeric(as.character(c44$c4))
c44$c4_freq = as.numeric(as.character(c44$c4_freq))
colnames(c44) <- c("Combination", "Reach", "Frequency")
write.csv(c44, file="turf_outputs_4.csv", row.names = FALSE)

c5 <- round(count.comb(df_overall, 5)/nrow(df_overall), 2)
head(sort(c5, decreasing = TRUE), 10)
c5_freq <- count.comb(df_overall, 5)
c55 <- data.frame(c5)
c55 <- cbind(newColName = rownames(c55), c55, c5_freq)
rownames(c55) <- 1:nrow(c55)
c55$c5 = as.numeric(as.character(c55$c5))
c55$c5_freq = as.numeric(as.character(c55$c5_freq))
colnames(c55) <- c("Combination", "Reach", "Frequency")
write.csv(c55, file="turf_outputs_5.csv", row.names = FALSE)


#===============================================================
## Create new workbooks
wb <- createWorkbook() 

## Create the worksheets
addWorksheet(wb, sheetName = "c1")
addWorksheet(wb, sheetName = "c2")
addWorksheet(wb, sheetName = "c3")
addWorksheet(wb, sheetName = "c4")
addWorksheet(wb, sheetName = "c5")

## Write the data
writeData(wb, "c1", c11)
writeData(wb, "c2", c22)
writeData(wb, "c3", c33)
writeData(wb, "c4", c44)
writeData(wb, "c5", c55)

## Save workbook to working directory 
saveWorkbook(wb, "turf_ALL_Total.xlsx", overwrite = TRUE)
saveWorkbook(wb, "turf_IN_FB_Creator.xlsx", overwrite = TRUE)
saveWorkbook(wb, "turf_IN_FB_Member.xlsx", overwrite = TRUE)
saveWorkbook(wb, "turf_IN_Competitive_Creator.xlsx", overwrite = TRUE)
saveWorkbook(wb, "turf_IN_Competitive_Member.xlsx", overwrite = TRUE)
saveWorkbook(wb, "turf_IN_Non-Category_User.xlsx", overwrite = TRUE)
saveWorkbook(wb, "turf_IN_Priority_Segments.xlsx", overwrite = TRUE)


