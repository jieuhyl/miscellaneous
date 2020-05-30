#install.packages('openxlsx')
library(openxlsx)

rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0529")
set.seed(1337)


# read data
df <- read.csv('turf_0528_us.csv', header=T, stringsAsFactors=FALSE)

# Total
df_Total <- df[2:25]

# FB_Creator
df_FB_Creator <- subset(df, QCOHORT4 %in% c(1))
df_FB_Creator <- df_FB_Creator[2:25]

# FB_Member	
df_FB_Member <- subset(df, QCOHORT4 %in% c(2))
df_FB_Member <- df_FB_Member[2:25]

# Competitive_Creator
df_Competitive_Creator <- subset(df, QCOHORT4 %in% c(3))
df_Competitive_Creator <- df_Competitive_Creator[2:25]

# Competitive_Member
df_Competitive_Member <- subset(df, QCOHORT4 %in% c(4))
df_Competitive_Member <- df_Competitive_Member[2:25]

# Non-Category_User
df_Non_Category_User <- subset(df, QCOHORT4 %in% c(5))
df_Non_Category_User <- df_Non_Category_User[2:25]

# Priority_Segments
df_Priority_Segments <- subset(df, QSEGMENTATION %in% c(1,2,3,7))
df_Priority_Segments <- df_Priority_Segments[2:25]


# =============
write.function <- function(df_overall, title)
  {
  # combination with turf
  count.comb <- function(df_overall, n) {
    comb.sum <- function(cols) length(rowSums(df_overall[cols])[rowSums(df_overall[cols]) > 0])
    counts <- combn(colnames(df_overall), n, FUN = comb.sum)
    names  <- combn(colnames(df_overall), n, FUN = paste, collapse = "+")
    setNames(counts, names)
  }
  
  c1 <- round(count.comb(df_overall, 1)/nrow(df_overall), 2)
  c1_freq <- count.comb(df_overall, 1)
  c11 <- data.frame(c1)
  c11 <- cbind(newColName = rownames(c11), c11, c1_freq)
  rownames(c11) <- 1:nrow(c11)
  c11$c1 = as.numeric(as.character(c11$c1))
  c11$c1_freq = as.numeric(as.character(c11$c1_freq))
  colnames(c11) <- c("Combination", "Reach", "Frequency")

  
  c2 <- round(count.comb(df_overall, 2)/nrow(df_overall), 2)
  c2_freq <- count.comb(df_overall, 2)
  c22 <- data.frame(c2)
  c22 <- cbind(newColName = rownames(c22), c22, c2_freq)
  rownames(c22) <- 1:nrow(c22)
  c22$c2 = as.numeric(as.character(c22$c2))
  c22$c2_freq = as.numeric(as.character(c22$c2_freq))
  colnames(c22) <- c("Combination", "Reach", "Frequency")
  
  c3 <- round(count.comb(df_overall, 3)/nrow(df_overall), 2)
  c3_freq <- count.comb(df_overall, 3)
  c33 <- data.frame(c3)
  c33 <- cbind(newColName = rownames(c33), c33, c3_freq)
  rownames(c33) <- 1:nrow(c33)
  c33$c3 = as.numeric(as.character(c33$c3))
  c33$c3_freq = as.numeric(as.character(c33$c3_freq))
  colnames(c33) <- c("Combination", "Reach", "Frequency")
  
  c4 <- round(count.comb(df_overall, 4)/nrow(df_overall), 2)
  c4_freq <- count.comb(df_overall, 4)
  c44 <- data.frame(c4)
  c44 <- cbind(newColName = rownames(c44), c44, c4_freq)
  rownames(c44) <- 1:nrow(c44)
  c44$c4 = as.numeric(as.character(c44$c4))
  c44$c4_freq = as.numeric(as.character(c44$c4_freq))
  colnames(c44) <- c("Combination", "Reach", "Frequency")
  
  c5 <- round(count.comb(df_overall, 5)/nrow(df_overall), 2)
  c5_freq <- count.comb(df_overall, 5)
  c55 <- data.frame(c5)
  c55 <- cbind(newColName = rownames(c55), c55, c5_freq)
  rownames(c55) <- 1:nrow(c55)
  c55$c5 = as.numeric(as.character(c55$c5))
  c55$c5_freq = as.numeric(as.character(c55$c5_freq))
  colnames(c55) <- c("Combination", "Reach", "Frequency")
  
  
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
  saveWorkbook(wb, paste0('turf_', title, ".xlsx"))
  
}

# single one run
write.function(df_Total, "US_total")


# all the run
df_list <- list(US_Total = df_Total,
                US_FB_Creator = df_FB_Creator,
                US_FB_Member = df_FB_Member,
                US_Competitive_Creator= df_Competitive_Creator,
                US_Competitive_Member = df_Competitive_Member,
                US_Non_Category_User = df_Non_Category_User,
                US_Priority_Segments = df_Priority_Segments)
  

for(i in 1:length(df_list)) {
  print(names(df_list)[i])
  print(dim(df_list[[i]]))
  write.function(df_list[[i]], names(df_list)[i])
}


 
