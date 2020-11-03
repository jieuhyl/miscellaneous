rm(list = ls())
setwd("C:/Users/Jie.Hu/Desktop/Sawtooth/1102")
#setwd("C:/Users/Jie.Hu/Desktop/TURF/0914")
#setwd("C:/Users/Jie.Hu/Desktop/Driver Analysis/1005")
#setwd("C:/Users/Jie.Hu/Desktop/Segmentation/0904")
#setwd("C:/Users/Jie.Hu/Desktop/IRT")
#set.seed(1337)


#install.packages('foreign')
#library(foreign)
#install.packages('haven')
library(haven)

name = 'us19001982'
spss_data <- paste0(name,'.sav')
csv_data <- paste0(name,'.csv')
mydata <- read_spss(spss_data)
write.csv(mydata, file=csv_data, row.names = FALSE)


# check missing value
sapply(mydata, function(x) sum(is.na(x)))

df <- na.omit(mydata)
df <- mydata[!is.na(mydata$QINCLUSIVEr1), ]
table(df$MRK_COUNTRY)
tapply(df$QCONCEPT_INTEREST_Lr1, df$MRK_COUNTRY, summary)
tapply(df$QINCLUSIVEr1, df$MRK_COUNTRY, summary)
tapply(mydata$QCONCEPT_INTEREST_Lr1, mydata$MRK_COUNTRY, summary)

