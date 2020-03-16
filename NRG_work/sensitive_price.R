rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Segmentation/0225")
set.seed(1337)

df <- read.csv('sensitive.csv', header=T, stringsAsFactors=FALSE)


output_psm_demo <- psm_analysis(toocheap = "QPSM2",
                                 cheap = "QPSM4",
                                 expensive = "QPSM3",
                                 tooexpensive = "QPSM1",
                                 #pi_cheap = "pint_ch",
                                 #pi_expensive = "pint_ex",
                                 data = df)

summary(output_psm_demo)
