install.packages("pricesensitivitymeter")
library(pricesensitivitymeter)


set.seed(42)

# standard van Westendorp Price Sensitivity Meter Analysis
# input directly via vectors
#psm_analysis_weighted 9
tch <- round(rnorm(n = 250, mean = 5, sd = 0.5), digits = 2)
ch <- round(rnorm(n = 250, mean = 8.5, sd = 0.5), digits = 2)
ex <- round(rnorm(n = 250, mean = 13, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 250, mean = 17, sd = 1), digits = 2)
output_psm_demo1 <- psm_analysis(toocheap = tch,
                                 cheap = ch,
                                 expensive = ex,
                                 tooexpensive = tex)
summary(output_psm_demo1)


# additional analysis with Newton Miller Smith Extension
# input via data.frame
pint_ch <- sample(x = c(1:5), size = length(tex),
                  replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3))
pint_ex <- sample(x = c(1:5), size = length(tex),
                  replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1))
data_psm_demo <- data.frame(tch, ch, ex, tex, pint_ch, pint_ex)
output_psm_demo2 <- psm_analysis(toocheap = "tch",
                                 cheap = "ch",
                                 expensive = "ex",
                                 tooexpensive = "tex",
                                 pi_cheap = "pint_ch",
                                 pi_expensive = "pint_ex",
                                 data = data_psm_demo)
summary(output_psm_demo2)
