
# Chapter 3: Sampling and Resampling Techniques ---------------------------

# https://github.com/Apress/machine-learning-using-r/tree/master/Code

library(data.table)
data <- fread("ccFraud.csv", header = T, verbose = F, showProgress = F)
head(data)

US_state <- fread("US_State_Code_Mapping.csv", header = T, showProgress = F)

data <- merge(data, US_state, by = "state")
head(data)

Gender_map <- fread("Gender Map.csv", header = T)
data <- merge(data, Gender_map, by = 'gender')
Credit_line <- fread("credit line map.csv", header = T)
head(Credit_line)
data <- merge(data, Credit_line, by = "creditLine")

setnames(data,"custID","CustomerID")
setnames(data,"code","Gender")
setnames(data,"numTrans","DomesTransc")
setnames(data,"numIntlTrans","IntTransc")
setnames(data,"fraudRisk","FraudFlag")
setnames(data,"cardholder","NumOfCards")
setnames(data,"balance","OutsBal")
setnames(data,"StateName","State")

data$creditLine <- NULL
data$gender <- NULL
data$state <- NULL
data$PostalCode <- NULL

# store data after merges
# write.csv(data, "CreditCardFraudData.csv", row.names = F)
str(data)

# 3.5 Population Mean
Population_Mean_P <-mean(data$OutsBal) # 4109.92

# 3.6 Population Variance
Population_Variance_P <-var(data$OutsBal)

# 3.7 Pooled Mean and Variance
set.seed(937)
i <- 1
n <- rbind(10000, 20000, 40000, 80000, 100000)
Sampling_Fraction <- n / nrow(data)
sample_mean <- numeric()
sample_variance <- numeric()
for (i in 1:5){
  sample_100k <- data[sample(nrow(data), size = n[i], replace = FALSE, prob = NULL),]
  sample_mean[i] <- round(mean(sample_100k$OutsBal), 2)
  sample_variance[i] <- round(var(sample_100k$OutsBal), 2)
}

Sample_statistics <- cbind(1:5, 
                           c("10K", "20K", "40K", "80K", "100K"),
                           sample_mean, 
                           sample_variance,
                           round(sqrt(sample_variance), 2),
                           Sampling_Fraction)
knitr::kable(Sample_statistics, col.names = c("S.No.", "Size", "Sample_Mean",
                                              "Sample_Variance", "Sample_SD",
                                              "Sample_Fraction"))



