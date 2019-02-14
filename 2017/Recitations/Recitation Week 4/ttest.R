#######################################
#### SUTD 40.004 2017 Recitation 4 ####
######## prepared by Qifang Bao #######
#######################################

## Exercise I, body temperature ##
data <- read.csv("w4-body temperature.csv")
dataT <- data$Temp..F.

# Q-Q plot
qqnorm(dataT)
qqline(dataT)

# Shapiro-Wilk normality test
shapiro.test(dataT)

# Calculate CI with t-distribution
t.test(dataT,conf.level=0.99, mu=98.6)

# R documentation of function t.test()
?t.test


## Exercise II, training technique ##
dataTime <- c(50.1, 50.3, 50.3, 51.2, 51.5, 51.6)
t.test(dataTime, alternative = "less", conf.level=0.99, mu=52)


