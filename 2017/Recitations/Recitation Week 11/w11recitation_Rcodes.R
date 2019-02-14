#######################################
#### SUTD 40.004 2017 Recitation 11 ###
######### Logistic Regression #########
######## prepared by Qifang Bao #######
#######################################

# import data
data <- read.csv("credit.csv")

# code dummy variables
data$Credit.History <- factor(data$Credit.History)
data$Savings.Account <- factor(data$Savings.Account)
data$Housing <- factor(data$Housing)
data$Occupation <- factor(data$Occupation)

# build logistic regression models
lr1 <- glm(Creditability ~ . , family=binomial, data)
lr2 <- glm(Creditability ~ (Duration.Of.Credit + Credit.Amount + Age)
           *(Credit.History + Savings.Account + Housing + Occupation), family=binomial, data)

# stepwise model selection
lr_step1 <- step(lr1) # defaut: backward selection

# forward model selection
minModel <- glm(Creditability ~1, family = binomial, data)
biggest <- formula(glm(Creditability ~ ., family=binomial, data))
lr_step2 <- step(minModel, direction = "forward", scope = biggest)


