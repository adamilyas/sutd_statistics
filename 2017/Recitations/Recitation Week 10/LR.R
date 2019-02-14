#######################################
#### SUTD 40.004 2017 Recitation 10 ###
#### Polinomial Linear Regression #####
######## prepared by Qifang Bao #######
#######################################

# read data from csv file
dataWL <- read.csv("weightlifting.csv")

# create polinomial terms of the weight catetory
dataWL$WeightCategory2 <- dataWL$WeightCategory^2
dataWL$WeightCategory3 <- dataWL$WeightCategory^3

# scatter plot
plot(dataWL$WeightCategory, dataWL$Record) 

# use the first 6 data points to fit linear regression models
# use the last data point to validate models
dataTrain <- dataWL[1:6,]
dataValidate <- dataWL[7,]

data = dataTrain
# fit linear regression models
lr1 <- lm(Record ~ WeightCategory, data)
lr2 <- lm(Record ~ WeightCategory + WeightCategory2, data)
lr3 <- lm(Record ~ WeightCategory + WeightCategory2 + WeightCategory3, data)
lr4 <- lm(Record ~ WeightCategory2, data)
lr5 <- lm(Record ~ WeightCategory3, data)
lr6 <- lm(Record ~ WeightCategory2 + WeightCategory3, data)
lr7 <- lm(Record ~ WeightCategory + WeightCategory3, data)

# save adjusted R-Squared of the seven models into dataframe "R2adj"
R2adj <- data.frame("Adjust R-Squared" = summary(lr1)$adj.r.squared)
rownames(R2adj) <- c("lr1")    # rename the row
R2adj <- rbind(R2adj, lr2 = summary(lr2)$adj.r.squared)
R2adj <- rbind(R2adj, lr3 = summary(lr3)$adj.r.squared)
R2adj <- rbind(R2adj, lr4 = summary(lr4)$adj.r.squared)
R2adj <- rbind(R2adj, lr5 = summary(lr5)$adj.r.squared)
R2adj <- rbind(R2adj, lr6 = summary(lr6)$adj.r.squared)
R2adj <- rbind(R2adj, lr7 = summary(lr7)$adj.r.squared)

# Prediction using linear regression model
RecordPredict <- predict(lr1, dataValidate)

# Compare the predicted value to the actual value 
# Save the difference into dataframe "ErrorPredict"
ErrorPredict <- data.frame("Error" = predict(lr1, dataValidate) - dataValidate$Record)
rownames(ErrorPredict) <- c("lr1")   # rename the row
ErrorPredict <- rbind(ErrorPredict, rl2 = predict(lr2, dataValidate) - dataValidate$Record)
ErrorPredict <- rbind(ErrorPredict, rl3 =predict(lr3, dataValidate) - dataValidate$Record)
ErrorPredict <- rbind(ErrorPredict, rl4 =predict(lr4, dataValidate) - dataValidate$Record)
ErrorPredict <- rbind(ErrorPredict, rl5 =predict(lr5, dataValidate) - dataValidate$Record)
ErrorPredict <- rbind(ErrorPredict, rl6 =predict(lr6, dataValidate) - dataValidate$Record)
ErrorPredict <- rbind(ErrorPredict, rl7 =predict(lr7, dataValidate) - dataValidate$Record)


### plot datapoints with regression lines
# Create data for regression lines
newdat = data.frame(WeightCategory = seq(min(dataWL$WeightCategory-10), 
                             max(dataWL$WeightCategory+10), 
                             length.out = 100))
newdat$WeightCategory2 <- newdat$WeightCategory^2
newdat$WeightCategory3 <- newdat$WeightCategory^3

newdat$pred2 = predict(lr2, newdata = newdat)
newdat$pred3 = predict(lr3, newdata = newdat)

# plot data
plot(dataTrain$WeightCategory, dataTrain$Record, pch=16,
     xlim = c(50,110), ylim = c(200,500), 
     xlab = "Weight Category", ylab = "Record") 

# plot the validation point as red
points(dataValidate$WeightCategory, dataValidate$Record, col="red", pch=16)

# plot regression lines
lines(newdat$WeightCategory, newdat$pred2, col = "blue", lwd = 1) # plot regression line
lines(newdat$WeightCategory, newdat$pred3, col = "green", lwd = 1.5) # plot regression line

# legend
legend(85,270,c( "regression model 2", "regression model 3"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1.5,1.5), # gives the legend lines the correct width
       col=c("blue","green"))  # and colors

