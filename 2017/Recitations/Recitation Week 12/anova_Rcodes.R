#######################################
#### SUTD 40.004 2017 Recitation 12 ###
################ ANOVA ################
######## prepared by Qifang Bao #######
#######################################

### Q1 Caffeine - one way ANOVA ###
data_caffeine <- read.csv('caffeine.csv')  # import data 
plot(PerformanceLevel ~ CaffeineDose, data = data_caffeine) # visualize data with boxplot

# run ANOVA analysis
aov_Q1 <- aov(PerformanceLevel ~ CaffeineDose, data = data_caffeine)
summary(aov_Q1)

# residual diagnostics
par(mfrow=c(2,2))
plot(aov_Q1)
dev.off()

# Bonferroni pairwise t-test
pairwise.t.test(data_caffeine$PerformanceLevel, 
                data_caffeine$CaffeineDose, 
                p.adj = "bonferroni")


# equivalent to aov()
oneway.test(PerformanceLevel ~ CaffeineDose, data = data_caffeine, var.equal = TRUE)



### Q2 Paper Airplane - two way ANOVA ###
data_PaperAirplane <- read.csv('paper_airplane.csv') # import data
aov_Q2 <- aov(Distance ~ NoseLength*WingAngle, data = data_PaperAirplane) # ANOVA test
summary(aov_Q2)


