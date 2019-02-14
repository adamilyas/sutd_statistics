#######################################
#### SUTD 40.004 2017 Recitation 5 ####
############# Simulation ##############
######## prepared by Qifang Bao #######
#######################################


### plot pdf and cdf of a normal distribution
x <- seq(-4,6,0.5)

ypdf <- dnorm(x,1,2)
plot(x,ypdf,"l")

ycdf <- pnorm(x,1,2)
plot(x,ycdf,"l")



### German Tank
E <- c() # create an empty vector to store the result from simulations

n = 5
N = 250
n_simulation <- 500 # number of times the simulation runs 

for (i in 1:n_simulation){
  s <- sample(0:N,n,replace = TRUE) # 5 samples randomly drawn from 0~250 with replacement
  e <- 2*mean(s) # value of the estimator 2*X_bar
  E <- c(E, e) # store the newest estimation to the array
}

t.test(E, mu=251) 

