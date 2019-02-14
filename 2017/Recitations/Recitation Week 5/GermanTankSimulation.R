#######################################
#### SUTD 40.004 2017 Recitation 5 ####
############# Simulation ##############
######## prepared by Qifang Bao #######
#######################################

# German tank problem 
# N = 250, n=5, sampling with replacement

E <- c() # create an empty vector to store the estimations

n = 5 # number of samples in each iteration of simulation
N = 250 # the max number of the tank
n_simulation <- 500 # number of times the simulation runs 


for (i in 1:n_simulation){
  s <-  # *Need your input! n samples randomly drawn from 0~N with replacement
  e <- # *Need your input! Value of the estimator 2*X_bar
  E <- c(E, e) # store the newest estimation to the array
}

summary(E)

t.test() # *Need your input! Calculate the mean, CI of the mean, of the simulated estimations 

