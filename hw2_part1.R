source("functions.R")

trueA <- 5   #true slope  
trueB <- 0    #true intercept
trueSd <- 10    #true standard deviation of error
sampleSize <- 31  #sample size

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)  # n=sampleSize
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y, main="Test Data")  
#balanced x values around zero to “de-correlate” slope and intercept
#a scatterplot of test data


# Example: plot the likelihood profile of the slope a

slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues) #applies the function on a vector of slopes
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")
#plot log likelihoods against different values of slope parameter a 




######## Metropolis algorithm ################


startvalue = c(4,0,10)  #starting value of the procedure
chain = run_metropolis_MCMC(startvalue, 10000) #inputs startvalue and number of iterations, outputs matrix

burnIn = 5000
#The first steps of the algorithm may be biased by the initial value, 
#and are therefore usually discarded for the further analysis (burn-in time)

acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))  # acceptance rate


### Summary: #######################


graph_summary(chain,burnIn,trueA,trueB,trueSd)


# for comparison:
summary(lm(y~x))
#compare this procedure MCMC with linear regression


#compare_outcomes, that takes as input an iteration number
#for instance 1000, 10000, or 100000. Your function should loop 10 times; each 
#time, it should initialize the MCMC chain with randomly selected start values 
#for a, b, and sd and then run to completion after the required number of 
#iterations. After each loop, the function should compute the mean and std 
#of the values in the chain for a, and it should print these out. Test your 
#function for 1,000, 10,000, and 100,000 iterations.


compare_outcomes <- function(iterations) {
  for (i in 1:10) {
    a = runif(1, min=0, max=10)
    b = rnorm(1, 0, 5)
    sd = runif(1, min=0, max=30)
    startvalue=c(a,b,sd)
    chain = run_metropolis_MCMC(startvalue,iterations)
    print(c(mean(chain[,1]),sd(chain[,1])))
  }
}


compare_outcomes(1000)
compare_outcomes(10000)
compare_outcomes(100000)




