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







