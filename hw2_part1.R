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

# Given that our linear model y = b + a*x + N(0,sd) takes the parameters (a, b, sd) as
# an input, the likelihood function returns the probability of obtaining the test data
# above under this model
likelihood <- function(param){
  a = param[1]    #assigns the value frist number in param to a 
  b = param[2]    #assigns the value second number in param to b 
  sd = param[3]   #assigns the value of the third number in param to sd
  
  pred = a*x + b  #calculates predictions
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) #a vector of single log likelihoods
  sumll = sum(singlelikelihoods) #the logarithm of a product equals the sum of the logarithms
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
#the function returns the likelibood based on input x(slope)
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues ) #applies the function on a vector of slopes
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")
#plot log likelihoods against different values of slope parameter a 

# Prior distribution
prior <- function(param){
  a = param[1]   #assigns the value frist number in param to a
  b = param[2]  #assigns the value second number in param to b 
  sd = param[3]  #assigns the value of the third number in param to sd
  aprior = dunif(a, min=0, max=10, log = T) #log desity at a for unif[0,10]
  bprior = dnorm(b, sd = 5, log = T)    #log desity at b for normal distribution
  sdprior = dunif(sd, min=0, max=30, log = T)   #log desity at sd for unif[0,30]
  return(aprior+bprior+sdprior)
}


#Posterior distribution
posterior <- function(param){
  return (likelihood(param) + prior(param))
}


######## Metropolis algorithm ################



#The function returns a new parameter value close to the old value based on the function
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}


#The function returns a matrix chain which shows approximations to the true parameters
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))    #specifying the dimensions of an emtry matrix
  chain[1,] = startvalue    #initialize the first row with starting value
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])    #returns a new parameter value close to the old value
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))  #acceptance probability
    if (runif(1) < probab){
      chain[i+1,] = proposal    #if true, proposals are saved to the next row of the chain
    }else{
      chain[i+1,] = chain[i,]   #if false, the next row stays the same as the previous row
    }
  }
  return(chain)
}

startvalue = c(4,0,10)  #starting value of the procedure
chain = run_metropolis_MCMC(startvalue, 10000) #inputs startvalue and number of iterations, outputs matrix

burnIn = 5000
#The first steps of the algorithm may be biased by the initial value, 
#and are therefore usually discarded for the further analysis (burn-in time)

acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))  # acceptance rate


### Summary: #######################

graph_summary <- function(chain,burnIn,trueA,trueB,trueSd) {

  par(mfrow = c(2,3))
  hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )
  #shows posterior estimates for slope (a)
  abline(v = mean(chain[-(1:burnIn),1]))
  #vertical line of mean of estimated slope
  abline(v = trueA, col="red" )
  #vertical line of true slope
  hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
  #shows posterior estimates for intercept
  abline(v = mean(chain[-(1:burnIn),2])) #vertical line of mean of estimated intercept
  abline(v = trueB, col="red" ) #vertical line of true intercept
  hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
  #shows posterior estimates for standard deviation of the error
  abline(v = mean(chain[-(1:burnIn),3]) ) #vertical line of mean of estimated sd
  abline(v = trueSd, col="red" ) #vertical line of true sd
  plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
  abline(h = trueA, col="red" )
  #Markov Chain of slope values
  plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
  abline(h = trueB, col="red" )
  #Markov Chain of intercept values
  plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
  abline(h = trueSd, col="red" )
  #Markov Chain of sd values

}

graph_summary(chain,burnIn,trueA,trueB,trueSd)


# for comparison:
summary(lm(y~x))
#compare this procedure MCMC with linear regression







