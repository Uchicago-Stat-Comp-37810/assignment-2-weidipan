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



#the function returns the likelihood based on input x(slope)
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}



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




