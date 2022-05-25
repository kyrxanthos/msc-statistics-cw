setwd("/Users/lysi2/Documents/UNI_Imperial/CompStats/Assignments/Group")

dat <- read.csv("group11.csv")
dat$Y
dat$X1
dat$X2

likelihood <- function(param){
  b1 = param[1]
  b2 = param[2]
  omega = param[3]
  
  pred = b1*dat$X1 + b2*dat$X2
  singlelikelihoods = dnorm(dat$Y, mean = pred, sd = omega, log = T)
  # print(singlelikelihoods)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

prior <- function(param){
  # b1_prior <- rnorm(1,mean=0,sd=5)
  b1_prior <- dnorm(rnorm(1,mean=0,sd=5), mean=param[1],sd=5)
  # b2_prior <- rnorm(1,mean=0,sd=5)
  b2_prior <- dnorm(rnorm(1,mean=0,sd=5), mean=param[2],sd=5)
  # omega_prior <- rgamma(1,1,1)
  omega_prior <- dgamma(rgamma(1,1,1), 1,1)
  # print(b1_prior)
  # print(b2_prior)
  # print(omega_prior)
  # print('---')
  return(log(b1_prior) +log(b2_prior)+log(omega_prior))
}


posterior <- function(param){
  return (likelihood(param) + prior(param))
  
}

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(5,5,1)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    print(posterior(proposal))
    print('--')
    print(posterior(chain[i,]))
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}


startvalue = c(1,1,1)
chain = run_metropolis_MCMC(startvalue, 10000)


trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31


