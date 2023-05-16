library(markovchain)
statesNames=c("Serba","Ada")
mcA<-new("markovchain",transitionMatrix=matrix(c(0.667,0.333,0.625,0.375),byrow = TRUE, nrow = 2, dimnames = list(statesNames,statesNames)))
print(mcA)
mcA
steadyStates(mcA)
absorbingStates(mcA)
markovchainSequence(n=20, markovchain = mcA, include = TRUE)


##MARKOV CHAIN MONTE CARLO##
trueA<-5
trueB<-0
trueSd<-10
sampleSize<-31

#MEMBANGKITKAN VARIABEL BEBAS#
x<-(-(sampleSize-1)/2):((sampleSize-1)/2)

#MEMBANGKITKAN VARIABEL DEPENDENT#
y<-trueB+trueA*x+rnorm(n=sampleSize, mean = 0, sd=trueSd)
plot(x,y, main = "Scaterplot")


##fungsi likelihood
#membuat model stokastik
likelihood<-function(param) {
  a<-param[1]
  b<-param[2]
  sd<-param[3]
  
  pred<-a*x+b
  singlelikelihoods<-dnorm(y, mean = pred, sd = sd, log = T)
  sumll<-sum(singlelikelihoods)
  return(sumll)
}

#plot likelihood
slopevalues<-function(x) {
  return(likelihood(c(x, trueB, trueSd)))
}
slopelikelihoods<-lapply(seq(3,7, by=0.05), slopevalues)
plot(seq(3,7, by=0.05), slopelikelihoods, type = "l", xlab = "value of slope parameter a", ylab = "Log likelihood")


##prior distribution
prior<-function(param) {
  a<-param[1]
  b<-param[2]
  sd<-param[3]
  aprior<-dunif(a, min = 1, max = 10, log = T)
  bprior<-dnorm(b, sd = 5, log = T)
  sdprior<-dunif(sd, min = 0, max = 30, log = T)
  return(aprior+bprior+sdprior)
}

posterior<-function(param) {
  return(likelihood(param)+prior(param))
}
proposalfunction<-function(param) {
  return(rnorm(3, mean = param, sd=c(0.1, 0.5, 0.3)))
}

run_metropolis_MCMC<-function(startvalue, iteration) {
  chain <- array(dim = c(iteration+1,3))
  chain[1,]<-startvalue
  for (i in 1:iteration) {
    proposal<-proposalfunction(chain[i,])
    probab<-exp(posterior(proposal)-posterior(chain[i,]))
    if(runif(1)<probab) {
      chain[i+1,]=proposal
    }else{
      chain[i+1,]=chain[i,]
    }
  }
  return(chain)
}

startvalue<-c(4,0,10)
chain<-run_metropolis_MCMC(startvalue, 10000)
burnln<-5000#bangkitkan sebanyak 5000 kali
acceptance<-1-mean(duplicated(chain[-(1:burnln)]))

#estimasi parameter dengan algoritma MH
cat("estimasi parameter a =", mean(chain[-(1:burnln),1]))
cat("estimasi parameter b =", mean(chain[-(1:burnln),2]))
cat("estimasi parameter sd =", mean(chain[-(1:burnln),3]))

#estimasi dengan OLS
lin<-lm(y~x)
summary(lin)
