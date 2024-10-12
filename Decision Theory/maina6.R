rm(list=ls())
#=========================== Question 1 . a
n=10000;
set.seed(5);
x=rgamma(n,2.5,0.25)

#probability that a product is successful is modelled- eq 15
psm <- function(x,mu,gamma){
  ex = exp((x-mu)*exp(-gamma));
  y = ex / (1+ex);
  
ifelse(is.nan(y),1,y)
 # plogis(ex)
}
#---------------------------------------------------------
b1<<-10; b2<<--6; b3<<--1;
theta=13;mu=15; gamma=1;

utility <-function(x,theta,mu,gamma){
#  vbinom <- Vectorize(rbinom,"prob");
#  z= vbinom(1,1,psm(x,mu,gamma));
  z= rbinom(length(x),1,psm(x,mu,gamma));
  u=rep(b2, n);
  u[which(z==1)]=b1;
  u[which(x<theta)]=b3;
  u
  }

z= rbinom(length(x),1,psm(x,mu,gamma));
u<- utility(x,theta,mu,gamma)
#vbinom <- Vectorize(rbinom,"prob");
#z= vbinom(1,1,psm(x,mu,gamma));
#=========================== Question 1 . b
mean(u)

sum(u*psm(x,mu,gamma))
#*psm(x,mu,gamma)
#=========================== Question 1 . c

range_theta=seq(10,20,by=1)

expected_utility1=c();
for(i in 1:length(range_theta)){
  u=utility(x,range_theta[i],mu,gamma);
 expected_utility1[i]=sum(u*psm(x,mu,gamma))
 # expected_utility1[i]=mean(u)
  
  #*psm(x,mu,gamma)
}
plot(range_theta,expected_utility1,type="l",col="red",xlab="theta",ylab="expected utility")


#=========================== Question 2



utility2 <-function(x,theta,mu,gamma){
  z= rbinom(1,1,psm(x,mu,gamma));
    u=ifelse(x<theta,b3,ifelse(z==1,b1,b2))
  }
    
mu=15;gamma=1;theta=13;
ex_ut <- function(theta,mu,gamma){
  exut=integrate(function(x) utility2(x,theta,mu,gamma) * psm(x,mu,gamma),min(x),max(x))
}

expected_utility2=c();

set.seed(7)
for(i in 1:length(range_theta)){
  expt=ex_ut(range_theta[i],mu,gamma)
  expected_utility2[i]=expt$value;
}

plot(range_theta,expected_utility2,type="l",col="red",xlab="theta",ylab="expected utility")

#=========================== Question 3 - a
setwd(dir = "D:/Karha/People/F.AB/Bayes/A6/code")
successful=read.csv("successful.txt",header = FALSE)[,1]
unsuccessful=read.csv("unsuccessful.txt",header = FALSE)[,1]
data=append(successful,unsuccessful)
zdata=c(rep(1,length(successful)),rep(0,length(unsuccessful)))

range_mu=c(2.5,10,15,20)
range_gamma=c(0.25,0.5,1,2)
par(mfcol=c(4,4))
for(mu in range_mu){
  for(gamma in range_gamma){
    s_model=psm(sort(data),mu,gamma)
    plot(sort(data),s_model,type="l",main=paste("mu=",mu,"gamma=",gamma),col="red",xlab="data",ylab="success model")
  }
}


#=========================== Question 3 - b

#log-odds link function
log_odds <- function(mu, gamma, x) {
  return ((x-mu)*exp(-gamma))
  
}
#-------------------------------------------------------------
#logistic transformation function
logistic <- function(logodds) {
  return (exp(logodds) / (1 + exp(logodds)))
}
#-------------------------------------------------------------
#log posterior function
logpost <- function(mu, gamma, y, x) {

  
  logodds <- log_odds(mu, gamma, x)
  probabilities <- logistic(logodds)
  
  # Calculate the log-likelihood, y~B(p)
  log_likelihood <- sum(y * log(probabilities) + (1 - y) * log(1 - probabilities))
 
   # prior distributions for mu and gamma
  log_prior_mu <- 0  #log prior for mu
  log_prior_gamma <- 0  #log prior for gamma
  
  # Calculate the log posterior
  log_posterior <- log_likelihood + log_prior_mu + log_prior_gamma
  return(log_posterior)
}
#-------------------------------------------------------------

log_posterior <- logpost(mu, gamma, zdata, data)

cat("Log Posterior:", log_posterior, "\n")


#================

#=========================== Question 3 - c
#================

logpost_matrix=matrix(0,20,20)

range_mu=seq(2.5,20,length.out=20)
range_gamma=seq(0.25,2,length.out=20)

for(i in 1:20){
  for(j in 1:20){
    logpost_matrix[i,j]=logpost (range_mu[i],range_gamma[j],zdata,data);
  }
}
par(mfcol=c(1,1))
image(range_mu,range_gamma,logpost_matrix)

#=========================== Question 4
negative_logpost <-function(param,z,x){
  mu=param[1];
  gamma=param[2];
  -logpost(mu,gamma,z,x)
}

opt_result = nlm(negative_logpost, c(15,1),zdata,data)
mu_opt = opt_result$estimate[1]
gamma_opt = opt_result$estimate[2]
cat("mu_opt=", mu_opt, "gamma_opt=",gamma_opt, "\n")
#=========================== Question 5

ex_ut <- function(theta,mu,gamma){
  exut=integrate(function(x) utility2(x,theta,mu,gamma) * psm(x,mu,gamma),min(x),max(x))
}

optimal_theta <- optimize(function(theta) -ex_ut(theta,mu_opt,gamma_opt)$value, interval = c(min(data), max(data)))$minimum
cat("theta_opt=", optimal_theta, "\n")
#=========================== Question 6

#======================

mean_gamma=range_gamma[which.max(colMeans(logpost_matrix))]
mean_mu=range_mu[which.max(rowMeans(logpost_matrix))]
cat("mean_gamma=", mean_gamma," , mean_mu=", mean_mu, "\n")

 optimal_theta <- optimize(function(theta) -ex_ut(theta,mean_mu,mean_gamma)$value, interval = c(min(data), max(data)))$minimum
 cat("theta_opt=", optimal_theta, "\n")
 #======================

