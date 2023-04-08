# pre made models in R studio you can build around to answer hte ICA 

# ICA 6 

setwd("C:/Users/Adam/Desktop/TellComputer/2022-2023/Pubh7440/Week 11")

canada <- read.csv('canadian_cities.csv')


# question 1 

linearmod_1 <- '
 model{
   for( i in 1 : N) {
			y[i] ~ dnorm(mu[i],tau) # normal outcome 
			mu[i] <- beta[1] + beta[2]*x[i] # linear regression
   }
		# prior distributions 
		
		for(k in 1:p){
		beta[k] ~ dnorm(0, 0.001)
		}
		
		# prior for precision 
		tau ~ dgamma(0.01, 0.01)
		sigma2 <- 1/tau
		
		# predict Victoria, province 4!! british columbia
		logpop_vic <- beta[1] + beta[2] * log(300.9)
		pop_vic <- exp(logpop_vic)
 }
'

data_list <- list('y' = log(canada$pop1996), 'x' = log(canada$pop1992), 
                  'N' = nrow(canada), 'p' = 2)


# mod 2, new linear model with constants for province; 

linearmod_2 <- '
 model{
   for( i in 1 : N) {
			y[i] ~ dnorm(mu[i],tau) # normal outcome 
			mu[i] <- beta[1] + beta[2]*x[i] + betap[prov[i]] # linear regression
   }
		# prior distributions 
		
		for(k in 1:p){
		beta[k] ~ dnorm(0, 0.001)
		}
		
		# sort of like dummy variables, but ignoring the first province
		# the intercept will take care of the first province (cant have duplicate parameters)
		for(q in 2:G){
		betap[q] ~ dnorm(0, 0.001)
		}
		betap[1] <- 0
		
		# prior for precision 
		tau ~ dgamma(0.01, 0.01)
		sigma2 <- 1/tau
		
				# predict Victoria, province 4!! british columbia
		logpop_vic <- beta[1] + beta[2] * log(300.9) + betap[4]
		pop_vic <- exp(logpop_vic)
 }
'

# convert the province variable into a numeric starting with 1
province <- as.numeric(as.factor(canada$province)) #<- this works for random effect
data_list <- list('y' = log(canada$pop1996), 'x' = log(canada$pop1992), 
                  'N' = nrow(canada), 'p' = 2, 'G' = max(province), 
                  'prov' = province)


linearmod_3 <- '
 model{
   for( i in 1 : N) {
			y[i] ~ dnorm(mu[i],tau) # normal outcome 
			mu[i] <- beta[1] + beta[2]*x[i] + betap[prov[i]] # linear regression
   }
		# prior distributions 
		
		for(k in 1:p){
		beta[k] ~ dnorm(0, 0.001)
		}
		
		for(q in 1:G){
		betap[q] ~ dnorm(0, tau_b)
		}
		
		# prior for precision 
		tau ~ dgamma(0.01, 0.01)
		tau_b ~ dgamma(0.01, 0.01)
		sigma2 <- 1/tau
		sigma2_b <- 1/tau_b
		
		# predict Victoria, province 4!! british columbia
		logpop_vic <- beta[1] + beta[2] * log(300.9) + betap[5]
		pop_vic <- exp(logpop_vic)
 }
'

