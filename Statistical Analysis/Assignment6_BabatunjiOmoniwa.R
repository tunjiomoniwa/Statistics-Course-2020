
#PART 1
#Code to simulate a mixture model from 3 components
# I used this approach because I could understand what goes on 

samp_num <- 150
obs <- numeric(samp_num)

for (i in seq_len(samp_num)){
  prob.i <- runif(n, min = 0, max = 1) # Uniform probability distribution
  if (prob.i>=0 && prob.i<0.3333){
    obs[i] <- rnorm(1, mean = -1, sd = 0.1) # rnorm(n, mean = x, sd = x)
  }  
  else if (prob.i>=0.3333 && prob.i<0.6666)
  {
    obs[i] <- rnorm(1, mean = 0, sd = 0.1) # rnorm(n, mean = x, sd = x)
  }
  else
  {
    obs[i] <- rnorm(1, mean = 1, sd = 0.1) # rnorm(n, mean = x, sd = x)
  }
}

obs
hist(obs, main = paste("Histogram of 150 observations"), xlab = "Observations") # to plot the histogram of the mixture

# From varying the standard deviation (s.d.), 
# I observed that the power to detect clusters lies in the spread and s.d. of the given distribution.
#########################################

#PART 2

library(mclust) # I added this library for Gaussian Mixture Modelling for Model-Based Clustering, Classification, and Density Estimation
newdatay <- seq(-1.5,1.5,0.015) # I get 201 points from this
#length(seq(-1.5,1.5,0.015)) # used to check the number of points

out <- EM.GMM(x= obs, G=3, initpar = NULL, maxstep = 2000, tol = 1e-6 )
#out$par$mu
emGMM_fxn <- function(out1e=out, y = newdatay) {
    #from the lecture slides, we get the expression for the density
  fx <- out1e$par$wei[1]*dnorm(y, mean = out1e$par$mu[1], sd = out1e$par$sd[1]) + out1e$par$wei[2]*rnorm(y, mean = out1e$par$mu[2], sd = out1e$par$sd[2]) +out1e$par$wei[3]*rnorm(y, mean = out1e$par$mu[3], sd = out1e$par$sd[3])
  return(fx)
  }

dataEM <- emGMM_fxn(out, newdatay)
dataEM

hist(obs, freq = FALSE)# to plot the histogram of the mixture
points(newdatay, dataEM) # I overlayed the points


###############
#Part 3


emGMMbic_fxn <- function(out=out) {
  #from the lecture slides, we get the expression for BIC
  #BICG = -2( max log-likelihood )G + (3G - 1) log(n)
  return(-2*out$loglike + (3*out$G - 1)*log(length(out$data)))
}
emGMMbic_fxn(out) # the BIC using out user defined function

# Now we try to fit the model to differentr values of G, 2, 3, 4, 
obsG_2 <- emGMMbic_fxn(EM.GMM(x= obs, G=2, initpar = NULL, maxstep = 2000, tol = 1e-6 ))
obsG_2

obsG_3 <- emGMMbic_fxn(EM.GMM(x= obs, G=3, initpar = NULL, maxstep = 2000, tol = 1e-6 ))
obsG_3

obsG_4 <- emGMMbic_fxn(EM.GMM(x= obs, G=4, initpar = NULL, maxstep = 2000, tol = 1e-6 ))
obsG_4

obsG_5 <- emGMMbic_fxn(EM.GMM(x= obs, G=5, initpar = NULL, maxstep = 2000, tol = 1e-6 ))
obsG_5
############Yes the BIC may slightly affect the correctness of the model at s.d=1
####When the S.D was increase from 0.1 to 0.5, there was slighter increase from G=2, upto G=5



##########
#Part 4
modelA <- Mclust(obs)
modelA

# to get the bic using the predefined function
modelA$BIC

### Yes, there is a difference observed using the predefined function with 
### that obtained from our user-defined function (emGMMbic_fxn).
#### For G=3 and S.d. = 0.1, the BIC for the emGMMbic_fxn is 68.5342
##### while the BIC for the Mclust(obs) = -68.53420 (v,3)
###### The value is inverted.
######This implies that the in-built function deals with the model negatively
####### It may be minimizing the likelihood for data points to belong to other groups
######## WHile our function is maximizing the likelihood for data points to belong same group
