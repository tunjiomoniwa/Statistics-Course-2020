
###Part 1


varlist <- function(yval,xval)
  {
  ## sample of integers from 1 to 100
  x <- sample(1:100, 100, replace=T)
  # given that the error is normally distributed
  error <- rnorm(100, mean = 0, sd = 1)
  m <- 0.2 # slope
  c <- 0.1 # intercept
  y <- m*x +  c + error # output data from a simple linear regression model
  return(list(yval = y, xval = x))
}

listofvar <- varlist(y,x)
#listofvar
plot(listofvar$xval, listofvar$yval, xlab = "Explanatory variable", ylab = "Response", main = "Scatter plot")


### Part 2
### Creating a function to compute the par estimates

mx <- mean(listofvar$xval) ##mean of x
my <- mean(listofvar$yval) ##mean of y
num <- sum((listofvar$xval- mx)*(listofvar$yval - my))
den <- sum((listofvar$xval- mx)^2)
b1 <- num/den
b0 <- my - mx*b1

parvalues <- function(beta0, beta1)
  {
  return(list(beta0 = b0, beta1 = b1))
}

parv <- parvalues(b0,b1)
parv


##creating a function for least_squares_criterion
least_squares_criterion <- function(par_val, listov)
{
  return(sum((listov$yval - par_val$beta0 - par_val$beta1*listov$xval)^2))
}
kk <- least_squares_criterion(parv, listofvar)
kk


###Part 3

listov = c(listofvar[1],listofvar[2])

myfunct=function(par_val, listov){ sum((-listov$yval + par_val[2] + par_val[1]*listov$xval)^2) }

ans <- optim(c(0.5,0.5), myfunct, listov=listov, method="BFGS")
ans



#### Part 4

num_of_experiments <- 1000
parbeta0 <- vector("numeric", num_of_experiments)
parbeta1 <- vector("numeric", num_of_experiments)
for (i in 1:num_of_experiments)
  {
    listofvar <- varlist(y,x)
    listov = c(listofvar[1],listofvar[2])
    
    myfunct=function(par_val, listov){ sum((-listov$yval + par_val[2] + par_val[1]*listov$xval)^2) }
    ans <- optim(c(1,1), myfunct, listov=listov, method="BFGS")
    ###to store all 1000 par values
    parbeta0[i]<-ans$par[1]
    parbeta1[i]<-ans$par[2]
}

#parbeta0
#parbeta1
#producing the deplots for Par-beta1
plot(density(parbeta1))
#the histogram
hist(parbeta1)

##producing the deplots for Par-beta0
#plot(density(parbeta0))
#hist(parbeta0)

##Explaination:
# When I lowered the standard deviation from 1 to 0.1, I observed that the bandwidth of 
# the density plot decreased about 0.04494 to about 0.00459. This shows that the S.d.
# has considerable impact on the behaviour of our model.
