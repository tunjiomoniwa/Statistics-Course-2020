
p<-seq(0,1,0.01)
Pr <- 45 * (p^8)*(1-p)^2
plot(p,Pr)

myfunc <- function(x)
{
  return((x-2)^2)
}

optim(par = 1 , fn = myfunc, method = "BFGS")


########################

## sample of integers from 1 to 100
x <- sample(1:100, 100, replace=T)
x
# given that the error is normally distributed
error <- rnorm(100, mean = 0, sd = 1)
error
m <- 0.2 # slope
c <- 0.1 # intercept
y <- m*x +  c + error # output data from a simple linear regression model 


#fit <- lm(y ~ x)
#fit
plot(x,y)
#abline(fit)

mx <- mean(x)
my <- mean(y)
num <- 0
den <- 0
for (i in 1:100){
num <- num + (x[i]- mx)*(y[i] - my)
den <- den + (x[i]- mx)^2
}
beta1 <- num/den
beta0 <- my - mx*beta1


print(beta1)
print(beta0)

#Simple linear regression least squares criterion
#f=0
#for (i in 1:100){
#  f <- f + (y[i] - beta0 - beta1*x[i])^2 
#}
#print(f)

myfunc <- function(thex)
{
  f=0
  for (i in 1:100){
    return(-thex[2]*x[i] -  thex[1] + y[i])
  }
}

optim(par = c(1,-1) , fn = myfunc, method = "BFGS")

### v = list(yVals = lin.sim(n, B_0, B_1, x), xVals = x) is okay?
## leastSqCrit()
### leastSqCrit(pars, listofvars) 
### listofvars--x,y
##par---betas 


########################

xx=c(1,2,3,4,5)
zz=c(3,4,2,2,1)
yy=c(30,40,22,33,40)

funk=function(param,x,y,z){
  a=rep(param[1],5)
  b=param[2]
  d=param[3]
  fit=sum((y-(a+b*x+z*d))^2)
  return(fit)
}

optim(par=c(1,1,1), fn=funk, x=xx, y=yy, z=zz) 