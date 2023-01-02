
data.a <- c(3,4,5,2,1,3,6,5,4)
matrix.a <- matrix(data.a, nrow = 3, ncol = 3, byrow = TRUE)
matrix.a

zz <- matrix.a^2
zz

zz_sum <- apply( zz, MARGIN = 1, sum )
zz_sum 

rr <- exp(matrix.a) + 1
rr
 
mystatfunc <- function( arg1, arg2, arg3 )
{
  #do something with arg1, arg2, arg3
  #return a value
}