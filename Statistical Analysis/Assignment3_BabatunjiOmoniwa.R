# Tasks 1

# Generate a 1000 times 100 matrix of standard normal random variates. 

P <- 1000 # define P as 1000
Q <- 100  # define Q as 100
Matrix_A <- matrix( rnorm(P*Q, mean=0, sd=1), P, Q)
#The matrix function was to generate the 1000 X 100 matrix
#rnorm is to generate the normal random variates
Matrix_A

#Task 1A
# Using the apply function to find row means 
row_mean <- apply( Matrix_A, MARGIN = 1, mean )
row_mean

#Task 1B
# Using the apply function to find column means 
Col_mean <- apply( Matrix_A, MARGIN = 2, mean )
Col_mean

#Task 1C
# Using the apply function to find sum of squared row entries

Matrix_A_sqr <- Matrix_A^2 # This is to find the square of each entry
Matrix_A_sqr

row_sum_square <- apply( Matrix_A_sqr, MARGIN = 1, sum )
#the apply function is used to find the sum
row_sum_square

#Task 1D
# Using the apply function to find sum of exp(x) + 1 for each of the rows
Matrix_A_exp <- exp(Matrix_A) + 1 # This is to find exp(x) + 1 of each entry
Matrix_A_exp

row_exp_fxn <- apply( Matrix_A_exp, MARGIN = 1, sum )
#the apply function is used to find the sum of the given expression 
row_exp_fxn


# Task 2


mystatfunc <- function(matrix, task_call)
{
  task_call <- readline(prompt="Is your operation on the rows or columns (type r or c): ")
  task_call
  #do something with arg1, arg2, arg3
  if (task_call=='r'){
    row_mean <- apply( Matrix_A, MARGIN = 1, mean ) # generate a vector with the row means
    row_sd <- apply( Matrix_A, MARGIN = 1, sd ) # generate a vector with the sd means
    
    #return a value
   #row_mean #this returns the row mean
    #row_sd #this returns the row sd
    names(row_mean) <-c(1:length(row_mean))
    names(row_sd) <-c(1:length(row_sd))
    a <- paste('Row', names(row_mean),'Mean', row_mean)#as stated in the question
    b <- paste('Row', names(row_sd),'St Dev', row_sd) #as stated in the question
    }
  else{
    Col_mean <- apply( Matrix_A, MARGIN = 2, mean )# generate a vector with the column means
    Col_sd <- apply( Matrix_A, MARGIN = 2, sd )# generate a vector with the column means
    #Col_mean #this returns the column mean
    #Col_sd #this returns the column sd
    names(Col_mean) <-c(1:length(Col_mean))
    names(Col_sd) <-c(1:length(Col_sd))
    a <- paste('Col', names(Col_mean),'Mean', Col_mean) #as stated in the question
    b <- paste('Col', names(Col_sd),'St Dev', Col_sd) #as stated in the question
    #paste('Col Mean', Col_mean, 'Col St Dev', Col_sd)
    }
  
  return(list( a, b))
}

mystatfunc(Matrix_A)

#

# Part 3

#Part 3A

myhistplot <- function(entermatrix)
{
  row_mean <- apply( entermatrix, MARGIN = 1, mean )
  hist(row_mean, main = "Row Means", xlim = c(-0.5, 0.4), ylim = c(0, 220))
  
}

myhistplot(Matrix_A) #This plots the histogram of row means from Part A

#Part 3B

# Generate a 1000 times 1000 matrix of standard normal random variates. 

D <- 1000 # define P as 1000
G <- 100  # define Q as 100
Matrix_B <- matrix( rnorm(D*G, mean=0, sd=1), D, G)

#To comppute the row mmeans using my function
mystatfunc(Matrix_B)

# To plot the histogram
myhistplot(Matrix_B) #This plots the histogram of the new matrix


### Observation
#1 As the samples size increases, the spread reduces and more samples converge towards the mean (more precise), while smaller samples are more evenly spread. 

#2 The spread in the 1000 X 1000 matrix has a smaller range of (-0.3 to 0.35 ) compared to that in the 1000 X 100 matrix ( -0.4 to about 0.38).