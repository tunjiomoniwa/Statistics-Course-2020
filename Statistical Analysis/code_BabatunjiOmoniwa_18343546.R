########## Final Project Code by Babatunji Omoniwa - 18343546

## 1)	To come up with a way to predict the number of shares an article 
## will receive using all or some subset of the attributes provided. 
## You should demonstrate how well your prediction works by splitting 
## the data into training data (the data used to fit the model) and test 
## data (the data which is used to predict from the model).  


# Part 1A 
#Data cleaning process
projectdata <- read.csv('STU33002ProjectDataset.csv',header=T, na.strings=c("")) # assign NA to blank/empty cells
projectdata
projectdata <- na.omit(projectdata) # to remove NA rows in the data
#duplicated(projectdata) #to verify that no duplicates exist

projectdata <- subset(projectdata, select = -c(12:17, 30:37)) # keeping 39 potentially important attributes for popularity of articles
projectdata
## ncol(projectdata) #To get the length of columns in R


library(caret) #this package has the createDataPartition function

set.seed(123) #randomization`

#creating indices
trainIndex <- createDataPartition(projectdata$n_tokens_title, p=0.80,list=FALSE)

train <- projectdata[trainIndex,] #training data (80% of data)
train
test <- projectdata[-trainIndex,] #testing data (20% of data)
test

model <- lm( shares~., data=train)
model

summary(model)

p <- predict(model, newdata=subset(test,select=c(1:39)), type="response")

##ideas from https://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/


# Baseline model - predict the mean of the training data
best.guess <- mean(train$shares) 
# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$shares)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$shares))
MAE.baseline

# ...and evaluate the accuracy of the linear regression model
RMSE.lin.reg <- sqrt(mean((p - test$shares)^2))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(p-test$shares))
MAE.lin.reg


# Using a decision tree (aka regression tree)
# Needed to grow a tree
library(rpart)

rt <- rpart(shares~., data=train)
# As always, predict and evaluate on the test set
test.pred.rtree <- predict(rt,test) 

RMSE.rtree <- sqrt(mean((test.pred.rtree-test$shares)^2))
RMSE.rtree

MAE.rtree <- mean(abs(test.pred.rtree-test$shares))
MAE.rtree

##Question 2

## 2)	The editor of the World Section is interested in whether
## the articles that are published in that section on Thursdays
## fall into defined groups, and if so, what distinguishes one 
## group from another. You have been asked to investigate this question. 

projectdata <-read.table(file="STU33002ProjectDataset.csv", sep=",", header=TRUE)
projectdata

datelimit <- length(projectdata$shares) # to find the length of shares column
datelimit


###To find the subset of data published on Mondays
newMondata <- subset(projectdata, projectdata$weekday_is_monday[1:datelimit]==1)
Mondata <- data.frame(Day="Mon", share = newMondata$shares)
Mondata
#hist(newMondata$shares)

###To find the subset of data published on Tuesdays
newTuedata <- subset(projectdata, projectdata$weekday_is_tuesday[1:datelimit]==1)
Tuedata <- data.frame(Day="Tue", share = newTuedata$shares)
Tuedata


###To find the subset of data published on Wednesday
newWeddata <- subset(projectdata, projectdata$weekday_is_wednesday[1:datelimit]==1)
Weddata <- data.frame(Day="Wed", share = newWeddata$shares)
Weddata

###To find the subset of data published on thursdays
newThursdata <- subset(projectdata, projectdata$weekday_is_thursday[1:datelimit]==1)
Thursdata <- data.frame(Day="Thurs", share = newThursdata$shares)
Thursdata
#hist(newThursdata$shares)

###To find the subset of data published on Fridays
newFridata <- subset(projectdata, projectdata$weekday_is_friday[1:datelimit]==1)
Fridata <- data.frame(Day="Fri", share = newFridata$shares)
Fridata

###To find the subset of data published on Saturdays
newSatdata <- subset(projectdata, projectdata$weekday_is_saturday[1:datelimit]==1)
Satdata <- data.frame(Day="Sat", share = newSatdata$shares)
Satdata

###To find the subset of data published on Sundays
newSundata <- subset(projectdata, projectdata$weekday_is_sunday[1:datelimit]==1)
Sundata <- data.frame(Day="Sun", share = newSundata$shares)
Sundata

#I have succeeded in converting the binary data to categorical data
totaldata <- rbind(Mondata, Tuedata, Weddata, Thursdata, Fridata, Satdata, Sundata) #combining all data according to day of the week
##totaldata[14555:15000,]
plot(totaldata$Day, totaldata$share, xlab = "Days of the Week", ylab = "Shares")

Thursdata

newdf <- scale(totaldata$share) # To apply the scaling function
newdf

newthdata <- scale(Thursdata$share) # To apply the scaling function on thursday
newthdata 



# install.packages("Ckmeans.1d.dp") # I installed this package for univariate clustering
require(Ckmeans.1d.dp) # sourced from https://www.r-pkg.org/pkg/Ckmeans.1d.dp
k <- 6  #choose your number of clusters
result <- Ckmeans.1d.dp(newdf, k)
#thursresult <- Ckmeans.1d.dp(newthdata, k)
plot(result)
#line(thursresult)

plot(newdf, col=result$cluster, pch=result$cluster, cex=1.5,
     main="Optimal univariate clustering given k",
     sub=paste("Number of clusters given:", k))
abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
legend("topright", c(paste("Cluster", 1:k), "Thurs Data"), col=1:k, pch=1:k, cex=0.8, bty="n")
points(newthdata, col= 'purple')

