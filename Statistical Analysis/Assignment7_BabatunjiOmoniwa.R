# Part A 
#Data cleaning process
bankdata <- read.csv('credit.csv',header=T,na.strings=c("")) # assign NA to blank/empty cells
#bankdata
bankdata <- na.omit(bankdata) # to remove NA rows in the data
#duplicated(bankdata) #to verify that no duplicates exist
bankdata <- subset(bankdata, select = -c(Loan_ID)) # removing the Loan_ID column
bankdata

library(caret) #this package has the createDataPartition function

set.seed(123) #randomization`

#creating indices
trainIndex <- createDataPartition(bankdata$Gender,p=0.80,list=FALSE)

train <- bankdata[trainIndex,] #training data (80% of data)
train
test <- bankdata[-trainIndex,] #testing data (20% of data)
test

model <- glm( Loan_Status~., family=binomial(link='logit'), data=train)
model

summary(model)

#To interpreting the results of our logistic regression model
#we carry out ANOVA
#anova(model, test="Chisq")


#####PART B
# Install and load ROCR package
#install.packages("ROCR")
#To plot the ROC Curve 
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(1:11)), type="response")
pr <- prediction(p, test$Loan_Status)
#the performance function takes in the prediction
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf
plot(prf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))



## Justification for the threshold (0.5802) 
## I used the 3Q deviance residual, which can be used to check the 
## model's fitness at each observation for generalized linear models.
## I observed at that threshold the accuracy of our prediction is
## very good (about 81%)
## Also, from the ROC curve plotted below, the optimal threshold can
## be seen to lie between 0.4 and 0.6.


##PART 3

#To evaluate the performance of our model, by comparing the actual against the predicted
fitted.results <- predict(model,newdata=subset(test,select=c(1:11)),type='response')
#fitted.results
plot(fitted.results) # we plot the fitted results to visualize
abline(h = 0.5802, col=c("red"), lty=c(2), lwd=c(2)) #we draw a line to demarkate the points that are dissimilar


#Defining a threshold to classify an applicant as "repay" or "default"
fitted.results <- ifelse(fitted.results > 0.5802,"repay","default") # ifelse(test, yes, no)
fitted.results

##assigning labels to the Loan_status column
levels(test$Loan_Status)[1] <-"default"
levels(test$Loan_Status)[2] <-"repay"
test$Loan_Status

## To evaluate the error performance we compare the Actual vs. the prediction
misClasificError <- mean(fitted.results != test$Loan_Status)
misClasificError
# we can observe that the accuracy varies based on our selected threshold
print(paste('Accuracy is given as: ',(1-misClasificError)*100,'%', sep = ""))
