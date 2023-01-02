##Lab 8

#Data cleaning process
bankdata <- read.csv('credit.csv',header=T,na.strings=c("")) 
# assign NA to blank/empty cells
#bankdata
bankdata <- na.omit(bankdata) # to remove NA rows in the data
#duplicated(bankdata) #to verify that no duplicates exist
bankdata <- subset(bankdata, select = -c(Loan_ID)) # removing the Loan_ID column
#bankdata

#ensure outcome is a factor
bankdata$Loan_Status <-as.factor(bankdata$Loan_Status)
#bankdata$Loan_Status


#set up data in format glmnet can use (must have dummy vars for categorical predictors)
library(glmnet)
xfactors <- model.matrix(Loan_Status ~ ., data=bankdata)[, -1]
xfactors
xnonfactors<- subset(bankdata, select = c("ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Loan_Amount_Term"))
#xnonfactors
prepared.dat <- cbind(xnonfactors, xfactors, bankdata$Loan_Status)
prepared.dat
names(prepared.dat)[15] <- c("Loan_Status")
prepared.dat$ID <- seq.int(nrow(prepared.dat))
 

#subset data to create training, valdiation, and test data
library(dplyr)
train.dat <- prepared.dat %>% sample_frac(.6)
rem.dat <- dplyr::anti_join(prepared.dat, train.dat, by = 'ID')
test.dat <- rem.dat %>% sample_frac(.5)
val.dat <- dplyr::anti_join(rem.dat, test.dat, by = 'ID')

#create predictor matrix and outcome vector for training, validation, and test data
train.X <- as.matrix(within(train.dat, rm(Loan_Status, ID)))
val.X <- as.matrix(within(val.dat, rm(Loan_Status, ID)))
test.X <- as.matrix(within(test.dat, rm(Loan_Status, ID)))

train.y <- train$Loan_Status
val.y <- valid$Loan_Status
test.y <- test$Loan_Status

#cross-validate to tune lambda for ridge and lasso
cvridge <- cv.glmnet(train.X, train.y, family="binomial", alpha=0, nlambda=20, type.measure="auc")
cvlasso <- cv.glmnet(train.X, train.y, family="binomial", alpha=1, nlambda=20, type.measure="auc")

#fit models with final lambda
ridgemod <- glmnet(train.X, train.y, family="binomial", alpha = 0, lambda = cvridge$lambda.1se)
lassomod <- glmnet(train.X, train.y, family="binomial", alpha = 1, lambda = cvlasso$lambda.1se)


##Recall from last week, a bank has asked you to help them with 
##finding a way to decide whether an applicant for a loan is likely 
##to default. Today you will write a report for the client. Using the
##same data as last week, split your data into training, validation, 
##and test (60%, 20%, 20%). Fit four models; a logistic regression model;
##a Ridge regression model; a LASSO regression model; and an Elastic Net
##model. You may choose to include transformations, interactions and 
##higher order terms in your models if you like, but you should explain 
##your choices to the client.

##Note
## Logistic regression is most appropriate when the outcome variable Y is 
## continuous. However, in most cases there aren't.

##A. logistic regression model
model <- glm( Loan_Status~., family=binomial(link='logit'), data=train)
model

?glm