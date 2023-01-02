# Part A 
#Data cleaning process
breastdata <- read.csv('breast.csv',header=T,na.strings=c("")) # assign NA to blank/empty cells
breastdata

breastdata <- subset(breastdata, select = -c(class_tumor)) 
breastdata

benigndata <- subset(breastdata, breastdata$Status == "benign")
benigndata <- subset(benigndata, select = -c(Status))
benigndata
malignantdata <- subset(breastdata, breastdata$Status == "malignant")
malignantdata <-  subset(malignantdata, select = -c(Status))
malignantdata

write.table(benigndata,file="benign_data.csv",row.names=FALSE,sep=",")
write.table(malignantdata,file="malignant_data.csv",row.names=FALSE,sep=",")
