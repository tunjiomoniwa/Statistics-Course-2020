#PART 1A

#First I tried to read the raw data from EU_20190102.txt

Y<-read.table(file="EU_20190102.txt", sep=",", header=TRUE)

print(Y) #printing the output of the raw data

kk <- subset(Y, age>=30 & age<=40) #All EU customers between 30 and 40 are returned using the subset function
print(kk) #To print the result of all EU customers between 30 and 40

#next to write in the data to a CSV
write.table(kk,file="EU_cust_30_40.csv",row.names=FALSE,sep=",")


#PART 1B

ter <- subset(Y, education=='tertiary' & (job =='management'| job =='blue-collar')) #finding EU customers with tertiary education, and doing blue-collar or management job
print(ter) # to print the subset

#next to write in the data to a CSV
write.table(ter,file="EU_cust_ter_bc_mgmt.csv",row.names=FALSE,sep=",")



#Part 2 
#using the levels command

pl <- factor(Y[,2], levels = c("unemployed", "services", "management", "blue-collar", "self-employed", "technician", "entrepreneur", "admin.", "student", "housemaid", "retired" ))
levels(pl)[1] 


# I was able to implement it, but i'll practice using your approach of getting the levels
for (i in 1:11){
  
  ma <- subset(Y, job == levels(pl)[i]) #this generates a subset of services
  print(ma)
  zzz <- paste("EU_cust", levels(pl)[i],".csv") #to create a unique file name
  print(zzz)
  write.table(ma,file= zzz,row.names=FALSE,sep=",") #writes the file
  
   i= i + 1
  
}

#PART 3

Y<-read.table(file="EU_20190102.txt", sep=",", header=TRUE)

print(Y) #printing the output of the raw data

Q<-read.table(file="US_2019_02_01.txt", sep=" ", header=TRUE)


Y$poutcome <- NULL #I deleted the poutcome column in the EU customer data

Y$contact <- NULL #I deleted the contact column in the EU customer data for uniformity since the data wont make meaning when merged
Q$contact <- NULL #I deleted the contact column in the US customer data for uniformity

Y$balance <- Y$balance/0.87 # converting Euros to dollars
print(Y)
write.table(Y,file="EU_data.csv",row.names=FALSE,sep=",")
newY<-read.table(file="EU_data.csv", sep=",", header=TRUE)
print(newY)

print(Q) #printing the output of the raw data
write.table(Q,file="US_data.csv",row.names=FALSE, sep=",")
newQ<-read.table(file="US_data.csv",sep=",", header=TRUE)
print(newQ)
