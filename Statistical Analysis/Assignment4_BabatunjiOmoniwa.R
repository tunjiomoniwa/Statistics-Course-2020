# Part 1

# To extract the series for 2 weeks before and 2 weeks after 26th June.

rate<-read.table(file="Currency.txt", header=FALSE, sep="\t")
#head(rate)

rate$V1 <- as.Date(rate$V1, "%d %b %Y" ) # to change the format of the date to the desired type

index_date <- which("2016-06-26" == rate$V1)#[[1]] # to get
index_date


lb_ind  <- index_date - 14 #for two weeks before
lb_ind
up_ind <- index_date + 14 #for two weeks after
up_ind

mr <- mean(rate$V2[lb_ind:up_ind]) # To compute the mean



plot(rate$V1[lb_ind:up_ind], rate$V2[lb_ind:up_ind], type = "l", lwd = "2", lty =3, col = "green", 
     xlab = "Date", ylab = "daily avg", main = "June 2016 Avg Daily Exchange Rate GBP:USD")
  abline(h= mr, col ="blue", lwd = "2", lty = 2)

    dev.copy2pdf(file="xchange_June2016.pdf") # to create a pdf file based on the desired format
  


# Part 2
    
my_iris <- iris
my_iris


pl <- factor(my_iris[,5], levels = c("setosa", "versicolor", "virginica" )) #setting levels based on "setosa", "versicolor", "virginica"

setos <- subset(my_iris, my_iris$Species == levels(pl)[1]) # Creating a subset for setosa
versi <- subset(my_iris, my_iris$Species == levels(pl)[2]) # Creating a subset for versicolor
virg <- subset(my_iris, my_iris$Species == levels(pl)[3]) # Creating a subset for virginica

seto_ind <- which("setosa" == my_iris$Species) # to get the in
seto_ind
lwseto <- min(seto_ind)
upseto <- max(seto_ind)


plot(setos$Sepal.Length[lwseto:upseto], setos$Sepal.Width[lwseto:upseto], pch = 16, col = "purple",
     cex = 1.5, lwd = 2,  xlim = c(min(my_iris$Sepal.Length), max(my_iris$Sepal.Length)),
     ylim = c(1.5, 5), xlab = "Sepal Length", ylab = "Sepal Width", main = "Iris Dataset",
     cex.main=3.5, cex.lab=2, cex.axis=1.2) # plot command with specifications

points(versi$Sepal.Length[lwseto:upseto], versi$Sepal.Width[lwseto:upseto], pch = 16, col = "chartreuse4",
     cex = 1.5, lwd = 2)  # point to add points to the plot

points(virg$Sepal.Length[lwseto:upseto], virg$Sepal.Width[lwseto:upseto], pch = 16, col = "darkgoldenrod2",
       cex = 1.5, lwd = 2)

legend('bottomright', legend = c("Setosa", "Versicolor", "Virginica"),
       col = c("purple", "chartreuse4", "darkgoldenrod2"), cex = 1.2, pch = c(16, 16, 16) ) 
      #to set the legend to the bottomright

  dev.copy2pdf(file="irisnew.pdf") # to create a pdf file based on the desired format



