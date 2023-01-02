library(ggplot2)
library(jtools)


df <- USArrests  #US data
df
ln <- length(df$Rape)
ln

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

range01(df$Murder)

dd <- data.frame(
  X1=range01(df$Murder),
  X2=range01(df$Assault),
  X3=range01(df$UrbanPop),
  Outcome = range01(df$Rape)
)

# fit standard model     
mymodel<-glm(Outcome~X1+X2+X3, data=dd, family=quasibinomial)
mypred <- predict(mymodel.glm, type="response", interval = "confidence")
mypred
#effect_plot(mymodel, X2)

mydata <- cbind(dd, mypred)
#mydata

#library("ggplot2")
p <- ggplot(mydata, aes(X2, Outcome)) +
  geom_point() + stat_smooth(method = glm) + 
  geom_smooth(stat = 'smooth', color = 'Red')
p