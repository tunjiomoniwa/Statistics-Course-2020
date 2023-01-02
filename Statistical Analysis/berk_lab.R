sensorposi<-read.table(file="mote_locs.txt", header=FALSE)
sensorposi
plot(sensorposi$V2, sensorposi$V3, xlab = "x - position", ylab = "y-position")
length(sensorposi$V1)


connect<-read.table(file="connectivity.txt",  fill = TRUE, header=FALSE)
connect


pl <- factor(connect$V1, levels = 1:length(sensorposi$V1))  
levels(pl)[2]
ma <- subset(connect, (connect$V1 == levels(pl)[2] && connect$V2 == levels(pl)[4]))
ma

newma <- data.frame(ma, V4 = sensorposi$V2[2], V5 = sensorposi$V3[2], V6 = sensorposi$V2[2], V7 = sensorposi$V3[2])
#newma <- data.frame(ma, V4 = sensorposi$V2[i], V5 = sensorposi$V3[i], V6 = sensorposi$V2[j], V7 = sensorposi$V3[j])
newma

pl <- factor(connect$V1, levels = 1:length(sensorposi$V1)) 
levels(pl)[2]
for (i in 1:length(connect$V1)){
  for (j in 1:length(connect$V1)){
    
     
    levels(pl)[i]
    newma <- data.frame(connect, V4 = sensorposi$V2[i], V5 = sensorposi$V3[i], V6 = sensorposi$V2[j], V7 = sensorposi$V3[j])
    
  }
  
}

newma
#return
nn <- sensorposi$V2[1]

#how to map position to connectivity?
t <- which(connect, connect$V1[i] == levels(pl)[i])

help ?which()

adddf <- data.frame(connect, V4 = rnorm(1:length(connect$V1)))
adddf




labels

