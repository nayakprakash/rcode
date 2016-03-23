install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggvis")

library (dplyr)
library (tidyr)
library (ggplot2)
library (ggvis)

csatest1<-read.csv("C:/Users/nayakp//research/Rspace/CSA.csv",header = T, sep = ",",stringsAsFactors = F)#this read.csv works well
# / slash is used for R code, \ this one for windows native address
which(csatest1$Age.at.Surgery.<=35)
ifelse(csatest1$Age.at.Surgery.<=35,"young","old")->csatest1$agesplitat35#I was super happy at being able to parse the data by age and add a young/old column in one elegant step!
with(csatest1,mean(Age.at.Surgery.[agesplitat35=="young"]))
with(csatest1,mean(Age.at.Surgery.[agesplitat35=="old"]))
subset(csatest1,agesplitat35=="young")->csayoung
subset(csatest1,agesplitat35=="old")->csaold
csatest1$agesplitat35<-as.factor(csatest1$agesplitat35)#since young and old were characters
str (csayoung)
csa<- csatest1[,1:52]
str(csa)
write.table(csa,file="C:/Users/nayakp/research/Rspace/csa.csv",row.names=F)#saves output to a txt or csv file but doesn't render well with View function later
