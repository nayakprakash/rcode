install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggvis")
install.packages("lubridate")
library (dplyr)
library (tidyr)
library (ggplot2)
library (ggvis)
library (lubridate)

csatest<-read.csv("C:/Users/nayakp//research/Rspace/CSA.csv",header = T, sep = ",",stringsAsFactors = F)#this read.csv works well
# / slash is used for R code, \ this one for windows native address
which(csatest1$Age.at.Surgery.<=35)
ifelse(csatest1$Age.at.Surgery.<=35,"young","old")->csatest1$agesplitat35#I was super happy at being able to parse the data by age and add a young/old column in one elegant step!
with(csatest1,mean(Age.at.Surgery.[agesplitat35=="young"]))
with(csatest1,mean(Age.at.Surgery.[agesplitat35=="old"]))
subset(csatest1,agesplitat35=="young")->csayoung
subset(csatest1,agesplitat35=="old")->csaold
csatest1$agesplitat35<-as.factor(csatest1$agesplitat35)#since young and old were characters
write.table(csa,file="C:/Users/nayakp/research/Rspace/csa.csv",row.names=F)#saves output to a txt or csv file but doesn't render well with View function later

csa<-csatest[,7:52]#remove unwanted columns

# coerce POSIXct to all columns with date
csa[,grep("Date",names(csa))]<-lapply(csa[,grep("Date",names(csa))],dmy)
csa<-csa[,-9]
csa<-csa[,-5]
csa$Grade.<-as.factor(csa$Grade.)
names(csa)<-c("dob","age_surgery","sex","diagnosis","site","anatomy","presenting_status","date_biopsy","prior_surgery","type_surgery","date_surgery","type_closure","radiation","chemotherapy","size","grade","margin","necrosis","complications","surgery_complications","date_surgery_complications","relapse1_type","relapse1_date","relapse1_type_surgery","relapse1_date_surgery","relapse1_radiation","relapse1_chemotherapy","relapse2_type","relapse2_date","relapse2_type_surgery","relapse2_date_surgery","relapse2_radiation","relapse2_chemotherapy","relapse3_type","relapse3_date","relapse3_type_surgery","relapse3_date_surgery","relapse3_radiation","relapse3_chemotherapy","status","date_status","dfs_months","os_months","comments")
write.csv(csa,file="csvposix.csv",row.names=F)
