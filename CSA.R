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
install.packages ("cmprsk")
library (cmprsk)
install.packages("survival")
library (survival)
install.packages ("survcomp")
library (survcomp)
csatest<-read.csv("C:/Users/nayakp/research/Rspace/CSAmain.csv",header = T, sep = ",",stringsAsFactors = F)#this read.csv works well
# / slash is used for R code, \ this one for windows native address
# which(csatest1$Age.at.Surgery.<=35)
# ifelse(csatest1$Age.at.Surgery.<=35,"young","old")->csatest1$agesplitat35#I was super happy at being able to parse the data by age and add a young/old column in one elegant step!
#with(csatest1,mean(Age.at.Surgery.[agesplitat35=="young"]))
#with(csatest1,mean(Age.at.Surgery.[agesplitat35=="old"]))
#subset(csatest1,agesplitat35=="young")->csayoung
#subset(csatest1,agesplitat35=="old")->csaold
#csatest1$agesplitat35<-as.factor(csatest1$agesplitat35)#since young and old were characters
#write.table(csa,file="C:/Users/nayakp/research/Rspace/csa.csv",row.names=F)#saves output to a txt or csv file but doesn't render well with View function later

csa<-csatest[,7:52]#remove unwanted columns

# coerce POSIXct to all columns with date
csa[,grep("Date",names(csa))]<-lapply(csa[,grep("Date",names(csa))],dmy)
csa<-csa[,-9]
csa<-csa[,-5]
csa$Grade.<-as.factor(csa$Grade.)

# add new column names without fullstops spaces and all the non sense
names(csa)<-c("dob","age_surgery","sex","diagnosis","site","anatomy","presenting_status","date_biopsy","prior_surgery","type_surgery","date_surgery","type_closure","radiation","chemotherapy","size","grade","margin","necrosis","complications","surgery_complications","date_surgery_complications","relapse1_type","relapse1_date","relapse1_type_surgery","relapse1_date_surgery","relapse1_radiation","relapse1_chemotherapy","relapse2_type","relapse2_date","relapse2_type_surgery","relapse2_date_surgery","relapse2_radiation","relapse2_chemotherapy","relapse3_type","relapse3_date","relapse3_type_surgery","relapse3_date_surgery","relapse3_radiation","relapse3_chemotherapy","status","date_status","dfs_months","os_months","comments")

write.csv(csa,file="csvposix.csv",row.names=F)

csatest<-read.csv("C:/Users/nayakp/research/Rspace/csvposix.csv",header = T, sep = ",",stringsAsFactors = F)

# function for chondrosarcoma types
map_fn<-function(diag) {
if (grepl("mesenchymal",diag))
    "primary_mesenchymal"
else if (grepl("dedifferentiated",diag))
    "dediff"
else if (grepl("clear cell",diag) )
    "primary_clearcell"
else if (grepl("osteochondromatosis",diag) | grepl( "osteochondroma",diag) | grepl("enchondroma",diag) | grepl("chondroblastoma",diag)| grepl("enchondromas",diag)|grepl("osteochondromas",diag))
    "secondary"
else "primary_conventional"
}
csatest$type_chondrosarcoma<-sapply(csatest$diagnosis,map_fn)
csatest$type_chondrosarcoma<-as.factor(csatest$type_chondrosarcoma)
summary(csatest$type_chondrosarcoma)

# function for body region
map_region <- function(extreme){
    if (grepl("pelvis",extreme) | grepl ("spine",extreme)|grepl("Pelvis",extreme)| grepl ("Sacrum",extreme))
        "central"
    else if  (grepl("Calcaneus",extreme)|grepl("Cuneiform",extreme)|grepl("Metacarpal",extreme)|grepl("Metatarsal",extreme)|grepl("Phalanx",extreme))
        "Hand_Foot"
    else if (grepl("Clavicle",extreme)|grepl("Scapula",extreme))
        "flat_bone"
    else "Long_Bone"
 }
csatest$body_region <- sapply (csatest$anatomy, map_region)
csatest$body_region<-as.factor(csatest$body_region)

str(csatest)
write.csv(csatest,file="csvposix.csv",row.names=F)

# grade by 3 tiers

tiergrade <- csatest$grade
tiergrade <- as.character(tiergrade)
tiergrade <- gsub(1.0,"low",tiergrade)
tiergrade <-gsub(0.5,"low",tiergrade)
tiergrade <-gsub(2.0,"high",tiergrade)
tiergrade <-gsub(3.0,"dediff",tiergrade)
tiergrade <- gsub("highdediff","high",tiergrade)
tiergrade <- gsub("high4","high",tiergrade)

# substituting the NA values took some time. remember the trick to turn them into -1 and then substitute if nothing else works

tiergrade[is.na(as.character(tiergrade))]<- "high"
tiergrade <- as.factor(tiergrade)
levels(tiergrade)
csatest$tiergrade <- tiergrade
write.csv(csatest,file="csvposix.csv",row.names=F)

# margin by 4 tiers

csatest<-read.csv("C:/Users/nayakp/research/Rspace/csvposix.csv",header = T, sep = ",",stringsAsFactors = T)
csatest$margin <- as.character(csatest$margin)
map_margintype <- function (margin){
    if (grepl ("Gross",margin))
        "gross_positive"
    else if (grepl("Micro",margin))
        "micro_positive"
    else if (grepl ("Negative",margin)& (grepl("1mm",margin)|grepl("very close",margin)|grepl("revised",margin)))
        "close_negative"
    else if (grepl ("Negative",margin)| (grepl ("Negative",margin)& (grepl("cm",margin)|grepl("4mm",margin)|grepl("enchondroma @ margin",margin))))
        "negative"
    else "NA"
}
csatest$margintype <- sapply(csatest$margin,map_margintype)
csatest$margin <- as.factor(csatest$margin)
csatest$margintype <- as.factor(csatest$margintype)
levels(csatest$margintype)
write.csv(csatest,file="csvposix.csv",row.names=F)
# radiation yes or no
csatest$radiation <- as.character(csatest$radiation)
map_rtstatus<-function (rtstatus){
    if (grepl("No- remote- for Wilms tumor",rtstatus)|grepl("Remote preop",rtstatus) )
    "remote"
 else if (grepl( "PostOp",rtstatus)| grepl("PreOp",rtstatus))
    "yes"
 else
    "no"
}
csatest$radiation_status<- sapply(csatest$radiation,map_rtstatus)
csatest$radiation_status<- as.factor(csatest$radiation_status)
csatest$radiation <- as.factor(csatest$radiation)
levels(csatest$radiation_status)

# at presentation path frac, LR, Met yes or no
csatest$presenting_status <- as.character(csatest$presenting_status)
map_pathfrac <- function (frac) {
    if (grepl ("path",frac))
        "yes"
    else
        "no"}
csatest$pathfrac_presentation<- sapply (csatest$presenting_status,map_pathfrac)

map_metstatus <- function (metstatus) {
    if (grepl ("M1",metstatus))
        "yes"
    else
        "no"
}
csatest$met_presentation <- sapply (csatest$presenting_status,map_metstatus)

map_lrstatus <- function (lrstatus){
    if (grepl ("LR",lrstatus))
        "yes"
    else
        "no"
}
csatest$lr_presentation <- sapply (csatest$presenting_status,map_lrstatus)

csatest$pathfrac_presentation <- as.factor(csatest$pathfrac_presentation)
csatest$met_presentation <- as.factor(csatest$met_presentation)
csatest$lr_presentation <- as.factor(csatest$lr_presentation)
)

# LR and Met yes or no
map_lr <- function (lr){
    if (grepl ("LR",lr))
        "yes"
    else
        "no"
}

csatest$lr <- sapply (csatest$relapse1_type,map_lr)
csatest$lr <- as.factor(csatest$lr)

map_met <- function (met){
    if (grepl ("Systemic",met)|grepl("systemic",met))
        "yes"
    else
        "no"
}
csatest$met <- sapply (csatest$relapse1_type,map_met)
csatest$met <- as.factor(csatest$met)
# for lr2 and met2
map_lr <- function (lr){
    if (grepl ("LR",lr))
        "yes"
    else
        "no"
}

csatest$lr2 <- sapply (csatest$relapse2_type,map_lr)
csatest$lr2 <- as.factor(csatest$lr2)

map_met <- function (met){
    if (grepl ("Systemic",met)|grepl("systemic",met))
        "yes"
    else
        "no"
}
csatest$met2 <- sapply (csatest$relapse2_type,map_met)
csatest$met2 <- as.factor(csatest$met2)
csatest$met3 <- sapply (csatest$relapse3_type,map_met)
csatest$met3 <- as.factor(csatest$met3)
csatest$lr3 <- sapply (csatest$relapse3_type,map_lr)
csatest$lr3 <- as.factor(csatest$lr3)

csatest<-read.csv("C:/Users/nayakp/research/Rspace/csvposix.csv",header = T, sep = ",",stringsAsFactors= F)
str(csatest)
# converting all colnames with "date" to POSIXct

obj <- csatest[,c(grep("date",colnames(csatest)))]
obj <- lapply(obj, as.character)
obj <- lapply(obj, as.POSIXct)
csatest[,c(grep("date",colnames(csatest)))] <- obj
str(csatest[,c(grep("date",colnames(csatest)))])
csatest$dob <- as.POSIXct (csatest$dob)
#for (i in nrow (csatest)){
#csatest$age_sx[i] <- csatest$date_surgery[i] - csatest$dob[i]
}#
csatest$age_sx <- csatest$date_surgery - csatest$dob
head(csatest$age_sx)
str(csatest$age_sx)

# as.Date for date calculations,this is the best and easiest when time zones are not involved. Remember that the date columns are in chr mode and that '%d-%m-%Y' format is specified.Look at the str to know how the chr dates are being read, same file was read differently at home and at the lab.Wierd, haven't found out why yet.
csatest[,c(grep("date",colnames(csatest)))] <- lapply(csatest[,c(grep("date",colnames(csatest)))],as.Date,format='%Y-%m-%d')
csatest$dob <- as.Date (csatest$dob,format='%Y-%m-%d')


# calculating LRFS
# the 2 lines below did not work because as. character coerced it to 1,2 due to leaky abstraction problems as desxcribed by John Mount in r-bloggers as why factors are not first class citizens
# str(csatest)
# csatest[,c("lr","lr2","lr3")] <- as.character(csatest[,c("lr","lr2","lr3")])
for (i in 1:nrow (csatest)){
if (csatest$lr[i]=="yes"){
    csatest$lrfs[i] <- csatest$relapse1_date[i] - csatest$date_surgery[i]
}else if(csatest$lr2[i]=="yes") {
    csatest$lrfs[i] <- csatest$relapse2_date[i] - csatest$date_surgery[i]
}else if(csatest$lr3[i]=="yes") {
    csatest$lrfs[i] <- csatest$relapse3_date[i] - csatest$date_surgery[i]
}else
    csatest$lrfs[i] <- csatest$date_status[i] - csatest$date_surgery[i]
}
str(csatest)
str(csatest$lrfs)
head(csatest$lrfs)
write.csv(csatest,file="csvposix.csv",row.names=F)
csatest<-read.csv("C:/Users/nayakp/research/Rspace/csvposix.csv",header = T, sep = ",",stringsAsFactors=F)

# calculating MFS
for (i in 1:nrow (csatest)){
    if (csatest$met_presentation[i]=="yes"){
        csatest$mfs[i] <- csatest$date_surgery[i] - csatest$date_surgery[i]
   } else if (csatest$met[i]=="yes"){
    csatest$mfs[i] <- csatest$relapse1_date[i] - csatest$date_surgery[i]
} else if(csatest$met2[i]=="yes") {
    csatest$mfs[i] <- csatest$relapse2_date[i] - csatest$date_surgery[i]
} else if(csatest$met3[i]=="yes") {
    csatest$mfs[i] <- csatest$relapse3_date[i] - csatest$date_surgery[i]
}else
    csatest$mfs[i] <- csatest$date_status[i] - csatest$date_surgery[i]
}
str(csatest$mfs)

# calculating OS
csatest$os <- csatest$date_status - csatest$date_surgery
str(csatest$os)
write.csv(csatest,file="csvposix.csv",row.names=F)

# converting them all from days to years
csatest$os <- as.numeric(csatest$os)#lm model does not identify difftime after doing as.numeric for difftime objects
csatest$os_y <- (csatest$os)/365
csatest$mfs_y <- (csatest$mfs)/365
csatest$lrfs_y <- (csatest$lrfs)/365

# Analysis

# lm for survival univariate with time in days
summary (lm(lrfs~age_sx,data=csatest))
summary (lm(mfs~age_sx,data=csatest))
summary (lm(os~age_sx,data=csatest))

# lm for survival univariate with time in years
## lrfs
summary (lm(lrfs_y~age_y,data=csatest))
summary (lm(lrfs_y~factor(pathfrac_presentation),data=csatest))
summary (lm(lrfs_y~factor(margintype),data=csatest))

## mfs
summary (lm(mfs_y~age_y,data=csatest))
summary (lm(mfs_y~factor(pathfrac_presentation),data=csatest))
summary (lm(mfs_y~factor(margintype),data=csatest))

## os
summary(lm(os_y~age_y,data=csatest))
summary(lm(os_y~factor(pathfrac_presentation),data=csatest))
summary(lm(os_y~factor(margintype),data=csatest))



