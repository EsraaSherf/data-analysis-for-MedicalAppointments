Data<-read.csv( "E://Sna 4//BioStat//assig//MedicalAppointments.csv")
sapply(Data, class)
classes<-c("numeric","integer","character","character","character","numeric","character","integer",
                 "integer","integer","integer","integer","integer","character")
mainData<-read.table("MedicalAppointments.csv",header = TRUE,sep=",",colClasses =classes )
#as.numeric(Data$Age)

########################################2.1
c2.1<-tapply(Data$AppointmentID,Data$Neighbourhood,table)
print(c2.1)
table(Data$Neighbourhood)
################################################2.2
hist(Data$PatientId)
hist(table(Data$PatientId))
############################################### 2.3
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
DataFilter<-filter(Data,as.numeric(Data$Age)>0)
x<-as.numeric(DataFilter$Age)
hist.default(x)
density(x=x)
plot(density(x = x))
##############################################2.4
table(is.na(Data))
which(is.na(Data))
table(is.na(DataFilter))
which(is.na(DataFilter))

###################################################################2.5
DataFilter2<-filter(Data,Data$Age<1)
Datafilter3<-filter(Data,Data$Scholarship>1| Data$Hipertension>1 
                    |Data$Diabetes>1 |Data$Alcoholism>1|Data$Handcap>1
                    |Data$SMS_received>1)
Datafilter4<-filter(Data,Data$Scholarship<0| Data$Hipertension<0 
                    |Data$Diabetes<0 |Data$Alcoholism<0|Data$Handcap<0
                    |Data$SMS_received<0)
Datafilter5<-filter(Data,Data$Gender!="M"&Data$Gender!="F")

####################################################################2.6
gender<-table(DataFilter$Age,DataFilter$Gender)
male<-gender[,"M"]
female<-gender[,"F"]
par(mfrow=c(1,2))
p1<-plot(male,col=2,type="l")
#plot(male)
p2<-plot(female,type="l")
#######################################2.7
table1<-table(DataFilter$PatientId,DataFilter$No.show)
table2<-table1[,"Yes"]
veb<-table(DataFilter$PatientId)
avg=table2/veb
print(avg)
###############################################

##############################################2.8
listofranges<-cut(DataFilter$Age,
                  breaks=c(0,14,22,40,115),
                  include.lowest=TRUE,
                  labels=c("children","teenagers","adults","elderlies"))

tablemax<-table(listofranges,DataFilter$No.show)
print(tablemax)
visit<-tablemax[,"Yes"]
maxy<-max(visit)
print(maxy)
#######################################################2.9
visit<-tablemax[,"No"]
mxy<-max(visit)
print(mxy)
#################################2.10
yes<-filter(DataFilter,DataFilter$No.show=="Yes")
x2<-as.Date(yes$AppointmentDay)
mhonth<-month(x2,abbr=TRUE)
x<-table(mhonth,yes$No.show)
plot(x)








