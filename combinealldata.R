#install.packages("data.table",dependencies=T)
##load necessary packages
library(data.table)
library(plyr)
##set the working directory
setwd("D:/spectralsignatures/flight3")
##read the time files from the independent flights
time=read.table("Datacube_GPS_Times.txt",sep="\t",skip=1)
##remove unnecessary columns
time[2]=NULL
time[2]=NULL
time[2]=NULL
##add column names to dataframe
colnames(time)= c("ID","Hour","Minute","Second")
##join to one time moment and convert to numeric data
time$sumtime=as.numeric(strptime(paste(time$Hour,":",time$Minute,":",unlist(strsplit(as.character(time$Second),"\\."))[1],sep=""),format="%H:%M:%S"))
ILS_time<-read.csv("D:/flight 3/ILS-timed.csv")
ILS_time$time<-as.numeric(strptime(ILS_time$time,format="%H:%M:%S"))
##remove some more unnecessary data columns (old time info)
time[2]=NULL
time[2]=NULL
time[2]=NULL
#allflightvals=data.frame()

##get all flight values
flightfiles<-list.files()
for (x in flightfiles[1:5]){
  values=read.table(x,sep=";",header=T)
  ##merge with time values of flight
  timed_values=merge(time,values,by="ID",all.values=T,all.times=F)
  tt4=c()
  ##merge with the data of the ILS
  for(y in timed_values$sumtime){
    t1<-which.min(abs(ILS_time$time-y))
    tt4=c(tt4,ILS_time$time[t1])
  }
  timed_values$time=tt4
  combtimedvalues<-merge(timed_values,ILS_time,by="time")
  combtimedvalues$sumtime=NULL
  ##get mean values per time value
  uniqval<-aggregate(as.matrix(combtimedvalues) ~ time , data = combtimedvalues , FUN = mean )
  uniqval$filename<-x
  allflightvals=rbind(allflightvals,uniqval)
  
  #write.csv(combtimedvalues,file=paste("../timed_",x,sep=""),row.names=F)
}
#write.csv(allflightvals,file="allflightvals.csv")