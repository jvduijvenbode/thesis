#install.packages("data.table",dependencies=T)
#install.packages("boa",dependencies=T)
##load necessary packages
library(data.table)
library(plyr)
library(boa)
library(reshape2)
##set the working directory
setwd("D:/spectralsignatures")
values=read.csv("allflightvals.csv")

values$panel=substr(values$filename,14,14)
values$flight=substr(values$filename,12,12)
ILSvalues=as.data.frame(c(values[c(1,c(108:length(values)))]))
ILSvalues$filename=NULL
flightvalues=as.data.frame(c(values[c(1,7:107,c(209:211))]))
flightvalues$filename=NULL
ILSvalnorm<-data.frame()
for(paneel in c(1:5)){ 
ILSnormalized<-subset(ILSvalues,ILSvalues$panel==paneel)
ILSnormalized[2:102]<-ILSnormalized[2:102]/colMeans(ILSnormalized[2:102])
ILSvalnorm<-rbind(ILSvalnorm,ILSnormalized)
}
colnames(ILSvalnorm)<-colnames(ILSvalues)
rm(ILSnormalized)
flightvalues<-arrange(flightvalues,X)
ILSvalnorm<-arrange(ILSvalnorm,X)
flightvalscorrected<-data.frame(flightvalues[,1],flightvalues[,2:102]/ILSvalnorm[2:102],flightvalues[,103:104])


uncorrectedvariance<-data.frame()
correctedvariance<-data.frame()
for(paneel in c(1:5)){ 
  uncorvar<-subset(flightvalues,flightvalues$panel==paneel)
  corvar<-subset(flightvalscorrected,flightvalscorrected$panel==paneel)
  uncvars<-c()
  cvars<-c()
  for (i in 2:102){uncvars<-rbind(uncvars,as.numeric(var(uncorvar[i])))}
  for (i in 2:102){cvars<-rbind(cvars,as.numeric(var(corvar[i])))}
  uncorrectedvariance<-cbind(colnames(uncorvar)[2:102],uncvars)
  correctedvariance<-cbind(colnames(corvar)[2:102],cvars)
  
}

colnames(uncorrectedvariance)<-c("band","variance uncorrected")
colnames(correctedvariance)<-c("band","variance corrected")
variancematrix<-merge(uncorrectedvariance,correctedvariance,by="band")
