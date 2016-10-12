complete<-function(directory,id=1:332)
{

setwd("D:/Ricky/R programming/files")
lufu<-list.files(directory)
setwd(directory)
nobs<-NULL
for(i in id)
{
 zee<-read.csv(lufu[i])
 Sum<-sum(!is.na(zee[,2]))
 nobs<-c(nobs,Sum)
 ID<-c(NULL,i)
}
data.frame(ID,nobs)
}