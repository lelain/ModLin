#reperage des donnees aberrantes

data<-read.table("FW_groupe5.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(data)
N=length(data)
Extreme=matrix(ncol=2,nrow=N)

for (i in 1:N)
{
  Extreme[i,1]=which.min(data[[i]])
  Extreme[i,2]=which.max(data[[i]])
}
unique(Extreme[,1])
unique(Extreme[,2])
