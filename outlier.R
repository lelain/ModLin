#reperage des donnees aberrantes

data<-read.table("FW_groupe5.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(data)
N=length(data)

#essai avec les valeurs extremes, sachant qu'extreme est pas forcement suspect...
Extreme=matrix(ncol=2,nrow=N)
for (i in 1:N)
{
  Extreme[i,1]=which.min(data[[i]])
  Extreme[i,2]=which.max(data[[i]])
}
unique(Extreme[,1])
unique(Extreme[,2])

#residus
reg=lm(data[[1]]~data[[4]],data=data)
e <- reg$residuals
plot(data[[4]],e,ylab="Residus",xlab=names(data)[4])
abline(h=0)