#reperage des donnees aberrantes

data<-read.table("FW_groupe5.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(data)
N=length(data)
