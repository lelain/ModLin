
#pour 


#r?cup?ration des donn?es
x <- read.table('FW_groupe5.txt',header=T,sep="\t", dec=",",row.names=1)

data<-read.table("FW_groupe5.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(data)

N=length(data)

#pour 1 variable
res=matrix(data=rep(0,N),nrow=N,ncol=N)
for (i in 1:(N-1))
{
  for (j in (i+1):N)
  {
    modele = lm(data[[i]] ~ data[[j]], data=data)
    res[i,j] = summary(modele)$r.squared
  }
}

redondant=unique(which(res>0.96,arr.ind=T)[,2])
redondant
data=data[,-redondant] #on enleve les descripteurs lies
length(data)

#pour 2
N=length(data)
res=array(data=rep(0,N),dim=c(N,N,N))
#attention on a pas tout vu, par exemple X2~X1+X3, etc.
J=1:(N-1)
K=1:N
compte=c()
for (i in 1:N)
{
  compte=c(compte,i)
  for (j in J[-i])
  {
    compte=c(compte,j)
    for (k in K[-compte])
    {
      modele = lm(data[[i]] ~ data[[j]]+data[[k]], data=data)
      res[j,k,i] = summary(modele)$r.squared
    }
  }
  compte=c()
}

redondant=unique(which(res>0.97,arr.ind=T)[,3])
redondant

data2=data[,-redondant]
length(data2)
