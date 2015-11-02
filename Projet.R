
#pour 


#r?cup?ration des donn?es
x <- read.table('FW_groupe5.txt',header=T,sep="\t", dec=",",row.names=1)

data<-read.table("FW_groupe5.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(data)

#voir pour eventuellement enlever la premiere colonne d

N=length(data)

#pour 1
res=matrix(data=rep(0,N),nrow=N,ncol=N)
for (i in 1:(N-1))
{
  for (j in (i+1):N)
  {
    modele = lm(data[[i]] ~ data[[j]], data=data)
    res[i,j] = summary(modele)$r.squared
  }
}
 
which(res>0.98,arr.ind=T)
plot(data[[2]],data[[7]])


#avant de commencer, supprimer ceux de l'etape 1
#pour 2
N=length(data)
res=array(data=rep(0,N),dim=c(N,N,N))
#attention on a pas tout vu, par exemple X2~X1+X3, etc.
for (i in 1:(N-2))
{
  for (j in (i+1):(N-1))
  {
    for (k in (j+1):N)
    {
      modele = lm(data[[i]] ~ data[[j]]+data[[k]], data=data)
      res[j,k,i] = summary(modele)$r.squared
    }
  }
}
which(res>0.95,arr.ind=T)

