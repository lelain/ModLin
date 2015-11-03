
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
data1=data[,-redondant] #on enleve les descripteurs lies
length(data1)

#pour 2
N=length(data1)
res=array(data=rep(0,N),dim=c(N,N,N))

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
      modele = lm(data1[[i]] ~ data1[[j]]+data1[[k]], data=data1)
      res[j,k,i] = summary(modele)$r.squared   #ou r ajuste??
    }
  }
  compte=c()
}

redondant=unique(which(res>0.97,arr.ind=T)[,3])
redondant

data2=data1[,-redondant]
length(data2)


#pour liaison entre 3 variables
N=length(data2)
N=6
res=array(data=rep(0,N),dim=c(N,N,N,N))

J=1:(N-1)
K=1:(N-1)
L=1:N
compte_k=c()
compte_l=c()
for (i in 1:N)
{
  
  compte_k=c(compte_k,i)
  compte_l=c(compte_l,i)
  for (j in J[-i])
  {
    
    compte_k=c(compte_k,j)
    compte_l=c(compte_l,j)
    
    for (k in K[-compte_k])
    {
      compte_l=c(compte_l,k)
      print(L[-compte_l])   #pb ici, compte_l a revoir
      for (l in L[-compte_l])
      {
        modele = lm(data2[[i]] ~ data2[[j]]+data2[[k]]+data2[[l]], data=data2)
        res[j,k,l,i] = summary(modele)$r.squared   #ou r ajuste??
      }
    }
  }
  compte_k=c()
  compte_l=c()
}

redondant=unique(which(res>0.97,arr.ind=T)[,3])
redondant

data2=data1[,-redondant]
length(data2)

