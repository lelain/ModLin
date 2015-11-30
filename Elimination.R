#recuperation des donn?es
x <- read.table('FW_groupe5.txt',header=T,sep="\t", dec=",",row.names=1)


data<-read.table("FW_groupe5.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(data)
N=length(data)

#pour visualiser l'effet de colinearite
pairs(data[,(1:7)])  #on voit deja sur le graphe une forte colinearite entre X2 et X4, X2 et X7, X4 et X7


#pour 1 variable
res=matrix(data=rep(0,N),nrow=N,ncol=N)
for (i in 1:(N-1))
{
  for (j in (i+1):N)
  {
    modele = lm(data[[i]] ~ data[[j]], data=data)
    res[j,i] = summary(modele)$r.squared
  }
}

which(res>0.95,arr.ind=T)
#d'apres les resultats, on peut supprimer :
redondant=c(4,7,16,22,23,36,44,47)   #c(2,4,7,12,21,23,37,38)
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
      res[j,k,i] = summary(modele)$r.squared   
    }
  }
  compte=c()
}

#attention quand on supprime, parce que si par exemple X1 est bien explique par 
#X2 + X3 il faut pas supprimer X2 ou X3 ensuite !
which(res>0.969,arr.ind=T)
#au vu des resultats, il semble que l'on puisse supprimer les valeurs suivantes :
redondant=c(2,11,32,37,39) #c(12,14,30,34,36)
redondant
data2=data1[,-redondant]
length(data2)


#pour liaison entre 3 variables
N=length(data2)
N
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
      for (l in L[-compte_l])
      {
        modele = lm(data2[[i]] ~ data2[[j]]+data2[[k]]+data2[[l]], data=data2)
        res[j,k,l,i] = summary(modele)$r.squared   
      }
    }
    compte_l=compte_k
  }
  compte_k=c()
  compte_l=c()
}

which(res>0.970,arr.ind=T)
#d'apres les resultats, on peut supprimer :
redondant=c(9,13,20,21,25,33)  #c(15,19,20,34,37)  
redondant
data3=data2[,-redondant]
length(data3)

#resultat
aGarder=names(data2)[-redondant]


