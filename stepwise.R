#selection stepwise, apres la premiere elimination
#Ici on travaille sur le fichier FW_groupe5_obs

#pour la cross validation
library(DAAG)

#lit les donnees
donnees<-read.table("FW_groupe5_obs.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(donnees)

#On garde les bonnes colonnes
aGarder = c("reponse","descripteur1","descripteur3","descripteur8","descripteur10","descripteur13",
            "descripteur14","descripteur15","descripteur16","descripteur21","descripteur22",
            "descripteur23","descripteur28","descripteur34","descripteur38","descripteur39",
            "descripteur41","descripteur43","descripteur44","descripteur45","descripteur46",
            "descripteur47","descripteur48","descripteur49","descripteur50","descripteur51",
            "descripteur59","descripteur60","descripteur63","descripteur65","descripteur66",
            "descripteur71","descripteur73")
donnees2=subset(donnees,select=aGarder)

#le press minimum pour tous les sous-ensembles a 3 variables
N=length(donnees2)
res=array(data=rep(0,N),dim=c(N,N,N))
for (i in 2:(N-2))
{
  for (j in (i+1):(N-1))
  {
    for (k in (j+1):N)
    {
      nom = c(names(donnees2[i]),names(donnees2[j]),names(donnees2[k]))
      formule = as.formula(paste("reponse ~ ",paste(nom,collapse = "*")))
      modele=lm(formule,data=donnees2)
      modele_sel=step(modele,direction="both")
      CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_sel,plotit="Observed",printit=FALSE)
      res[i,j,k] = attr(CrossVal,"ms")   
    }
  }
}

#resultats :
Res_ind=which(res==min(res[res>0]),arr.ind=T)
Res_ind=c(2,14,32) #resultat : 2,14,32
#On garde les colonnes qu'il faut
names(subset(donnees2,select=Res_ind))
modele=lm(reponse~descripteur1*descripteur34*descripteur71,data=donnees2)
modele_final=step(modele,direction="both")
CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_final,plotit="Observed",printit=T)
summary(modele_final)

#pour sous ensembles a 2 variables
N=length(donnees2)
res=array(data=rep(0,N),dim=c(N,N))
for (i in 2:(N-1))
{
  for (j in (i+1):N)
  {
    nom = c(names(donnees2[i]),names(donnees2[j]))
    formule = as.formula(paste("reponse ~ ",paste(nom,collapse = "*")))
    modele=lm(formule,data=donnees2)
    modele_sel=step(modele,direction="both")
    CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_sel,plotit="Observed",printit=FALSE)
    res[i,j] = attr(CrossVal,"ms")   
  }
}
#resultats :
Res_ind=which(res==min(res[res>0]),arr.ind=T)
Res_ind   #resultat : 2,7
#On garde les colonnes qu'il faut
names(subset(donnees2,select=Res_ind))
modele=lm(reponse~descripteur1*descripteur14,data=donnees2)
modele_final=step(modele,direction="both")
CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_final,plotit="Observed",printit=T)
summary(modele_final)

#pour sous ensembles a 4 variables
N=length(donnees2)
res=array(data=rep(0,N),dim=c(N,N,N,N))
for (i in 2:(N-3))
{
  for (j in (i+1):(N-2))
  {
    for (k in (j+1):(N-1))
    {
      for (l in (k+1):N)
      {
        nom = c(names(donnees2[i]),names(donnees2[j]),names(donnees2[k]),names(donnees2[l]))
        formule = as.formula(paste("reponse ~ ",paste(nom,collapse = "*")))
        modele=lm(formule,data=donnees2)
        modele_sel=step(modele,direction="both")
        CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_sel,plotit="Observed",printit=FALSE)
        res[i,j,k,l] = attr(CrossVal,"ms")  
      }
    } 
  }
}

#resultats :
Res_ind=which(res==min(res[res>0]),arr.ind=T)
Res_ind    #resultat : 
#On garde les colonnes qu'il faut
names(subset(donnees2,select=Res_ind))
modele=lm(reponse~descripteur1*descripteur34*descripteur71,data=donnees2)
modele_final=step(modele,direction="both")
CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_final,plotit="Observed",printit=T)
summary(modele_final)