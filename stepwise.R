#selection stepwise, apres la premiere elimination
#Ici on travaille sur le fichier FW_groupe5_obs
#D'abord supprimer de ce fichier les colonnes qu'on a selectionner dans Elimination

#pour la cross validation
library(DAAG)

donnees<-read.table("FW_groupe5_obs.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(donnees)
N=length(donnees)

#On garde les bonnes colonnes
aGarder = c("reponse","descripteur1","descripteur3","descripteur8","descripteur10","descripteur13",
            "descripteur14","descripteur15","descripteur16","descripteur21","descripteur22",
            "descripteur23","descripteur28","descripteur34","descripteur38","descripteur39",
            "descripteur41","descripteur43","descripteur44","descripteur45","descripteur46",
            "descripteur47","descripteur48","descripteur49","descripteur50","descripteur51",
            "descripteur59","descripteur60","descripteur63","descripteur65","descripteur66",
            "descripteur71","descripteur73")
data2=subset(donnees,select=aGarder)

N=length(data2)
N=5
res=array(data=rep(0,N),dim=c(N,N,N))
for (i in 2:(N-2))
{
  for (j in (i+1):(N-1))
  {
    for (k in (j+1):N)
    {
      #pb de noms, il faudrait lui donner les vrais nom : descripteuri, etc. a voir avec la fonction factor()
      modele = lm(names(data2[1]) ~ names(data2[i])*names(data2[j])*names(data2[k]), data=data2)
      selection=step(modele,direction="both")
      CrossVal=CVlm(data=data2,m=25,form.lm=selection)
      #res[i,j,k] = attr(CrossVal,"ms")   
    }
  }
}
str(data2)

modele=lm(factor(names(data2[1]))~factor(names(data2[2]))*factor(names(data2[3])),data=donnees2)
is.factor(reponse)
resDAAG <- CVlm(data=donnees,m=25,form.lm=modele)
attr(resDAAG,"ms")
donnees2=donnees[-9,]
donnees3=donnees[9,]
modele=lm(reponse~descripteur1*descripteur14,data=donnees2)
modele1=lm(reponse~descripteur1*descripteur14,data=donnees)
predict(modele,newdata=donnees3)
predict(modele,newdata=donnees3)
PRESS(modele)

#resultats :
Res_ind=which(res==min(res[res>0]),arr.ind=T)
Res_ind
#On garde les colonnes qu'il faut
names(subset(X_c,select=Res_ind))
modele=lm(reponse~descripteur1*descripteur14*descripteur71,data=donnees)
modele_final=step(modele,direction="both")
summary(modele_final)

#Test sur l'echantillon de validation
result=predict(modele_final,newdata=X_v)
plot(result)
points(X_v[[1]],col=2)

#erreur sur l'echantillon test
result-X_v$reponse
mean(abs(result-X_v$reponse))
erreur=mean((result-X_v$reponse)**2)
erreur
