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
donnees2=subset(donnees,select=c("reponse",aGarder))

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
      CrossVal=CVlm(data=donnees2,m=25,form.lm=modele_sel,printit=F)
      res[i,j,k] = attr(CrossVal,"ms")   
    }
  }
}

#resultats :
Res_ind=which(res==min(res[res>0]),arr.ind=T)
Res_ind     #10,13,28. En gardant d'autres : 2,14,32, c'est a dire les descripteurs 1,34,71

names(subset(donnees2,select=Res_ind))
modele=lm(reponse~descripteur16*descripteur26*descripteur60,data=donnees)
modele_final=step(modele,direction="both")
CrossVal=CVlm(data=donnees,m=25,form.lm=modele_final,printit=T)
summary(modele_final)

#graphe des reponses et des valeurs predites par le modele
prediction = predict(modele_final,data=donnees2[,Res_ind])
plot(prediction,col="red",pch=4,xlab="numéro de la molécule",ylab="reponse")
points(reponse)
legend("topleft",legend=c("reponses observees","valeurs predites"),col=c(1,"red"),pch=c(1,4))

erreur=prediction-donnees$reponse
mean(abs(erreur))   #resultat : 1.38
