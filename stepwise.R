#selection stepwise, apres la premiere elimination
#Ici on travaille sur le fichier FW_groupe5_obs
#D'abord supprimer de ce fichier les colonnes qu'on a selectionner dans Elimination

# definition de la fonction PRESS, sans doute inutile, peut-être fausse
PRESS <- function(model) {
  pr <- residuals(model)/(1-lm.influence(model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

#pour le problème des noms dans lm, step et VClm, faire choses de ce style :
lm(as.formula(paste(names(x[1])," ~ ", names(x[2]))),data=x)
#mieux :
nom = c(names(x[2]),names(x[3]),names(x[4]))
formu = as.formula(paste("reponse ~ ",paste(nom,collapse = "*")))
lm(formu,data=x)

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

#ensuite prendre 70% de l'echantillon pour faire la calibration (si pas assez on en prendra plus ou cross validation)
#sur les 30% restant pour la validation du modele. 
row_val=sample(1:25,5)   #on garde 7 lignes pour la validation
X_v=data2[row_val,]   #X_v, v pour validation
X_c=data2[-row_val,]  #X_c, c pour calibration

N=length(X_c)
res=array(data=rep(0,N),dim=c(N,N,N))
for (i in 2:(N-2))
{
  for (j in (i+1):(N-1))
  {
    for (k in (j+1):N)
    {
      modele = lm(X_c[[1]] ~ X_c[[i]]*X_c[[j]]*X_c[[k]], data=X_c)
      selection=step(modele,direction="both")
      res[i,j,k] = PRESS(selection)   
    }
  }
}

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
