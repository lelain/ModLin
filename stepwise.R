#selection stepwise, apres la premiere elimination
#Ici on travaille sur le fichier FW_groupe5_obs
#D'abord supprimer de ce fichier les colonnes qu'on a selectionner dans Elimination

# definition de la fonction PRESS
PRESS <- function(model) {
  pr <- residuals(model)/(1-lm.influence(model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

donnees<-read.table("FW_groupe5_obs.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(donnees)
N=length(donnees)

#On garde les bonnes colonnes 
data2=subset(donnees,select=aGarder)

#ensuite prendre 70% de l'echantillon pour faire la calibration (si pas assez on en prendra plus ou cross validation)
#sur les 30% restant pour la validation du modele. 
row_val=sample(1:25,7)   #on garde 7 lignes pour la validation
X_v=data2[row_val,]   #X_v, v pour validation
X_c=data2[-row_val,]  #X_c, c pour calibration

N=length(X_c)
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
      modele = lm(X_c[[1]] ~ X_c[[i]]*X_c[[j]]*X_c[[k]], data=X_c)
      selection=step(modele,direction="both")
      res[i,j,k] = PRESS(selection)   
    }
  }
  compte=c()
}

#resultats :
Res_ind=which(res==min(res[res>0]),arr.ind=T)
modele=lm(X_c[[1]]~X_c[[Res_ind[1]]]*X_c[[Res_ind[2]]]*X_c[[Res_ind[3]]],data=X_c)
modele_final=step(modele,direction="both")

#Test sur l'echantillon de validation
#On garde les colonnes qu'il faut
X_v=X_v[Res_ind]
Predict = modele_final$coef[1] + modele_final$coef[2]*X_v[[1]] + 
  modele_final$coef[2]*X_v[[2]] + modele_final$coef[3]*X_v[[3]] + 
  modele_final$coef[2]*X_c[[1]] modele_final$coef[2]*X_c[[1]]

