#selection stepwise, apres la premiere elimination
#Ici on travaille sur le fichier FW_groupe5_obs
#D'abord supprimer de ce fichier les colonnes qu'on a selectionner dans Elimination

donnees<-read.table("FW_groupe5_obs.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(donnees)
N=length(donnees)

#deja eliminer les colonnes 
#quelque chose comme ca, a voir
for (i in 1:length(aEnlever))
{
  donnees2=donnees[,colnames(donnees)==aEnlever[i]]
}

#ensuite prendre 70% de l'echantillon pour faire la calibration (si pas assez on en prendra plus ou cross validation)
#sur les 30% restant pour la validation du modele. 
#sample(1:10,3,replace=T) tire 3 entiers entre 1 et 10, avec remise
row_val=sample(1:25,7)   #on garde 7 lignes pour la validation
X_c=donnees[-calibre_row,]  #X_c, c pour calibration

modele = lm(X_c[[1]] ~ X_c[[2]]*X_c[[3]]*X_c[[4]])
summary(modele)
step(modele,trace=TRUE,direction="both")  #a bien regarder


