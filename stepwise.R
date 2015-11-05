#selection stepwise, apres la premiere elimination
#Ici on travaille sur le fichier FW_groupe5_obs
#D'abord supprimer de ce fichier les colonnes qu'on a selectionner dans Elimination

donnees<-read.table("FW_groupe5_obs.csv",sep=";",dec=",",header=TRUE,row.names=1) 
attach(donnees)
N=length(donnees)

#quelque chose comme ca, a voir
for (i in 1:length(aEnlever))
{
  donnees2=donnees[,colnames(donnees)==aEnlever[i]]
}