# Exploration des résultats du premier tour des élections municipales de mars 2014 à Marseille, par bureau de vote 

# on charge les données (déjà préparées)
load(normalizePath("./marseille.Rdata"))

library(FactoMineR)

# on ne conserve que les variables qu'on veut inclure dans l'ACP, et o y ajoute l'arrondissement
df <- marseille[, c("Abstention", "Nuls", "Coppola", "Ravier", "Gaudin", "Mennucci", "CS2", "CS3", "CS4", "CS5", "CS6", "etrangers", "chomage", "HLM")]
df$arrdt <- factor(substr(marseille$BUREAU_ID, 4,5))
row.names(df) <- marseille$BUREAU_ID

# on calcule l'ACP
acp1 <- PCA(df, quanti.sup=7:14, quali.sup=15, graph=FALSE)

# on représente le cercle des corrélations
plot(acp1, choix="var")

# on peut représenter le nuage des individus, mais ce n'est pas très lisible
plot(acp1, choix="ind")

# on peut alors ne représenter que les arrondissements - qui sont les barycentres des sous-nuages de points
plot(acp1, choix="ind", invisible="ind")

# avec des ellipses de concentration qui représentent 95 % des points 
concat = cbind.data.frame(df$arrdt,acp1$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T, axes=c(1,2))
plot(acp1, choix="ind", axes=c(1,2), invisible = "ind", ellipse=ellipse.coord)


# cercle des corrélations pour les dimensions 1 et 3
plot(acp1, choix="var", axes=c(1,3))



# description des axes 1 à 3
dimdesc(acp1)

