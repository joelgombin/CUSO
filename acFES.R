# Exploration de l'enquête French electoral study de 2012 (http://www.cee.sciences-po.fr/fr/recherche/les-analyses-electorales/lenquete-electorale-francaise-2012.html)

load(normalizePath("./data/FES2012.Rdata"))

# on nettoie un peu les variables qu'on veut utiliser

levels(FES2012$v3)[11:14] <- NA
levels(FES2012$v3)[c(1,2,10)] <- "Else"
levels(FES2012$sd46)[7:9] <- NA
levels(FES2012$sd46)[2:5] <- "Else"

# on calcule l'AFC
ac1 <- CA(table(FES2012[, c("v3", "sd46")]), graph=FALSE)

# on représente l'inertie des premiers facteurs
barplot(ac1$eig[,2], names.arg=row.names(ac1$eig))

# on représente le plan factoriel
plot(ac1, autoLab="no")

# on peut caractériser les facteurs
desc <- dimdesc(ac1, axes=1:2)
desc


# pour analyser plus largement ce jeu de données, on va l'étudier en ACM

FES2012$sd14_pcs <- factor(FES2012$sd14_pcs, labels=c("Agriculteur", "Artisan", "Commerçant", "Chef d'entreprise", "Prof. libérale", "Cadre du public", "Cadre du privé", "Prof. interm. public", "Prof. interm. privé", "Technicien", "Contremaître", "Employé public", "Employé adm privé", "Employé commerce", "Service aux particuliers", "Ouvrier qualifié", "Ouvrier non qualifié", "Ouvrier agricole", "888", "Inactif"))
levels(FES2012$sd14_pcs)[19] <- NA

# calcul de l'ACM, avec mise en variable supplémentaire (ou illustrative) du vote
acm1 <- MCA(FES2012[,c("h1", "v3", "sd46", "sd14_pcs")], quali.sup=2, graph=FALSE, na.method="Average")

# représentation de la variance des premiers facteurs
barplot(acm1$eig[1:5,2], names.arg=row.names(acm1$eig[1:5,]))

# représentation du nuage de modalités dans le premier plan factoriel
plot(acm1, invisible="ind", autoLab="no")

# caractériser les facteurs
desc <- dimdesc(acm1, axes=1:2)
desc


## Exercice : explorer d'autres dimensions du jeu de données FES2012, en utilisant le codebook (dans le dossier data) pour repérer les variables intéressantes. 


## CAH

hcpc <- HCPC(acm1, nb.clust=-1, graph=FALSE)
plot(hcpc, choice = "tree", tree.barplot=FALSE)
plot(hcpc, choice = "map", tree.barplot=TRUE, draw.tree = TRUE, ind.names = FALSE)

