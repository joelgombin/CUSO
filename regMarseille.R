# chargement des packages utiles

# install.packages("dplyr")
library(dplyr)

# Chargement des données 

# normalisePath ets utilisé pour que le chemin soit valable quel que soit le système d'exploitation (Win, MacOS, Unix)
bvINSEE <- read.csv(normalizePath("./data/bvINSEE2012.csv"), sep=";", stringsAsFactors=FALSE, check.names = FALSE)
# les identifiants des bureaux de vote sont codés différemment dans les deux jeux de données, on harmonise cela
bvINSEE$BUREAU_ID <- gsub("_", "", bvINSEE$BUREAU_ID)
marseille <- read.csv(normalizePath("./data/complet_par_bureaux.csv"), sep=",", stringsAsFactors=FALSE, check.names = FALSE, fileEncoding = "UTF-8")

marseille <- tbl_df(marseille)

# Nettoyage des données

marseille <- marseille %>%
  mutate( # on met les numéros de BV au bon format
    BUREAU_ID = paste0("132", substr(sprintf("%04.0f",bdv), 1, 2), sprintf("%04.0f",bdv)),
    Inscrits = inscrits,
    Abstention = inscrits - Nuls - `Total Résultat`,
    Exprimés = `Total Résultat`,
    Diouf = `CHANGER LA DONNE`,
    AEI = `ALLIANCE ECOLOGISTE INDEPENDANTE`,
    POI = `LISTE D UNITE ET DE RESISTANCE CONTRE LA POLITIQUE DU GOUVERNEMENT ET DE L U.E SOUTENUE PAR LE POI`,
    Coppola = `MARSEILLE A GAUCHE, L HUMAIN D ABORD DANS NOTRE VILLE`,
    Assante = `MARSEILLE A VIVRE`,
    Ravier = `MARSEILLE BLEU MARINE`,
    Gaudin = `MARSEILLE EN AVANT AVEC JEAN-CLAUDE GAUDIN`,
    MarseilleEnsemble = `MARSEILLE ENSEMBLE`,
    Persia = `MARSEILLE J Y CROIS`,
    MarseillePopulaire = `MARSEILLE POPULAIRE`,
    MarseilleUnie = `MARSEILLE UNIE`,
    Mennucci = `UN NOUVEAU CAP POUR LES MARSEILLAIS AVEC PATRICK MENNUCCI`,
    Qualite = `UNE QUALITE DE VIE POUR TOUS`,
    UnionMarseille = `UNION POUR MARSEILLE`
  )
# quand une cellule est vide : mettre une valeur égale à zéro
marseille[is.na(marseille)] <- 0

# On transforme les résultats en % des inscrits

marseille <- marseille %>% 
  mutate(Nuls = Nuls / Inscrits * 100,
         Abstention = Abstention / Inscrits * 100,
         Diouf = Diouf / Inscrits * 100,
         Coppola = Coppola / Inscrits * 100,
         Ravier = Ravier / Inscrits * 100,
         Gaudin = Gaudin / Inscrits * 100,
         Mennucci = Mennucci / Inscrits * 100)

# on fusionne les résultats électoraux avec les données de l'INSEE

marseille <- merge(marseille, bvINSEE, by = "BUREAU_ID")

# on transforme les données INSEE en %

marseille <- marseille %>%
  mutate(CS1 = C09_ACT1564_CS1 / P09_POP1564 * 100,
         CS2 = C09_ACT1564_CS2 / P09_POP1564 * 100,
         CS3 = C09_ACT1564_CS3 / P09_POP1564 * 100,
         CS4 = C09_ACT1564_CS4 / P09_POP1564 * 100,
         CS5 = C09_ACT1564_CS5 / P09_POP1564 * 100,
         CS6 = C09_ACT1564_CS6 / P09_POP1564 * 100,
         etrangers = P09_POP_ETR / P09_POP * 100,
         chomage = P09_CHOM1564 / P09_POP1564 * 100,
         HLM = P09_NPER_RP_LOCHLMV / P09_POP * 100)

# enfin, on estime le modèle

modele1 <- lm(Ravier ~ CS2 + CS3 + CS4 + CS5 + CS6 + etrangers + chomage + HLM, data = marseille)

# on regarde les résultats du modèle

summary(modele1)

# on regarde les résultats du modèle, avec une interface plus lisible

# install.packages("texreg")
library(texreg)
# l'option single.row permet de faire tenir le coefficient et son erreur-type sur la même ligne
screenreg(modele1)


# on peut ensuite analyser un élément donné du modèle, par exemple les résidus

summary(modele1$residuals)
plot(density(modele1$residuals))
# une altenative avec ggplot2
library(ggplot2)
# fortify permet de transformer l'objet lm en dataframe utilisable par ggplot2
ggmodele1 <- fortify(modele1)
ggplot(modele1, aes(x=.resid)) + geom_density(color="blue") + theme_bw()

# ou encore les valeurs prédites (fitted values), par exemple 
# on charge le fonds de carte de Marseille
load(normalizePath("./data/marseilleSHP.Rdata"))
# on prépare le jeu de données pour ggplot2
ggmarseilleSHP <- fortify(marseilleSHP, region="ID")
ggmarseilleSHP$pred <- modele1$fitted.values[match(ggmarseilleSHP$id, marseille$BUREAU_ID)]

# on prépare le thème

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")

ggplot(ggmarseilleSHP, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=pred)) + 
  coord_fixed() + 
  scale_fill_gradient(guide="legend", name="Valeurs prédites", low="white", high="blue") + 
  labs(x=NULL, y=NULL) + 
  new_theme_empty


# même carte mais avec les résidus
ggmarseilleSHP$res <- modele1$residuals[match(ggmarseilleSHP$id, marseille$BUREAU_ID)]
ggplot(ggmarseilleSHP, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=res)) + coord_fixed() + scale_fill_gradient2(guide="legend", name="Résidus", low="blue", high="red", mid="white") + labs(x=NULL, y=NULL) + new_theme_empty

# nouveau modèle, avec effets d'interaction

modele2 <- lm(Ravier ~ CS2 + CS3 + CS4 + CS5 + CS6*etrangers + CS6*chomage + HLM, data = marseille)
screenreg(list(modele1, modele2))

## attention : ce modèle présente une forte multicollinéarité !
library(car)
vif(modele2)

newdata <- expand.grid(CS6 = quantile(marseille$CS6, probs=seq(0,1,length.out=10), na.rm=TRUE), CS2 = mean(marseille$CS2, na.rm=TRUE), CS3 = mean(marseille$CS3, na.rm=TRUE), CS4 = mean(marseille$CS4, na.rm=TRUE), CS5 = mean(marseille$CS5, na.rm=TRUE), etrangers = quantile(marseille$etrangers, probs=seq(0,1,0.25), na.rm=TRUE), chomage = mean(marseille$chomage, na.rm=TRUE), HLM = mean(marseille$HLM, na.rm=TRUE))
newdata$predict <- predict(modele2, newdata=newdata)

newdata %>%
  ggplot(aes(x = CS6, y = predict, group = etrangers)) +
  geom_line(aes(color = as.factor(etrangers))) +
  scale_color_discrete(name = "proportion d'étrangers", labels = c("minimum", "1er quartile", "2e quartile", "3e quartile", "maximum")) +
  xlab("Ouvriers") +
  ylab("Score FN prédit") +
  theme_bw()

# modèle avec prise en compte des arrondissements

marseille$arrdt <- substr(marseille$BUREAU_ID, 4,5)
library(lme4)
modele3 <- lmer(Ravier ~ CS2 + CS3 + CS4 + CS5 + CS6*etrangers + CS6*chomage + HLM + (1 | arrdt), data = marseille)

screenreg(list(modele1, modele2, modele3))

modele4 <- lmer(Ravier ~ CS2 + CS3 + CS4 + CS5 + HLM + (0 + CS6 | arrdt), data = marseille)

screenreg(list(modele1, modele2, modele3, modele4))

ranef(modele3)
ranef(modele4)
