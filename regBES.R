# chargement du dataset British Election Study 2010
BES2010 <- read.spss(normalizePath("./data/2010BESPreandPost.sav"), to.data.frame=TRUE, use.value.labels=TRUE)

# préparation du dataset
BES <- BES2010[, c("BQ12_2", "ZQ88", "ZQ89", "ZQ96", "ZQ101")]
BES$vote <- ifelse(BES$BQ12_2 %in% c("Labour", "Conservatives"), as.character(BES$BQ12_2), NA)
BES$vote <- factor(BES$vote)
BES$gender <- BES$ZQ88
BES$age <- as.integer(as.character(BES$ZQ89))
BES$income <- BES$ZQ96
BES$ethnicity <- BES$ZQ101
levels(BES$ethnicity) <- c(rep("Other",2), "White British", rep("Other", 16))

# estimation du modèle
regBES <- glm(vote ~ gender + age + income + ethnicity, data=BES, family = binomial(link="logit"))

# on regarde les résultats
summary(regBES)
library(texreg)
screenreg(regBES)

# un modèle un peu plus complexe

regBES2 <- glm(vote ~ gender*age*ethnicity + income, data=BES, family = binomial(link="logit"))
screenreg(list(regBES, regBES2))

# utiliser des odd ratios plutôt que les coefficients

exp(coefficients(regBES))
exp(coefficients(regBES2))
