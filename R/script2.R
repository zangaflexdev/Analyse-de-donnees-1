#################################################
###   JOUR 3 CORRECTION DES TP DE MAISON      ###
###   MASTER SI-SD / ESI / UNB                ###
###   03 juin 2022                            ###
###   By Zanga Flex                           ###
#################################################


# Chargement des packages
library(tidyverse)
library(questionr)
library(lattice)

#chargement des donnees hdv dans questionr
data(hdv2003)

d <- hdv2003


#

##################################### Analyse descriptive de la variable âge###################

## Histogramme pour visualiser la distribution
hist(d$age)
hist(d$age, freq = FALSE)
lines(density(d$age), lwd=2, col="red")# On peut considérer que la distribution est symétrique
boxplot(d$age)

# calcul des caractéristiques
summary(d$age)

max(d$age)
min(d$age)
me<-mean(d$age)
med<-median(d$age)
quantile(d$age, probs = c(0,0.25,0.5,0.75,1)) 

# Découpage en classes
?cut
d$classAge<-cut(d$age, breaks = c(18,20,40,60,80,100), include.lowest = TRUE, right = FALSE)
summary(d$classAge)

# GRAPHIQUES
histogram(~age |sexe, data=d)
densityplot(~age |sexe, data=d)
bwplot(~age |sexe, data=d)
bwplot(sexe~age, data=d)



# ANALYSE BIVARIEE (SEXE ET AGE)
t.effectif<-table(d$classAge,d$sexe)
t.effectif
t.prop<-prop.table(t.effectif)
addmargins(t.prop)
# LIGNES
t.propligne<-prop.table(t.effectif,margin=1)
addmargins(t.propligne,2)
barplot(t(t.propligne),beside =TRUE,col=1:2)
# COLONNES
t.propcolonne<-prop.table(t.effectif,margin=2)
addmargins(t.propcolonne,1)
barplot(t.propcolonne,beside =TRUE,col=1:5)




##################################### Analyse descriptive de la variable poids###################

## Histogramme pour visualiser la distribution
hist(d$poids)
hist(d$poids, freq = FALSE)
lines(density(d$poids), lwd=2, col="red")# On peut considérer que la distribution est symétrique
boxplot(d$poids)

# calcul des caractéristiques
summary(d$poids)

max(d$poids)
min(d$poids)
me<-mean(d$poids)
med<-median(d$poids)
quantile(d$poids, probs = c(0,0.25,0.5,0.75,1)) 

# Découpage en classes
d$classPoids<-cut(d$poids, breaks = c(0,4000,8000,12000,max(d$poids)), include.lowest = TRUE, right = FALSE, labels = c("<4000", "[4000,8000[", "[8000,12000[ ", "12000>"))
summary(d$classPoids)

# GRAPHIQUES
histogram(~poids |sexe, data=d)
densityplot(~poids |sexe, data=d)
bwplot(~poids |sexe, data=d)
bwplot(sexe~poids, data=d)



# ANALYSE BIVARIEE (SEXE ET POIDS)
t.effectif<-table(d$classPoids,d$sexe)
t.effectif
t.prop<-prop.table(t.effectif)
addmargins(t.prop)
# LIGNES
t.propligne<-prop.table(t.effectif,margin=1)
addmargins(t.propligne,2)
barplot(t(t.propligne),beside =TRUE,col=1:2, legend=TRUE,  main="Repartition d'hommes et femmes dans une classe",
        ylab="proportions")
# COLONNES
t.propcolonne<-prop.table(t.effectif,margin=2)
addmargins(t.propcolonne,1)
barplot(t.propcolonne,beside =TRUE,col=1:4, legend= TRUE,  main="Poids x Sexe",
        ylab="proportions")






##################################### Analyse descriptive de la variable HEURE.TV###################

## Histogramme pour visualiser la distribution
d$heures.tv <- ordered(d$heures.tv)
d$heures.tv
hist(d$heure.tv)
hist(d$heure.tv, freq = FALSE)
lines(density(d$heure.tv), lwd=2, col="red")# On peut considérer que la distribution est symétrique
boxplot(d$heure.tv)

# calcul des caractéristiques
summary(d$heure.tv)

max(d$heure.tv)
min(d$heure.tv)
me<-mean(d$heure.tv)
med<-median(d$heure.tv)
quantile(d$heure.tv, probs = c(0,0.25,0.5,0.75,1)) 

# Découpage en classes
d$classheure.tv<-cut(d$heure.tv[,paste(i, 0:1, sep = ".")],  c(0, 1, 2, 3, 6, 12),include.lowest = TRUE, right= FALSE,
                     labels = c("<1h", "1h-2h", "2h-3h", "3h-6","6h>"))
summary(d$classHeureTv)

# GRAPHIQUES
histogram(heure.tv |sexe, data=d)
densityplot(heure.tv |sexe, data=d)
bwplot(d$heure.tv |sexe, data=d)
bwplot(sexe~heure.tv, data=d)



# ANALYSE BIVARIEE (SEXE ET heure.tv)
t.effectif<-table(d$classHeureTv,d$sexe)
t.effectif
t.prop<-prop.table(t.effectif)
addmargins(t.prop)
# LIGNES
t.propligne<-prop.table(t.effectif,margin=1)
addmargins(t.propligne,2)
barplot(t(t.propligne),beside =TRUE,col=1:2, legend=TRUE,  main="Repartition d'hommes et femmes dans une classe",
        ylab="proportions")
# COLONNES
t.propcolonne<-prop.table(t.effectif,margin=2)
addmargins(t.propcolonne,1)
barplot(t.propcolonne,beside =TRUE,col=1:4, legend= TRUE,  main="heure.tv x Sexe",
        ylab="proportions")