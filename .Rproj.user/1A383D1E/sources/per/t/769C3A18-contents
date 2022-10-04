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



## Pour faire l'analyse on procede comme suit
## 1=> Histograme

data.age <- d$age

par(mfrow= c(1,1),
    # Modification des couleurs
    col.main="#FF0000", col.lab="blue", col.sub="black",
    # Mettre les titres en italique-gras
    font.main=4, font.lab=4, font.sub=4,
    # Modificcation de la taille de la police
    cex.main=2, cex.lab=1.7, cex.sub=1.2)

lines(density(data.age), lwd= 2, col="red")
hist(data.age, main= "Histogramme de l'age", xlab = "age", ylab = "nombre de personnes")
boxplot(data.age)

## 2 => Calcul des caracteristiques
summary(data.age)

max(data.age)
min(data.age)
mean(data.age)
median(data.age)
quantile(data.age, probs = c(0.25,0.5,0.75))


## 3 =>  Classification
###  criteres pour la classification
####  1- Quand on aucune informations, on fait des classes d'amplitudes egales
####  2- Classes d'effectifs egales
####  3- Classes selon le decoupage propre du domaine d'etude
data.classAge <- cut(data.age, breaks = c(min(data.age), 20, 40, 60, 80, max(data.age)))
summary(data.classAge)
