#################################################
###   JOUR 2 ANALYSE BIVARIEE avec R          ###
###   MASTER SI-SD / ESI / UNB                ###
###   1er juin 2022                           ###
###   By Zanga Flex                           ###
#################################################

# Chargement des packages
library(tidyverse)
library(questionr)
library(lattice)

#chargement des donnees hdv dans questionr
help(package="questionr")
?hdv2003
data(hdv2003)

#mieux comprendre les donneees
View(hdv2003)
str(hdv2003)
names(hdv2003)

d <- hdv2003


# SEXE ET RELIGION
t.sexe <- d$sexe
freq(t.sexe)
t.relig <- d$relig
freq(t.relig)
## Tableau croise ou de contingence
t.eff <- table(t.relig, t.sexe)
addmargins(t.eff)
barplot(t.eff, col = 1:6)
barplot(t.eff, col = 1:6, beside = TRUE)
barplot(t.eff, col = 1:6, beside = TRUE, horiz = TRUE)

## tableau de freq
t.freq <- prop.table(t.eff)
addmargins(t.freq)
barplot(t.freq, col = 1:6, beside = TRUE)

## OPTIONS APPLICABLE AUX GRAPHIQUES
par(
  # Modification des couleurs
  col.main="#FF0000", col.lab="blue", col.sub="black",
  # Mettre les titres en italique-gras
  font.main=4, font.lab=4, font.sub=4,
  # Modificcation de la taille de la police
  cex.main=2, cex.lab=1.7, cex.sub=1.2
)

## Tableau de frequence marginales colonne
t.freq.c <- prop.table(t.eff, margin = 2) #margin 1 => ligne et 2 => column
addmargins(t.freq.c, margin = 1)
barplot(t.freq.c, col = 1:6, beside = TRUE)


## Tableau de frequence marginales ligne
t.freq.c <- prop.table(t.eff, margin = 1) #margin 1 => ligne et 2 => column
addmargins(t.freq.c, margin = 2)
barplot(t(t.freq.c), col =  c(4,2), main="Frequences marginales - lines",
        xlab="Religion",
        ylab="Sexe", ) # t(..) => pour faire la transposee ||| c(...) changer de couleur
