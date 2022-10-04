#################################################
###   Ma premiere analyse de donnees avec R   ###
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

## ANalyse des variables nominales
# Sexe
head(d$sexe)
  # repartion / comptage en fonction des categories
t.sexe <- table(d$sexe)
t.sexe
prop.table(t.sexe) # Tableau de proportion / frequence
addmargins(t.sexe) # Ajouter des marges

freq(t.sexe) # Tableau de frequence en utilisant le package "questionr"
# ==> INTERPRETATION
# == % designe la frequence reelle
# == val% designe la frequence corrigee (les NA sont corriges)

freq(t.sexe, valid=FALSE, total = TRUE, sort = "dec", digits = 2) 



# GRAPHIQUES
# 1. Graphique simple avec l'option simple de R
pie(t.sexe) #diagramme circulaire
barplot(t.sexe) #diagramme en bar 
# ==> Un bon graphique presente des titres, legendes, libelles, etc. personnalises  (A FAIRE)

## Occupation
t.occup = table(d$occup)
freq(t.occup, valid = FALSE, total = TRUE, digits = 2)
pie(table(d$occup))



# II - NIVEAU D'ETUDE
table(d$nivetud)
t.nivetud <- ordered(d$nivetud)
freq(t.nivetud, total = TRUE, digits = 2)
par(
  # Modification des couleurs
  col.main="red", col.lab="blue", col.sub="black",
  # Mettre les titres en italique-gras
  font.main=4, font.lab=4, font.sub=4,
  # Modificcation de la taille de la police
  cex.main=2, cex.lab=1.7, cex.sub=1.2
)

barchart(table(t.nivetud), main="Diagramme de niveau d'etude",
         xlab="Niveaux d'etude",
         ylab="nombre de personnees")
barplot(table(t.nivetud))
# ==> freq cummule de maniere manuelle
p.nivetud = prop.table(table(t.nivetud))
cumsum(p.nivetud)
cumsum(t)

# ==> 
tab.nivetud <- cbind(t.nivetud, round(100*p.nivetud,2), round(100*cumsum(p.nivetud), 2))
colnames(tab.nivetud) <- c("Eff", "Freq(%)", "Freq.C(%)")
tab.nivetud <- addmargins(tab.nivetud, 1)
tab.nivetud[9,3] <- NA
tab.nivetud


####################
# Cbind empile les vecteur en colums 

# CREATION DE FONCTION
my.tab <- function(var) {
  effectifs  <-  table(var)
  proportions <-  prop.table(effectifs)
  cumuls     <-  cumsum(proportions)
  effectifs   <-  c(effectifs, sum(effectifs))
  proportions <-  round(100 * c(proportions, sum(proportions)), 2)
  cumuls      <-  round(100 * c(cumuls, NA), 2)
  tableau     <-  cbind(effectifs, proportions, cumuls)
  colnames(tableau) <-  c("Eff", "Freq(%)", "Freq.C(%)")
  return (tableau)
}

# Test de la fonction
my.tab(d$trav.satisf)


#NOMBRE DE FRERES ET SOEURS
my.tab(d$freres.soeurs)

d$fr.sr <- d$freres.soeurs
d$fr.sr[d$fr.sr > 5] <- "6+"
d$fr.sr <- ordered(d$fr.sr)
my.tab(d$fr.sr)

barchart(table(d$fr.sr), horizontal = FALSE)

#descriptives
summary(d$fr.sr)

#POIDS
my.tab(d$poids)

d$poids <- d$poids
#d$poids[d$poids > 5] <- "6+"
d$poids <- ordered(d$poids)
my.tab(d$poids)

barchart(table(d$poids), horizontal = FALSE)
table(d$poids)

prop.table(d$poids) # Tableau de proportion / frequence
addmargins(d$poids) # Ajouter des marges



