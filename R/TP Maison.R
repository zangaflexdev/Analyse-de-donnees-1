#################################################
###   Mon travail de maison                   ###
###   MASTER SI-SD / ESI / UNB                ###
###   Donne: 2 juin 2022                      ###
###   Fait: 03 Juin 2022                      ###
###   Presente: -- Juin 2022                  ###
###```By Zanga Flex                           ###
#################################################

# Chargement des packages
library(tidyverse)
library(questionr)
library(lattice)

#chargement des donnees hdv dans questionr
#help(package="questionr")
?hdv2003
data(hdv2003)

#mieux comprendre les donneees
View(hdv2003)
str(hdv2003)
names(hdv2003)

d <- hdv2003



# FONCTION DE REPARTITION(les proportions)
flex.props <- function(var) {
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

# OPTIONS APPLICABLE AUX GRAPHIQUES
par(
  # Modification des couleurs
  col.main="#FF0000", col.lab="blue", col.sub="black",
  # Mettre les titres en italique-gras
  font.main=4, font.lab=4, font.sub=4,
  # Modificcation de la taille de la police
  cex.main=2, cex.lab=1.7, cex.sub=1.2
)



# I- ANALYSE DES VARIABLES DE MESURES
## heures.tv  |   Poids |   age   |   freres.soeurs

  ### I-1- ANALYSE DES heures.tv
head(d$heures.tv)
str(d$heures.tv)

  ### I-1-1 proportions
my.tab(d$heures.tv)
t.htv <- d$heures.tv
t.htv <- ordered(t.htv)
t.htv.p <- flex.props(t.htv)
t.htv.p

my.tab(t.htv)


  ### I-1-2 GRAPHIQUE 1
barchart(table(t.htv), horizontal = FALSE, main="Graphique 1 -- Heure de travail",
         xlab="Heures de travail",
         ylab="Nombre de personnes")

t.htv
  ### I-1-3 CLASSIFICATION
t.htv.c <- cut(t.htv, c(0, 1, 2, 3, 6, 12),include.lowest = TRUE, right= TRUE,
                     labels = c("<1h", "1h-2h", "2h-3h", "3h-6","6h>"))
freq(t.htv.c)



  ### I-1-4 GRAPHIQUE 2
barchart(table(t.htv.c), horizontal = FALSE, main="Graphique 2 -- Heure de travail",
         xlab="Heures de travail",
         ylab="Nombre de personnes")

  ### I-1-5 Descriptives
### I-1-5-1 Moyenne
t.htv.c
mean(t.htvc, na.rm = TRUE)
median(d$heures.tv)
