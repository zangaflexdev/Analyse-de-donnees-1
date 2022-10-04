#################################################
###   Ma deuxieme analyse de donnees avec R   ###
###   MASTER SI-SD / ESI / UNB                ###
###   2er juin 2022                           ###
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



# OCCUPATION ET LE NIVEAU DETUDE
table(d$occup, d$nivetud)
t.nivetud <- d$nivetud
t.nivetud[t.nivetud = "Enseignement superieur y compris technique superieur"] <- "ES&TS"


# AGE ET SEXE
table(d$sexe, d$age)
str(d$sexe, d$age)
