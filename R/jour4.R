#################################################
###   JOUR 3 CORRELATION DES VARIABLES        ###
###   MASTER SI-SD / ESI / UNB                ###
###   04 juin 2022                            ###
###   By Zanga Flex                           ###
#################################################


# Chargement des packages
library(tidyverse)
#chargement des donnees mtcar dans questionr
data("mtcars")
?mtcars

#creation de la table D
D <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
summary(D)


# MANIPULATION DES DONNEES AVEC dyplr
D
# filtrer ou selectionner les donnees
filter(D, am=="manual")
# selectionner des colonnes
select(D, vs, am)

select(filter(D, am=="manual"), vs, am)


# le pipe permet de faire plusieurs requetes en une commande
D %>% filter(mpg > 20) %>% select(mpg, hp)


D %>% 
  filter(am == "manual") %>% 
  select(vs, am)

# modifier ou creer des variables
D %>% 
  mutate(type="voiture")
  ## vitesse sup a 200 est grosse voiture
D %>%
  filter(hp>200) %>%
  mutate(type="Grosse voiture")

# EXERCICE
## Creer la colonne type qui combine le type de moteur et le nombre de cylindre
?cat
D %>%
  mutate(type = paste(vs, cyl, sep="-"))


# Visualisation des donnees avec ggplot2
ggplot(D, aes(x=mpg)) + geom_histogram()

ggplot(D, aes(x=mpg)) + geom_histogram(bins = 10, fill="khaki")

ggplot(D, aes(x=cyl)) + geom_bar()

ggplot(D) + geom_point(aes(x=wt, y=hp), color = "darkgreen", size= 3, alpha = .3)

ggplot(D) + 
  geom_boxplot(aes(x=vs, y=hp), color = "red", fill="wheat")


ggplot(D) + geom_point(aes(x=wt, y=hp,  color = vs), size= 3, alpha = 3)


ggplot(D) + geom_point(aes(x=wt, y=hp, shape=vs,  color = cyl), size= 3, alpha = 3)


D %>%
  mutate(type = paste(vs, cyl, sep="")) %>%
  ggplot() + 
  geom_point(aes(x=wt, y=hp, shape=vs,  color = cyl), size= 3) + 
  geom_text(aes(x=wt, y=hp, label=type,  color = cyl), nudge_y = 10)


D %>%
  mutate(type = paste(vs, cyl, sep="")) %>%
  ggplot(aes(x=wt, y=hp)) + 
  geom_point(aes(shape=vs,  color = cyl), size= 3) + 
  geom_text(aes(label=type,  color = cyl), nudge_y = 10) +
  geom_smooth(method = "lm")


################################################################################
####################         REGRESSION LINEAIRE         #######################
################################################################################
#FACTEURs EXPLIQUANT LA CONSOMMATION DU MOTEUR

ggplot(D) + 
  geom_boxplot(aes(x=cyl, y=mpg), fill= "wheat", color="red")
# => la consommation depend du nombre de cylindre

ggplot(D) + 
  geom_point(aes(x=hp, y=mpg), fill= "wheat", color="red")
# la consommation depend de la puissance

#modele simple comparer a 2 variables
cor(D$hp, D$mpg)  # correlation lineaire de pearson
cor(D$hp, D$mpg, method = "pearson")  # correlation lineaire de pearson
cor(D$hp, D$mpg, method = "spearman")  # correlation lineaire de pearson

# Expliquer mpg par rapport a hp
mod1 <- lm(mpg ~ hp, data=D)
summary(mod1) 
#ainsi on a 
# mpg = alpha + beta*hp
#dans notre cas alpha=30 et beta= -0.06823


mod2 <- lm(mpg ~ vs, data=D)
summary(mod2) 

# regression mmutiple
mod3 <- lm(mpg ~ vs + hp + wt + cyl, data=D)
summary(mod3) 





###############################################################################
########################### RECAPITULATIF #####################################
###############################################################################
# => 2 var de mesures
## tracer le nuage de points
## visualiser et remaquer sil ya une regression
## faire la regression