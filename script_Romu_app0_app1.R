






###################################################################
#### Le code à rectifier

# Environnement

library(dplyr)
library(MASS)
library(forcats)
library(yaml)


# Enlève les objets chargés, mais pas les packages chargés :
rm(list = ls())

# Antithèse de la reproductibilité : 
# le setwd, qui ne fait marcher le code que sur un poste !
# setwd("/home/onyxia/formation-bonnes-pratiques-R")

# Installer un package sur un poste d'utilisateur est invasif : ne pas faire !
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("stringr")) install.packages("stringr")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("tidyverse")) install.packages("tidyverse")

# Rajouté (partie 5 - gestion secret) : récup d'un token
API_TOKEN <- yaml::read_yaml("secrets.yaml")[["key"]]


###########################
# Définition de fonctions ----


decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %%
           10)
}

# fonction de stat agregee
fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = T, ...)
  } else if (b == "ecart-type" | b == "sd") {
    x <- sd(a, na.rm = T, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = T, ...)
  }
  return(x)
}

###########################

# Importe des données ----

df <- readr::read_csv2(
  "individu_reg.csv",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3", 
                 "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur")
)


###########################
# Retraitement des données ----
df <- df %>%
  mutate(aged = as.numeric(aged)) %>% 
  group_by(aged) %>% 
  summarise(nombre = n())


df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")



###########################
# Stat desc ----


# stats trans par statut
df3 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

# part d'homme dans chaque cohort
p <- 
  df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")

fonction_de_stat_agregee(df %>% filter(sexe == "Homme") %>% pull(aged))
fonction_de_stat_agregee(df %>% filter(sexe == "Femme") %>% pull(aged))


###########################
# Graphiques ----

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

ggsave("p.png", p)



# correction (qu'il faudra retirer)
# ggplot(
#   df %>% group_by(aged, sexe) %>% 
# summarise(SH_sexe = n()) %>% 
#   group_by(aged) %>% 
#   mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% 
#   filter(sexe==1)) + 
#   geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + 
#   geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat="identity", color = "red") + 
#   coord_cartesian(c(0,100))















# modelisation

# Ne pas laisse rdes appels de librairie en commentaire !
# Mettre les appels de librairie au début d'un code
# library(MASS)



df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
# Ajout de l'explicitation du package utilisé (dplyr) car conflit avec le package MASS
df3[, 1] <- factor(df3$surf, ordered = T)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)
MASS::polr(surf ~ cs1 + factor(ur), df3)
