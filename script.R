# Chaine de production sur le fichier recensement diffusé par l'Insee

# GESTION ENVIRONNEMENT ----------------------

library(dplyr)
library(ggplot2)
library(forcats)

library(arrow)

# install.packages("gt")
library(gt)

api_token <- yaml::read_yaml("secrets.yaml")$JETON_API


# FONCTIONS ---------------------------------
source("R/functions.R", encoding = "UTF-8")


# IMPORT DONNEES -----------------------------

df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2",
                 "cs3", "couple", "na38", "naf08", "pnai12", "sexe",
                 "surf", "tp", "trans", "ur")
)

# RETRAITEMENT --------------------------------
retraitement_donnees <- function(base) {
base_f <- base %>%
  mutate(aged = as.numeric(aged))

base_f$sexe <- base_f$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")

return(base_f)

}
# STATISTIQUES DESCRIPTIVES --------------------

summarise(group_by(df, aged), n())

stat_desc(df %>% filter(sexe == "Homme") %>% pull(aged))
stat_desc(df %>% filter(sexe == "Femme") %>% pull(aged))

## stats trans par statut =====================

df3 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

produce_table_age <- function(base) {
stats_age <- base %>%
  group_by(decennie = decennie_a_partir_annee(age)) %>%
  summarise(n())

table_age <- gt::gt(stats_age) %>%
  gt::tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  gt::fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  gt::cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )

return(table_age)
}

# GRAPHIQUES -----------------------------------

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(aged / 5)), stat = "count")

# part d'homme dans chaque cohort
p <- df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == "Homme") %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(
    aes(x = aged, y = SH_sexe),
    stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

ggsave("p.png", p)


# MODELISATION -------------------------------

df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")

df3 <- df3 %>%
  mutate(
    surf = factor(df3$surf, ordered = TRUE),
    cs1 = factor(cs1)
  )

MASS::polr(surf ~ cs1 + factor(ur), df3)
