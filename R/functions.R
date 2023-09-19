###Rq : la doc roxygen2 a été générée par chatgpt

# FONCTIONS ---------------------------------



library(dplyr)
library(ggplot2)
library(forcats)

## decennie_a_partir_annee ----


#' Calcul de l'année de début de décennie à partir d'une année donnée
#'
#' Cette fonction prend une année en entrée et renvoie l'année de début de la
#' décennie à laquelle cette année appartient. Par exemple, si vous fournissez
#' l'année 1987 en entrée, la fonction renverra 1980, car 1987 fait partie de la
#' décennie 1980.
#'
#' @param annee L'année dont vous souhaitez calculer l'année de début de décennie.
#'
#' @return L'année de début de la décennie à laquelle appartient l'année donnée.
#'
#' @examples
#' # Exemple d'utilisation de la fonction
#' decennie_a_partir_annee(1987) # Renvoie 1980
#' decennie_a_partir_annee(1995) # Renvoie 1990
#'
#' @export
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}




## stat_desc ----

#' Calculer des statistiques descriptives d'une variable
#'
#' Cette fonction permet de calculer différentes statistiques descriptives
#' d'une variable numérique.
#'
#' @param variable Le vecteur numérique dont vous souhaitez calculer les statistiques.
#' @param type_stat Le type de statistique à calculer. Peut prendre l'une des valeurs suivantes :
#'   - "moyenne" : Calcule la moyenne.
#'   - "ecart-type" ou "sd" : Calcule l'écart-type.
#'   - "variance" : Calcule la variance.
#' @param ... Des arguments supplémentaires qui seront passés aux fonctions mean(),
#'   sd(), ou var() de R.
#'
#' @return La valeur de la statistique calculée.
#'
#' @examples
#' # Calcul de la moyenne d'une variable
#' variable <- c(1, 2, 3, 4, 5)
#' stat_desc(variable, type_stat = "moyenne")
#'
#' # Calcul de l'écart-type d'une variable
#' variable <- c(1, 2, 3, 4, 5)
#' stat_desc(variable, type_stat = "ecart-type")
#'
#' # Calcul de la variance d'une variable
#' variable <- c(1, 2, 3, 4, 5)
#' stat_desc(variable, type_stat = "variance")
#'
#' @seealso
#' \code{\link{mean}}, \code{\link{sd}}, \code{\link{var}}
#'
#' @export

stat_desc <- function(variable, type_stat = "moyenne", ...) {
  if (type_stat == "moyenne") {
    x <- mean(variable, na.rm = TRUE, ...)
  } else if (type_stat == "ecart-type" || type_stat == "sd") {
    x <- sd(variable, na.rm = TRUE, ...)
  } else if (type_stat == "variance") {
    x <- var(variable, na.rm = TRUE, ...)
  }
  return(x)
}

stat_desc(rnorm(10))
stat_desc(rnorm(10), "ecart-type")
stat_desc(rnorm(10), "variance")



read_yaml_secret <- function(path, key) {
  return(yaml::read_yaml(path)[[key]])
}



read_from_parquet <- function(path) {
  df <- arrow::read_parquet(
    path,
    #Selection des variables de la table
    col_select  = c(
      "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
      "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
      "trans", "ur"
    )
  )
  return(df)
}

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}




retraitement_donnees <- function(df){
  df <- df %>%
    mutate(aged = as.numeric(aged))
  df$sexe <- df$sexe %>%
    as.character() %>%
    fct_recode(Homme = "1", Femme = "2")
  df <- df %>%
    mutate(
      surf = factor(surf, ordered = TRUE),
      cs1 = factor(cs1)
    )
  return(df)
}


produce_table_age <- function(df){
  stats_age <- df %>%
    group_by(decennie = decennie_a_partir_annee(aged)) %>%
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

