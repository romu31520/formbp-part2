
library(arrow)


#### Chargement du fichier


df <-
  aws.s3::s3read_using(
    FUN = readr::read_csv2,
    object = "diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv",
    bucket = "projet-formation",
    opts = list("region" = "")
  )
# Si ça marche pas, aller dans datalab > Mon compte > connexion stockage
#  > R paws > R(aws.S3), puis copier dans la console et exécuter la redéfiniton des variables d'environnement

# En parquet

arrow::write_parquet(df, "individu_reg.parquet")


# En csv
readr::write_csv2(df, "individu_reg.csv")
