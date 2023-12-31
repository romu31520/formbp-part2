
library(targets)

tar_option_set(packages = c("arrow","dplyr", "ggplot2", "forcats", "gt"))

source("R/functions.R", encoding = "UTF-8")

list(
  tar_target(file_token, "secrets.yaml", format = "file"),
  tar_target(file_data, "individu_reg.parquet", format = "file"),
  tar_target(token, read_yaml_secret(path = file_token, key = "JETON_API")),
  tar_target(data, read_from_parquet(file_data), format = "parquet"),
  tar_target(clean_data, retraitement_donnees(data)),
  tar_target(table_age, produce_table_age(clean_data))
)

#Dans console R
# tar_visnetwork()
# tar_make()