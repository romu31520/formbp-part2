

##### Installation des packages lintr et styler pour resp. repérer et rectifier 
### les bonnes pratiques de codage
install.packages("lintr")
install.packages("styler")

library(lintr)
library(styler)

#Pour définir les règles de bp (bonnes pratiques) comme étant celles du tidyverse
lintr::use_lintr(type = "tidyverse")

#Diagnostique
lintr::lint("script.R")

#Modification directe par le styler
styler::style_file("script.R")

#re-Diagnostique après passage du styler
lintr::lint("script.R")
