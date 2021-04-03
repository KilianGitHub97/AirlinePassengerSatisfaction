###########################
####### Packages ##########
###########################
pkgs<-c(
  "tidyverse",
  "data.table",
  "janitor",
  "knitr",
  "psych",
  "caret",
  "rpart.plot",
  "doParallel",
  "Rborist",
  "gbm",
  "FactoMineR",
  "factoextra"
)
lapply(pkgs, library, character.only = TRUE)
