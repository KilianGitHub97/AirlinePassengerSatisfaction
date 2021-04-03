###########################
####### Packages ##########
###########################
pkgs<-c(
  "tidyverse",
  "data.table",
  "janitor",
  "gridExtra",
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
