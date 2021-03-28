###########################
####### Packages ##########
###########################
pkgs<-c(
  "tidyverse",
  "data.table",
  "janitor",
  "gridExtra",
  "knitr"
)
lapply(pkgs, library, character.only = TRUE)
