###########################
####### Main #############
###########################


# Setup -------------------------------------------------------------------

#setwd to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#source other R-Files needed
source("packages.R")

#import data and make it a data.frame
X<-fread("..//data//train.csv")
X<-setDF(X)
class(X)


# Inspection --------------------------------------------------------------

#general overview
names(X)
summary(X)
sapply(X, typeof)
glimpse(X)
dim(X) #103904, 25

#check for balance of the criterion
table(X$satisfaction)
prop.table(table(X$satisfaction)) #No upsampling/downsampling required

#detect NAs
detna<-list()
for(i in 1:length(X)){
  detna[[i]] <- list("variable" = names(X)[i], 
                     "NA" = table(is.na(X[,..i])))
}

#filter the list for the columns that contain NAs (string: "TRUE")
names(X)[which(grepl("TRUE", detna))]

#How many NAs were in the "Arrival Delay in Minutes" variable?
detna[which(grepl("TRUE", detna))] #310

#Is there a systematic relationship between NAs and criterion?
with(X, table(is.na(`Arrival Delay in Minutes`), satisfaction)) #Not really


# Conversion --------------------------------------------------------------

#Convert all character variables into factors (for efficiency)
for (i in 1:length(X)){
  if(is.character(X[,i])){
    X[,i]<-as.factor(X[,i])
  }
}


# Preprocessing -----------------------------------------------------------




# descriptive statistics and visualisation --------------------------------
