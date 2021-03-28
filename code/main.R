###########################
####### Main #############
###########################


# Setup -------------------------------------------------------------------

#clean environment
rm(list=ls())

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

#delete rows that are not of use.
X<-subset(X, select = -c(V1, id))

#check for balance of the criterion
table(X$satisfaction)
prop.table(table(X$satisfaction)) #No upsampling/downsampling required

#detect NAs
sapply(X, function(x) sum(is.null(x)))
sapply(X, function(x) sum(is.na(x))) #Arrival Delay in Minutes:310

#Is there a systematic relationship between NAs and criterion?
with(X, table(is.na(`Arrival Delay in Minutes`), satisfaction))[2,]

# 310 missing values - Are they all from the same flight?
onlyna<-X[which(is.na(X$`Arrival Delay in Minutes`)),]
max(table(onlyna$`Flight Distance`)) #No

#proportion of NAs in the dataset
with(X, prop.table(table(is.na(`Arrival Delay in Minutes`))))

#drop NA
X<-na.omit(X)

#delete variables which are no longer of use
rm(onlyna)

# Conversion --------------------------------------------------------------

#Convert all character variables into factors (for efficiency)
#Note: This step is specifically for data vizualisation, I might recode
#some variables as dummys for modelling later
for (i in 1:length(X)){
  if(is.character(X[,i])){
    X[,i]<-as.factor(X[,i])
  }
}


glimpse(X)
# descriptive statistics and visualisation --------------------------------

#make barplots for ordinal and nominal variables
ggbarplots<-list()
nominalordinal<-names(X)[c(1:2, 4:20)]
count=1
for (i in nominalordinal) {
  ggbarplots[[count]]<-ggplot(X, aes(x = X[,count])) +
    geom_bar() +
    facet_wrap(~satisfaction) +
    ggtitle(paste(i)) +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90))
  count=count+1
}

##### Nominal Variables #####
grid.arrange(ggbarplots[[1]], ggbarplots[[2]], ggbarplots[[3]], ggbarplots[[4]], nrow=2)

##### Ordinal Variables #####
grid.arrange(ggbarplots[[5]], ggbarplots[[6]], ggbarplots[[7]], ggbarplots[[8]], nrow=2)

  ##### intervall #####
  
  #Age
  ggplot(X, aes(x = Age))+
    geom_histogram() +
    facet_wrap(~satisfaction)
  
  #Flight Distance
  ggplot(X, aes(x = `Flight Distance`))+
    geom_histogram() +
    facet_wrap(~satisfaction)
  
  #Departure Delay in Minutes
  ggplot(X, aes(x = `Departure Delay in Minutes`))+
    geom_histogram() +
    facet_wrap(~satisfaction)
  
  #Arrival Delay in Minutes
  ggplot(X, aes(x = `Arrival Delay in Minutes`))+
    geom_histogram() +
    facet_wrap(~satisfaction)
  
