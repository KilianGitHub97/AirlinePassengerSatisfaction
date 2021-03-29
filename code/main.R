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

# Vizualisation -----------------------------------------------------------

#make barplots for ordinal and nominal variables
for (i in c(1:2, 4:5, 7:20)) {
  ggbarplot<-ggplot(X, aes(x = X[,i])) +
    geom_bar() +
    facet_wrap(~satisfaction) +
    ggtitle(paste(names(X)[i])) +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90))
  ggsave(paste0("..//plots//plot",i,".jpg"),ggbarplot)
}

#make histograms for intervall variables
for (i in c(3, 6, 21:22)) {
  gghistogram<-ggplot(X, aes(x = X[,i])) +
    geom_histogram() +
    facet_wrap(~satisfaction) +
    ggtitle(paste(names(X)[i])) +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90))
  ggsave(paste0("..//plots//plot",i,".jpg"),gghistogram)
}

#delete variables which are no longer of use
rm(ggbarplot, gghistogram, i)


# Feature engineering -----------------------------------------------------



# Correlation -------------------------------------------------------------


