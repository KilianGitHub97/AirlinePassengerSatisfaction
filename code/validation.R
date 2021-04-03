##############################
######### Validation #########
##############################


# Setup -------------------------------------------------------------------

#setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages (I think the only package that is needed here is caret)
source("packages.R")

#load data
X<-fread("")
X<-setDF(X)


# Preprocess: Just run the whole block ------------------------------------
X<-subset(X, select = -c(V1, id))
X<-na.omit(X)
for(i in 1:length(X)){
  if(is.character(X[,i])){
    X[,i]<-as.factor(X[,i])
  }
}
#Gender: 1 = Female; 2 = Male
levels(X$Gender)<-c(1:2)
#Customer Type: 1 = disloyal Customer; 2 = Loyal Customer
levels(X$`Customer Type`)<-c(1:2)
#Type of Travel: 1 = Business travel, 2 = Personal Travel
levels(X$`Type of Travel`)<-c(1:2)
#Class: 1 = Eco; 2 = Eco Plus; 3 = Business                               
levels(X$Class)<-c(3,1,2)
#satisfaction: 1 = neutral or dissatisfied; 2 = satisfied
levels(X$satisfaction)<-c(1:2)
# make factors to integers again
for(i in 1:length(X)){
  if(is.factor(X[,i])){
    X[,i]<-as.numeric(X[,i])
  }
}
X$satisfaction<-factor(X$satisfaction,
                       levels = c(1,2),
                       labels = c("neutralOrDissatisfied", "satisfied"))
#for caret spaces, "-" and "/" are not allowed.
#So delete these signs from the variable names 
names(X)<-gsub("\\s", "", names(X))
names(X)<-gsub("-", "", names(X))
names(X)<-gsub("/", "", names(X))

# Test on new data --------------------------------------------------------

#load models
decisiontree<-readRDS("..//models//decisiontree.rds")
gradientboost<-readRDS("..//models//gradientboost.rds")
randomforest<-readRDS("..//models//randomforest.rds")

#decisiontree
pred_dt<-predict(decisiontree, X[,-23])
confusionMatrix(X$satisfaction, pred_dt)

#randomforest
pred_rf<-predict(randomforest, X[,-23])
confusionMatrix(X$satisfaction, pred_rf)

#gradientboost
pred_gb<-predict(gradientboost, X[,-23])
confusionMatrix(X$satisfaction, pred_gb)
