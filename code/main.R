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

#set.seed
set.seed(1997)

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
#rm(ggbarplot, gghistogram, i)


# Feature engineering -----------------------------------------------------

#make ordinal variables factors, which is easier to work with
for(i in 1:length(X)){
  if(is.character(X[,i])){
    X[,i]<-as.factor(X[,i])
  }
}
glimpse(X)

#save the data with labels to a new dataframe
#(X will from now on be numeric)
Xlabels<-X

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
glimpse(X)

# Correlation -------------------------------------------------------------

#Variables where 0 doesn't mean "no answer"
table(X$`Flight Distance` == 0)
table(X$`Arrival Delay in Minutes` == 0)
table(X$`Departure Delay in Minutes` == 0)

#Make new dataframe which does not include 0s
Xnozero<-X

#Convert all 0 to NA; leave out last 3 columns (since 0 doesn't mean "no answer")
for (i in 1:(length(Xnozero)-3)) {
  #if any value is zero, make it NA, else do nothing
  Xnozero[,i]<-ifelse(Xnozero[,i] == 0, NA, Xnozero[,i])
}

#Calculate Spearman correlations from between the independent variables
#and the dependent variable (NAs are not used for correlation)
yxcorrelation<-list()
for(i in 1:(length(Xnozero)-1)){
  yxcorrelation[[i]]<-data.table(y = "satisfaction",
                                 x = names(Xnozero)[i],
                                 cor = cor(y = Xnozero$satisfaction,
                                           x = Xnozero[,i],
                                           method = "spearman",
                                           use = "complete.obs"))
  
}

#make list a data.table
yxcorrelation<-rbindlist(yxcorrelation)

#check correlation with cohens interpretation criteria

# correlations higher than 0.5
yxcorrelation[which(cor > 0.5 | cor < -0.5)]
  
# correlations between 0.3 and 0.5
yxcorrelation[which(cor > 0.3 & cor < 0.5 | cor < -0.3 & cor > -0.5)]
  
# correlations between 0.1 and 0.3
yxcorrelation[which(cor > 0.1 & cor < 0.3 | cor < -0.1 & cor > -0.3)]
  
# items not associated with criterion
yxcorrelation[which(cor < 0.1 & cor > -0.1)]

##Heatmap

#prepare correlationmatrix
cormat<-round(cor(Xnozero, method = "spearman", use = "complete.obs"),2)
cormat<-melt(cormat)

#Make heatmap
ggheatmap<-ggplot(data = cormat,
       aes(x = Var1,
           y = Var2,
           fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "black", #negative coefficients
                       high = "black", #positive coefficients
                       mid = "white", #coefficients close to 0
                       midpoint = 0,
                       limit = c(-1,1),
                       name="Spearman \n Correlation") +
  geom_text(aes(y = Var2,
                x = Var1,
                label = value),
            color = "black",
            size = 3) +
  guides(fill = guide_colorbar(barwidth = 7,
                               barheight = 1,
                               title.position = "top",
                               title.hjust = 0.1)) +
  coord_fixed() +
  geom_text(aes(y = Var2,
                x = Var1,
                label = value),
            color = "black", size = 3) +
  guides(fill = guide_colorbar(barwidth = 7,
                               barheight = 1,
                               title.position = "top",
                               title.hjust = 0.1)) +
  ylab("") +
  xlab("") +
  ggtitle("Heatmap to visualize all correlations of the dataset") +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        legend.position = "none")

#save heatmap to the plots folder
# ggsave("..//plots//heatmap.jpg",
#        width = 10.24,
#        height = 10.16,
#        ggheatmap)

#delete unnecessary items
#rm(cormat, ggheatmap, yxcorrelation, i)

# Exploratory Factor Analysis ---------------------------------------------

#Packages used: FactoMineR, factoextra

#Perform Factor Analysis for Mixed Data (FAMD)
factoranalysis<-FAMD(base = Xlabels[,-23],
                     ncp = 5,
                     graph = FALSE)

#Check for proportion of variance
summary(factoranalysis)
get_eigenvalue(factoranalysis)

#screeplot
fviz_screeplot(factoranalysis)

# Plot the first two dimensions
fviz_famd_var(factoranalysis, repel = TRUE)


# Contribution to the first 5 dimensions
fviz_contrib(factoranalysis, "var", axes = 1)
fviz_contrib(factoranalysis, "var", axes = 2)
fviz_contrib(factoranalysis, "var", axes = 3)
fviz_contrib(factoranalysis, "var", axes = 4)
fviz_contrib(factoranalysis, "var", axes = 5)

#Plot of the quantitative variables
fviz_famd_var(factoranalysis, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

#Plot of the qualitative variables
fviz_famd_var(factoranalysis, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#delete variables that are no longer of use
#rm(factoranalysis)

# Modelling ---------------------------------------------------------------

######### Data preparation #########
####################################

#make new dataframe for modelling
Xmodeling<-X

#make criterion factors 
Xmodeling$satisfaction<-factor(Xmodeling$satisfaction,
                               levels = c(1:2),
                               labels = c("neutralOrDissatisfied", "satisfied"))

#for caret spaces, "-" and "/" are not allowed.
#So delete these signs from the variable names 
names(Xmodeling)<-gsub("\\s", "", names(Xmodeling))
names(Xmodeling)<-gsub("-", "", names(Xmodeling))
names(Xmodeling)<-gsub("/", "", names(Xmodeling))

#split the data randomly into test and train
splitrows<-sample(1:nrow(Xmodeling), round(nrow(Xmodeling)*0.8,0), replace = FALSE)
trainX<-Xmodeling[splitrows,]
testX<-Xmodeling[-splitrows,]

#specifiy cross validation technique (10-fold cross validation)
#for better robustness one could use repeated cross validation
#just specifie method = "repeatedcv" and add repeats = 5
#however, since i will use the same validation for all my models,
#I will not use repeated cv bc. it would exponentially increase
#computation times.
ctrlspecification<-trainControl(method = "cv",
                                number = 10,
                                savePredictions = "all", #also save class probability
                                classProbs = TRUE) #include probability of the class


######### Setup for parallel processing #########
#################################################

cluster <- makeCluster(8)
registerDoParallel(cluster)
getDoParWorkers()



######### Decision tree modeling and inspection #########
#########################################################

#get model parameters
getModelInfo()$rpart$parameters

#specifiy decision tree model
decisiontree<-train(satisfaction~.,
                    data = trainX,
                    method = "rpart", #decision tree
                    trControl = ctrlspecification) #use previously specified training method 

#show model key information on model
print(decisiontree)
(sum_dt<-summary(decisiontree$finalModel))

#predict labels of training data
trainpred_dt<-predict(decisiontree, trainX[,-23])

#confusion matrix for training data
confusionMatrix(trainX$satisfaction, trainpred_dt) #Acc = 0.8441, Kappa = 0.6794

#validate with test data
testpred_dt<-predict(decisiontree, testX[,-23])
confusionMatrix(testX$satisfaction, testpred_dt) #Acc = 0.8407, Kappa = 0.6718

#a priori probability
prop.table(table(X$satisfaction)) # 0.56 to beat

#visualisation of decision tree
prp(decisiontree$finalModel,
    tweak = 1.2,
    extra = 102)

#predictors used
plot(decisiontree)

#what predictors lead to prediction of criterion?
sum_dt$variable.importance

#save decisiontree to models folder
saveRDS(decisiontree, "..//models//decisiontree.rds")

######### Random forest modeling and inspection #########
#########################################################

#get model info
getModelInfo()$Rborist$parameters

#specify random forest model
#note, no formula was used, since this would make the 
#computation vastly slower.
randomforest<-train(y = Xmodeling$satisfaction,
                    x = Xmodeling[,-23],
                    method = "Rborist",
                    trControl = ctrlspecification,
                    do.trace = TRUE)

#show model key information on model
print(randomforest)
(sum_dt<-summary(randomforest))

#predict labels of training data
trainpred_rf<-predict(randomforest, trainX[,-23])

#confusion matrix for training data
confusionMatrix(trainX$satisfaction, trainpred_rf) #Acc = 0.9745 , Kappa = 0.9479

#validate with test data
testpred_rf<-predict(randomforest, testX[,-23])
confusionMatrix(testX$satisfaction, testpred_rf) #Acc = 0.9734 , Kappa = 0.9457

#a priori probability
prop.table(table(X$satisfaction)) # 0.56 to beat

#the model holds 2 parameters that were automatically tuned
#its interesting that the model provides best accuracy with
#when using all 22 predictors (predFixed)
randomforest$bestTune
plot(randomforest)

##save random forest to models folder
saveRDS(randomforest, "..//models//randomforest.rds")


######### gradient boosting trees modeling and inspection #########
###################################################################

#what are the parameters to tune?
getModelInfo()$gbm$parameters

#shrinkage and n.minobsinnode are held constant, so ill tune them manually
#note: in order to save computation time, the grid is not really sensitive
gradientboostgrid<-expand.grid(n.trees = c(150, 200, 250),
                               interaction.depth = c(3, 5, 7),
                               shrinkage = c(0.1, 0.125, 0.15),
                               n.minobsinnode = c(7, 10 , 15))

#specifie gradient boost model
gradientboost<-train(y = Xmodeling$satisfaction,
                     x = Xmodeling[,-23],
                     method = "gbm",
                     trControl = ctrlspecification,
                     tuneGrid = gradientboostgrid)

#show model key information on model
print(gradientboost)
(sum_dt<-summary(gradientboost))

#check for best parameters
gradientboost$bestTune

#predict labels of training data
trainpred_gb<-predict(gradientboost, trainX[,-23])

#confusion matrix for training data
confusionMatrix(trainX$satisfaction, trainpred_gb) #Acc = 0.9624, Kappa = 0.9232

#validate with test data
testpred_gb<-predict(gradientboost, testX[,-23])
confusionMatrix(testX$satisfaction, testpred_gb) #Acc = 0.9615, Kappa = 0.9212

#a priori probability
prop.table(table(X$satisfaction)) # 0.56 to beat

#save gradient boost to models folder
saveRDS(gradientboost, "..//models//gradientboost.rds")

######### Model Comparison #########
####################################

comparison<-resamples(list(Decision_Tree = decisiontree,
                           Random_Forest = randomforest,
                           Gradient_Boost = gradientboost
                           )
                      )

#summary, dotplot and barplot of the comparison
summary(comparison)
bwplot(comparison)
dotplot(comparison)

#End parallel
stopCluster(cluster)

#delete items that are no longer of use
# rm(cluster, comparison, ctrlspecification, gradientboostgrid,
#    sum_dt, splitrows,testpred_dt, trainpred_dt, testpred_rf,
#    trainpred_rf, testpred_gb, trainpred_gb)