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
# for (i in c(1:2, 4:5, 7:20)) {
#   ggbarplot<-ggplot(X, aes(x = X[,i])) +
#     geom_bar() +
#     facet_wrap(~satisfaction) +
#     ggtitle(paste(names(X)[i])) +
#     xlab("") +
#     theme(axis.text.x = element_text(angle = 90))
#   ggsave(paste0("..//plots//plot",i,".jpg"),ggbarplot)
# }

#make histograms for intervall variables
# for (i in c(3, 6, 21:22)) {
#   gghistogram<-ggplot(X, aes(x = X[,i])) +
#     geom_histogram() +
#     facet_wrap(~satisfaction) +
#     ggtitle(paste(names(X)[i])) +
#     xlab("") +
#     theme(axis.text.x = element_text(angle = 90))
#   ggsave(paste0("..//plots//plot",i,".jpg"),gghistogram)
# }

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

#Gender: 1 = Female; 2 = Male
levels(X$Gender)<-c(1:2)

#Customer Type: 1 = disloyal Customer; 2 = Loyal Customer
levels(X$`Customer Type`)<-c(1:2)

#Type of Travel: 1 = Business travel, 2 = Personal Travel
levels(X$`Type of Travel`)<-c(1:2)

#Class: 1 = Business; 2 = Eco; 3 = Eco Plus                               
levels(X$Class)<-c(1:3)

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

#Make new dataframe which does not include 0s
Xnozero<-X

#Convert all 0 to NA
for (i in 1:length(X)) {
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
# ggheatmap<-ggplot(data = cormat,
#        aes(x = Var1,
#            y = Var2,
#            fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "black", #negative coefficients
#                        high = "black", #positive coefficients
#                        mid = "white", #coefficients close to 0
#                        midpoint = 0,
#                        limit = c(-1,1),
#                        name="Spearman \n Correlation") +
#   geom_text(aes(y = Var2,
#                 x = Var1,
#                 label = value),
#             color = "black",
#             size = 3) +
#   guides(fill = guide_colorbar(barwidth = 7,
#                                barheight = 1,
#                                title.position = "top",
#                                title.hjust = 0.1)) +
#   coord_fixed() +
#   geom_text(aes(y = Var2,
#                 x = Var1,
#                 label = value),
#             color = "black", size = 3) +
#   guides(fill = guide_colorbar(barwidth = 7,
#                                barheight = 1,
#                                title.position = "top",
#                                title.hjust = 0.1)) +
#   ylab("") +
#   xlab("") +
#   ggtitle("Heatmap to visualize all correlations of the dataset") +
#   theme(axis.text.x = element_text(angle = 45,
#                                    vjust = 1,
#                                    hjust = 1),
#         legend.position = "none")

##save heatmap to the plots folder
# ggsave("..//plots//heatmap.jpg",
#        width = 10.24,
#        height = 10.16,
#        ggheatmap)


# Exploratory Factor Analysis ---------------------------------------------


# Preprocessing -----------------------------------------------------------


# Modelling ---------------------------------------------------------------


# Validation --------------------------------------------------------------


