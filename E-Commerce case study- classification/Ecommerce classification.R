#################################################################################
#################CASE STUDY - ECOMMERCEWEB ANALYTICS#############################
#################################################################################
options(scipen = 999)
# Require Library 
library(rpart)
#install.packages("scholar", dependencies=T)
#install.packages("RCurl")
library("RCurl")
library(h2o)
library(RColorBrewer)
require("rattle")

# Improt file 
Etrain <- read.csv("/home/dipesh/BA /Final Projects/Data /5. ECOMMERCE CASE STUDY - CLASSIFICATION/train.csv")

Etest <- read.csv("/home/dipesh/BA /Final Projects/Data /5. ECOMMERCE CASE STUDY - CLASSIFICATION/test.csv")

names(Etrain)
#View(Etrain)
str(Etrain)
#stmmary <- str(Etrain)

Etrain <- Etrain[Etrain$target != -1,]
#Etrain1 <- Etrain[Etrain$target == -1,]
#Etrain2 <- Etrain[Etrain$target == 1,]
Etrain$target <- as.factor(Etrain$target)

# Function For numeric varibles
myNum <- function(x){
  
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
  
}

myChar <- function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  mode = mode(x)
  return(c(N=n, Nmiss=nmiss, Mode=mode))
  
}


numeric_vars <- names(Etrain)[sapply(Etrain, FUN=is.numeric)]
Charater_var <- names(Etrain)[sapply(Etrain,FUN =is.factor)]

summary_stats_Num = as.data.frame(t(sapply(Etrain[numeric_vars], FUN=myNum)))
summary_stats_Char = as.data.frame(t(sapply(Etrain[Charater_var], FUN=myChar)))
#View(summary_stats_Num)
#names(Etrain)
#summary_stats_Num1 <- summary_stats_Num[-t1]
write.csv(summary_stats_Num, file = "Ecommerce_class_stats_Num.csv")
write.csv(summary_stats_Char, file = "Ecommerce_class_stats_Char.csv")

#-------------------------------------------------------------------------
#Outlier and Missing Value Treatment
#-------------------------------------------------------------------------
BoxPlots <- function(x){
  boxplot(x)
}
Histrogram <- function(x){
  hist(x)
}
sapply(X = Etrain[,numeric_vars],FUN = BoxPlots)
Hist <- sapply(X = Etrain[,numeric_vars],FUN = Histrogram)
#install.packages("DataExplorer")
#require("DataExplorer")

#Missing Value Treatment (we do not have any missing value)
Etrain[,numeric_vars] <- apply(data.frame(Etrain[,numeric_vars]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
Etrain[,Charater_var] <- apply(data.frame(Etrain[,Charater_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


names(Etrain)

Etrain <- Etrain[complete.cases(Etrain),]
# outlier tratment :
outlier_treat <- function(x){
  
  UC1 = quantile(x, p=0.95,na.rm=T)
  LC1 = quantile(x, p=0.05,na.rm=T)
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  
  return(x)
}

Etrain_num_vars = data.frame(sapply(Etrain[numeric_vars], FUN=outlier_treat))
str(Etrain)
names(Etrain)
names(Etrain_num_vars)
#sapply(Etrain, class(Etrain))

Etrain1 <- Etrain[,numeric_vars]
target <- Etrain$target

Etest1 <- Etest[,numeric_vars]

Etrain_F <- as.data.frame(cbind(Etrain1,target))
View(Etrain_F)
Etrain_F$dayHourMinute <- NULL
Etest1$dayHourMinute <- NULL
names(Etrain_F)

#Splitting data into Training, Testing and validation Dataset
train_Samp <- sample(1:nrow(Etrain_F), size = floor(0.70 * nrow(Etrain_F)))

# set the seed to make your partition reproducible
set.seed(123)
training<-Etrain_F[train_Samp,]
testing<-Etrain_F[-train_Samp,]
rownames(training) <- rownames(testing) <- NULL


# Using H2O package 

localH2O <- h2o.init(nthreads = -1)

h2o.init()

Etest1.h2o <- as.h2o(Etest1)
Etrain_F.h2o <- as.h2o(Etrain_F)

y.dep <- 28
x.dep <- c(1:27)

#Random Forest
##te = Etest1 and tr = Etrain_F
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.dep, training_frame =Etrain_F.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1245)
)
#   user  system elapsed 
#  2.284   0.272 189.311 
rforest.model@parameters
h2o.performance(rforest.model)
#** Metrics reported on Out-Of-Bag training samples **
#MSE:  0.06705154
#RMSE:  0.2589431
#LogLoss:  0.2330527
#Mean Per-Class Error:  0.260253
#AUC:  0.8672352
#pr_auc:  0.4412804
#Gini:  0.7344703
#
#Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#           0     1    Error           Rate
#0       289210 26826 0.084883  =26826/316036
#1       13855 17950 0.435623   =13855/31805
#Totals 303065 44776 0.116953  =40681/347841

#Maximum Metrics: Maximum metrics at their respective thresholds
#metric threshold    value idx
#1                       max f1  0.176798 0.468785 194
#2                       max f2  0.112603 0.574478 262
#3                 max f0point5  0.243470 0.468852 138
#4                 max accuracy  0.366817 0.915530  64
#5                max precision  0.490564 0.830097   5
#6                   max recall  0.025136 1.000000 399
#7              max specificity  0.513928 0.999994   0
#8             max absolute_mcc  0.174710 0.412859 196
#9   max min_per_class_accuracy  0.107102 0.788087 270
#10 max mean_per_class_accuracy  0.105007 0.789294 273

h2o.confusionMatrix(rforest.model,Etrain_F.h2o)
#Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.176986952543955:
#            0     1    Error           Rate
#0      289502 26534 0.083959  =26534/316036
#1       13915 17890 0.437510   =13915/31805
#Totals 303417 44424 0.116286  =40449/347841
h2o.auc(rforest.model,Etrain_F.h2o= FALSE, Etest1.h2o = FALSE)
h2o.auc(rforest.model,Etest1.h2o)
?h2o.auc
#making predictions on test data
system.time(predict_for <- as.data.frame(h2o.predict(rforest.model, Etest1.h2o)))
#   user  system elapsed 
#  0.288   0.004  10.489 

predict_for <- as.data.frame(h2o.predict(rforest.model, Etest1.h2o))

table(predict_for$predict)
#  0     1 
#78653  8308 
h2o.confusionMatrix(predict_for$predict,Etest1.h2o_labe)

## Automatic machine learningfunction

automl <- h2o.automl(x = x.dep, y = y.dep,
                  training_frame = Etrain_F.h2o,
                  max_models = 15,
                  seed = 2)
automl@leaderboard
View(Etest1.h2o)

predict.automl <- as.data.frame(h2o.predict(automl, Etest1.h2o[,3:30]))
sub_automl <- as.data.frame(cbind(UID = Etest1$UID,score = predict.automl$predict))
  

h2o.shutdown(prompt=FALSE)




