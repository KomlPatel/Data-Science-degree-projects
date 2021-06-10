#--------- Proactive Attrition Management-----------------------------------------------
#-----------------------------Logistic Regression Case Study--------------------------#

# Working Directory 
setwd("/home/dipesh/BA /Final Projects")

# Import Data 
Proactive <- read.csv("/home/dipesh/BA /Final Projects/Data /Proactive Attrition Management-Logistic Regression Case Study/Proactive Attrition Management-Logistic Regression Case Study.csv")

#View(Proactive)
names(Proactive)
ncol(Proactive)
nrow(Proactive)
head(Proactive)
str(Proactive)
summary(Proactive)

# convert numeric to factors 
for(i in c(23:26,30,34:64,67,68,72,74,76:78)) {
  Proactive[, i] <- as.character(Proactive[, i])
}

class(Proactive$CHURNDEP)

#User defined function to create Data audit report for numerical variables

mystats_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  cv = sd(x, na.rm=T)/mean(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, cv=cv, var=var, range=range, pctl=pctl))
}

#User defined function to create Data audit report for categorical variables
mystats_cat = function(x){
  nmiss<-sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  return(c(nmiss=nmiss,Nmiss_Pct= nmiss_pct))
}

#Implementing the function on one variable at a time
#Separating all the numeric & categorical variables to treat them
numeric_vars = names(Proactive)[sapply(Proactive, FUN=is.numeric)]
cat_vars = names(Proactive)[sapply(Proactive, FUN=is.character)]

#Data Audit Report for numerical variables
summary_stats = t(apply(Proactive[numeric_vars], 2, FUN=mystats_num))
#View(summary_stats)
write.csv(summary_stats, file = "Log_stats_numeric_vars.csv")

#Data Audit Report for categorical variables
summary_stats_cat = t(apply(Proactive[cat_vars], 2, FUN=mystats_cat))
#View(summary_stats_cat)
con <- file("stats_cat_vars.txt")
#View(summary_stats_cat)
write.csv(summary_stats_cat, file = "Log_stats_cat_vars.csv")

sink(con, append=TRUE)
#sink(con, append=TRUE, type="message")
summary_stats_cat
sink()

#--------------------------------------------------------------------------------------
#Outlier treatment

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.95,na.rm=T)
  LC1 = quantile(x, p=0.05,na.rm=T)
  #UC1 = mean(x,na.rm=T) + 3*sd(x, na.rm=T)
  #LC1 = mean(x,na.rm=T) - 3*sd(x, na.rm=T)
  
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  #x[x>UC1]=UC1
  #x[x<LC1]=LC1
  return(x)
  
}

mydata_num = data.frame(apply(Proactive[numeric_vars], 2, FUN=outlier_treat))
#View(mydata_num)

#Number of missings in each variable
sapply(Proactive, FUN=function(x) sum(is.na(x)))

#---------- Missing value treatment for numerical variables----------------------------------
# we have maximum 1.8 % of missing value so we can simply drop them 

mydata_num <- mydata_num[complete.cases(mydata_num),]
#mydata_num <- na.omit(mydata_num)
#(mydata_num[is.na(mydata_num),])
str(mydata_num)

# Missing value treatment for categorical variables

# Division of data for Testing and Pridicting-----------------------------
# Hear we have two data set for buiding a model (Calibration Data Set) and using this model we have to pridict 
# test data set (Validation Data Set).

##------------ Validation Data Set----------------------------------------

Validation <- Proactive[is.na(Proactive$CHURNDEP),]

#View(Validation)
#nrow(Validation)

##----------- Calibration Data Set------------------------------------

calibration  <- Proactive[!is.na(Proactive$CHURNDEP),]



mydata_cat = na.omit(cat_vars)
mydata_cat <- as.data.frame(mydata_cat)
mydata_cat[is.na(mydata_cat),]
View(mydata_cat)

#levels(mydata_cat)
#unique(mydata_cat)

summary(mydata_num)
str(mydata_cat)
names(mydata_num)
mydata_num <- mydata_num[-c(29,76)]
#mydata_num <- 
#mydata_num[is.na(mydata_num),]
cor_mat <- cor(mydata_num)

write.csv(cor_mat,"Log_cor_mat.csv")
#View(cor_mat)
names(mydata_cat)

#names(mydata_num)
#mydata1 = cbind(mydata_num,mydata_cat)
mydata1 = mydata_num
#View(mydata1)
ncol(mydata1)
names(mydata1)

# Factor Analysis for variable reduction 

mydata1 <- mydata1[-c(29,76)]

#mydata1 <- mydata1[!is.na(mydata1),]

corr <- cor(mydata1)
View(corr)
require(psych)


sum(mydata1$CHURNDEP)
names(mydata1)

new_mydata = mydata1[,-c(76,77,78)]

new_mydata=new_mydata[complete.cases(new_mydata),]

#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(new_mydata), size = floor(0.70 * nrow(new_mydata)))

train <-new_mydata[train_ind,]
test <-new_mydata[-train_ind,]
rownames(train) <- rownames(test) <- NULL


## FACTOR ANALYSIS 
corrm<- cor(train)                                 ### CORRELATION MATRIX

#View(corrm)
eigen(corrm)$values 

require(psych)
setInternet2(TRUE)
install.packages("GPArotation")
install.packages('GPArotation', dependencies=TRUE, repos='http://cran.rstudio.com/')
require('caret')
#require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)
options(scipen = 999)
scree(corrm, factors=F, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

#View(eigen_values)

write.csv(eigen_values, "Log_EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY

#FA<-fa(r=corrm,nfactors = 10)   
FA=principal(r=corrm,nfactors = 10,rotate = "varimax")

### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
#ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(train),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

#View(Loadings)

write.csv(Loadings, "log_loadings.csv") ### SAVING THE FILE
a=prcomp(corrm)
#View(a)

#MOU OPEAKVCE AGE1 CREDITCD SETPRCM RETCALL MONTHS MARRYNO CHURN

train$CHURNDEP=as.factor(train$CHURNDEP)

# Drop some varible which are not co-related with CHURNDEP
fit1 <- glm(CHURNDEP~MOU+MAILRES+PEAKVCE+AGE2+SETPRCM+INCMISS+RETCALLS+CREDITA+MARRYUN+
              AGE1+SETPRCM+MONTHS+ACTVSUBS+TRUCK+CHANGEM,data = train,
            family = binomial)
summary(fit1)
# Drop those variable which are not more significant to our pridicted variable
fit2<- glm(CHURNDEP~MOU+MAILRES+SETPRCM+RETCALLS+AGE1+
             SETPRCM+MONTHS+ACTVSUBS+CHANGEM,data = train,
           family = binomial)
summary(fit2)
#cor(as.numeric(as.character(train$CHURNDEP)),(train$CREDITCD))
#cor(as.numeric(as.character(train$CHURNDEP)),(train$CREDITA))

praid <- predict(fit1, type = 'response')
train1<- cbind(train, Prob=predict(fit1, type="response")) 

praid1 <- predict(fit2, type = 'response')
train11 <- cbind(train, Prob=predict(fit2, type="response")) 


require(InformationValue)
Concordance(train11$CHURNDEP, train11$Prob)

somersD(train11$CHURNDEP, train11$Prob)

plotROC(test$CHURNDEP, praid)

table(train1$CHURNDEP)
AUROC(train1$CHURNDEP, train1$Prob)

table(train11$CHURNDEP)
AUROC(train11$CHURNDEP, train11$Prob)

#?optimalCutoff

cut1<-optimalCutoff(train11$CHURNDEP, train11$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)


ROCTable<-data.frame(cut1$sensitivityTable)
# View(ROCTable)

train11$pred_y = ifelse(train11$Prob>0.48,1,0)
#View(train11)

confusionMatrix(train11$CHURNDEP, train11$pred_y)

#require(dplyr)
#write.csv(ROCTable, "Log_ROCTable.csv")

plotROC(train11$CHURNDEP, train11$Prob, Show.labels=F)

# AUC = concordance + 0.5 Tied

ks_table<-ks_stat(train11$CHURNDEP, train11$Prob, returnKSTable=TRUE)
write.csv(ks_table, "ks_table.csv")

ks_stat(train11$CHURNDEP, train11$Prob, returnKSTable=FALSE)
#?ks_stat


#validate our model
test1 <- cbind(test, Prob=predict(fit2,newdata=test, type="response")) 
#View(test1)
Concordance(test1$CHURNDEP, test1$Prob)
somersD(test1$CHURNDEP, test1$Prob)

#test1$pred_y = ifelse(test1$Prob>0.510,1,0)
test1$pred_y = ifelse(test1$Prob>0.480,1,0)

confusionMatrix(test1$CHURNDEP, test1$pred_y)
#View(test1)

ks_table<-ks_stat(test1$CHURNDEP, test1$Prob, returnKSTable=FALSE)

t(confusionMatrix(test1$CHURNDEP, test1$Prob, threshold=0.25))

sum(test1$CHURNDEP)

plotROC(test1$CHURNDEP, test1$Prob, Show.labels=F)

#--------------------------------------------------------------------------------
# We are applying this model to validation data set

Validation1 <- predict(fit2,data= Validation, type = 'response')
Validation11 <- cbind(Validation, Prob=predict(fit2, type="response"))

View(Validation1)
Concordance(Validation1$CHURNDEP, train11$Prob)

somersD(train11$CHURNDEP, train11$Prob)












#----------------------------------------------------------------------------------------------------
#
#
#



#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(mydata1), size = floor(0.70 * nrow(mydata1)))

training<-mydata1[train_ind,]
testing<-mydata1[-train_ind,]
rownames(training) <- rownames(testing) <- NULL

#Building Models for training dataset by using step vise regression method

names(training)
ncol(training)
# 1st reduce chategorical varible and make (n-1) dummay varible 
training <- training[-c(29,39,42,53,57,60,67,75,76)]


summary(training)
str(training)

# Build model on all training data set variables.

fit1<-glm(CHURNDEP~.
          ,data = training,
          family = binomial)

summary(fit1)
#    Null deviance: 3.8816e+04  on 27999  degrees of freedom
# Residual deviance: 1.6244e-07  on 27932  degrees of freedom
# AIC: 1528
# Number of Fisher Scoring iterations: 25

# Rendamly select top varibles check perfomace of model:

fit2 <- glm(CHURNDEP~REVENUE+MOU+RECCHRGE+OVERAGE+DIRECTAS+ROAM+CHANGEM+CHANGER+DROPVCE+BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+
              MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+DROPBLK+CALLFWDV,data = training,
            family = binomial)
summary(fit2)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 38372  on 27979  degrees of freedom
#AIC: 38401


# Drop some varible which are not co-related with CHURNDEP
fit3 <- glm(CHURNDEP~REVENUE+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+CUSTCARE+THREEWAY+INCALLS+PEAKVCE+CALLWAIT+
              MONTHS+UNIQSUBS+ACTVSUBS+PHONES+MODELS+EQPDAYS+AGE1+AGE2+CHILDREN
            ,data = training,
            family = binomial)
summary(fit3)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 38359  on 27979  degrees of freedom
#AIC: 38401

# Drop some varible which are not co-related with CHURNDEP
fit4 <- glm(CHURNDEP~REVENUE+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+CUSTCARE+THREEWAY+PEAKVCE+
              MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+AGE2+CHILDREN+CREDITA+CREDITAA+CREDITB+CREDITC+
              CREDITDE+CREDITGY+PRIZMRUR+PRIZMUB+REFURB+WEBCAP+TRUCK+RV+OCCPROF+OCCCLER+OCCCRFT+OCCSTUD+
              OCCHMKR+OCCRET
            ,data = training,
            family = binomial)
summary(fit4)

#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37711  on 27963  degrees of freedom
#AIC: 37785

# Drop some varible which are not co-related with CHURNDEP
fit5 <- glm(CHURNDEP~REVENUE+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+ACTVSUBS+
              MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+CHILDREN+CREDITDE+PRIZMUB+
              REFURB+WEBCAP+OWNRENT+MARRYUN+MARRYYES+MAILORD+MAILRES+TRAVEL+PCOWN+CREDITCD
            ,data = training,
            family = binomial)
summary(fit5)

#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37718  on 27973  degrees of freedom
#AIC: 37772
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit6 <- glm(CHURNDEP~REVENUE+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+ACTVSUBS+PHONES+
              EQPDAYS+AGE1+CHILDREN+CREDITDE+REFURB+WEBCAP+RETCALLS+RETACCPT+NEWCELLY+
              REFER+INCMISS+INCOME+MCYCLE
            ,data = training,
            family = binomial)
summary(fit6)

#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37611  on 27971  degrees of freedom
#AIC: 37617
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit7 <- glm(CHURNDEP~MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+
              ACTVSUBS+PHONES+EQPDAYS+AGE1+CHILDREN+CREDITDE+REFURB+
              WEBCAP+RETCALLS+RETACCPT+NEWCELLY+INCOME+CREDITAD+SETPRCM+SETPRC
            ,data = training,
            family = binomial)
summary(fit7)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37551  on 27976  degrees of freedom
#AIC: 37599
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit8 <- glm(CHURNDEP~MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+ACTVSUBS+
              PHONES+EQPDAYS+AGE1+CHILDREN+CREDITDE+REFURB+WEBCAP+RETCALLS+RETACCPT+NEWCELLY+
              CREDITAD+SETPRCM
            ,data = training,
            family = binomial)
summary(fit8)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37558  on 27978  degrees of freedom
#AIC: 37602
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit9 <- glm(CHURNDEP~MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+ACTVSUBS+
              PHONES+EQPDAYS+AGE1+CREDITDE+REFURB+WEBCAP+RETCALLS+RETACCPT+
              NEWCELLY+CREDITAD+SETPRC
            ,data = training,
            family = binomial)
summary(fit9)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37568  on 27979  degrees of freedom
#AIC: 37610
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit10 <- glm(CHURNDEP~MOU+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+ACTVSUBS+
               PHONES+EQPDAYS+AGE1+CREDITDE+REFURB+WEBCAP+RETCALLS+
               CREDITAD+SETPRC
             ,data = training,
             family = binomial)
summary(fit10)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37585  on 27982  degrees of freedom
#AIC: 37621
#Number of Fisher Scoring iterations: 4

pt1 <- predict(fit10, newdata=testing, type="response") 
str(pt1)

#library(InformationValue)

#optCutOff <- optimalCutoff(, predicted)[1] 
#=> 0.71
vif(fit12)

#confusion matrix
(table(Proactive$CHURNDEP, predict > 0.5))

#[1] "REVENUE"  "MOU"      "RECCHRGE" "DIRECTAS" "OVERAGE"  "ROAM"     "CHANGEM"  "CHANGER"  "DROPVCE" 
#[10] "BLCKVCE"  "UNANSVCE" "CUSTCARE" "THREEWAY" "MOUREC"   "OUTCALLS" "INCALLS"  "PEAKVCE"  "OPEAKVCE"
#[19] "DROPBLK"  "CALLFWDV" "CALLWAIT" "CHURN"    "MONTHS"   "UNIQSUBS" "ACTVSUBS" "PHONES"   "MODELS"  
#[28] "EQPDAYS"  "AGE1"     "AGE2"     "CHILDREN" "CREDITA"  "CREDITAA" "CREDITB"  "CREDITC"  "CREDITDE"
#[37] "CREDITGY" "PRIZMRUR" "PRIZMUB"  "REFURB"   "WEBCAP"   "TRUCK"    "RV"       "OCCPROF"  "OCCCLER" 
#[46] "OCCCRFT"  "OCCSTUD"  "OCCHMKR"  "OCCRET"   "OWNRENT"  "MARRYUN"  "MARRYYES" "MAILORD"  "MAILRES" 
#[55] "TRAVEL"   "PCOWN"    "CREDITCD" "RETCALLS" "RETACCPT" "NEWCELLY" "REFER"    "INCMISS"  "INCOME"  
#[64] "MCYCLE"   "CREDITAD" "SETPRCM"  "SETPRC"   "CHURNDEP"
# Drop some varible which are not co-related with CHURNDEP
fit11 <- glm(CHURNDEP~MOU+OVERAGE+ROAM+CHANGEM+CHANGER+MONTHS+UNIQSUBS+ACTVSUBS+
               PHONES+EQPDAYS+AGE1+CREDITDE+REFURB+WEBCAP+RETCALLS
             ,data = training,
             family = binomial)
summary(fit11)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37618  on 27978  degrees of freedom
#AIC: 37662

# Drop some varible which are not co-related with CHURNDEP
fit12 <- glm(CHURNDEP~MOU+OVERAGE+ROAM+CHANGEM+CHANGER+PEAKVCE+MONTHS+UNIQSUBS+ACTVSUBS+
               MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+CHILDREN+CREDITB+CREDITDE+
               CREDITDE+REFURB+WEBCAP+RETCALLS+SETPRC
             ,data = training,
             family = binomial)
summary(fit12)


ap <- available.packages()
chooseCRANmirror()
library(installr)
updateR()
source("http://bioconductor.org/biocLite.R")
biocLite("BiocUpgrade")











