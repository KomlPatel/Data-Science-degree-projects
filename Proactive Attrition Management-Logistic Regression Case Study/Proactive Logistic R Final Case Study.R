#--------- Proactive Attrition Management-----------------------------------------------

#-----------------------------Logistic Regression Case Study--------------------------#
# Required Library
#install.packages("MASS")
library("MASS") 
#install.packages("InformationValue")
require(InformationValue)
require(dplyr)
# Working Directory 
setwd("/home/dipesh/BA /Final Projects/Data /Proactive Attrition Management-Logistic Regression Case Study")

# Import Data 
Proactive <- read.csv("/home/dipesh/BA /Final Projects/Data /Proactive Attrition Management-Logistic Regression Case Study/Proactive Attrition Management-Logistic Regression Case Study.csv")

#View(Proactive)
names(Proactive)
ncol(Proactive)
nrow(Proactive)
head(Proactive)
str(Proactive)
summary(Proactive)
# convert numeric to character 
#for(i in c(23:26,30,34:64,67,68,72,74,76:78)) {
#  Proactive[, i] <- as.character(Proactive[, i])
#}
# Division of data for Testing and Pridicting-----------------------------
# Hear we have two data set for buiding a model (Calibration Data Set) and using this model we have to pridict 
# test data set (Validation Data Set).

##------------ Validation Data Set----------------------------------------

Validation <- Proactive[is.na(Proactive$CHURNDEP),]

#View(Validation)
#nrow(Validation)

##----------- Calibration Data Set------------------------------------
 
calibration  <- Proactive[!is.na(Proactive$CHURNDEP),]

nrow(calibration)
str(calibration)
summary(calibration)
names(calibration)
#View(calibration)


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
  pct_nmiss <- mean(is.na(x))
  return(c(nmiss=nmiss,Pct_Nmiss=pct_nmiss))
}

#Implementing the function on one variable at a time
#Separating all the numeric & categorical variables to treat them
numeric_vars = names(calibration)[sapply(calibration, FUN=is.numeric)]
cat_vars = names(calibration)[sapply(calibration, FUN=is.character)]
#View(cat_vars)

#Data Audit Report for numerical variables
summary_stats = t(apply(calibration[numeric_vars], 2, FUN=mystats_num))
#View(summary_stats)
write.csv(summary_stats, file = "Log_stats_numeric_vars.csv")

#Data Audit Report for categorical variables
summary_stats_cat = t(apply(calibration[cat_vars], 2, FUN=mystats_cat))
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
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  #UC1 = mean(x,na.rm=T) + 3*sd(x, na.rm=T)
  #LC1 = mean(x,na.rm=T) - 3*sd(x, na.rm=T)
  
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  #x[x>UC1]=UC1
  #x[x<LC1]=LC1
  return(x)
  
}

mydata_num = data.frame(apply(calibration[numeric_vars], 2, FUN=outlier_treat))
#View(mydata_num)

#Number of missings in each variable
sapply(calibration, FUN=function(x) sum(is.na(x)))

#---------- Missing value treatment for numerical variables----------------------------------
# Missing value treatment for numerical variables
miss_treat_num = function(x){
  x[is.na(x)] = median(x,na.rm=T) # replace missings with mean
  return(x)
}

mydata_num = data.frame(apply(mydata_num, 2, FUN=miss_treat_num))

# Missing value treatment for categorical variables

# Missing value treatment for categorical variables
miss_treat_cat = function(x){
  x[is.na(x)]<-x[which.max(prop.table(table(x)))] #replacing missings with mode
  return(x)
}

mydata_cat = data.frame(apply(calibration[cat_vars], 2, FUN=miss_treat_cat))

#levels(mydata_cat)
#unique(mydata_cat)

summary(mydata_num)
   str(mydata_cat)
names(mydata_num)
mydata_num <- mydata_num[-c(20,29,39,58,47,48,75,76)]

#mydata_num[is.na(mydata_num),]
cor_mat <- cor(mydata_num)
names(mydata_num)
write.csv(cor_mat,"Log_cor_mat.csv")
#View(cor_mat)
#names(mydata_cat)

#names(mydata_num)
#mydata1 <- cbind(mydata_num,mydata_cat)
mydata1 = mydata_num
#View(mydata1)
ncol(mydata1)
names(mydata1)

#Clean NA From data 

mydata1 <- mydata1[complete.cases(mydata1),]

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

#  drops varibles with NA:

fit2 <- glm(CHURNDEP~REVENUE+MOU+RECCHRGE+OVERAGE+DIRECTAS+ROAM+CHANGEM+CHANGER+DROPVCE+BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+
              MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+DROPBLK+CALLWAIT+MONTHS+UNIQSUBS+ACTVSUBS+
              PHONES++MODELS+EQPDAYS+AGE1+CHILDREN+CREDITA+CREDITAA+CREDITB+CREDITC+CREDITDE+CREDITGY+
              PRIZMRUR+PRIZMUB+REFURB+WEBCAP+RV+OCCCRFT+OCCRET+OCCSELF+OWNRENT+MARRYUN+MARRYYES+MAILFLAG+
              TRAVEL+PCOWN+RETCALLS+RETACCPT+NEWCELLN+REFER+INCMISS+INCOME+MCYCLE+CREDITAD+SETPRC,
            data = training,family = binomial)
summary(fit2)
#    Null deviance: 38815.89911065664  on 27999  degrees of freedom
#Residual deviance: 0.00000016244  on 27941  degrees of freedom
#AIC: 37592
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit3 <- glm(CHURNDEP~MOU+RECCHRGE+OVERAGE+DIRECTAS+ROAM+CHANGEM+CHANGER+THREEWAY+
              INCALLS+PEAKVCE+DROPBLK+MONTHS+UNIQSUBS+ACTVSUBS+
              PHONES+EQPDAYS+AGE1+CHILDREN+CREDITC+CREDITDE+
              PRIZMUB+REFURB+WEBCAP+MARRYUN+
              RETCALLS+RETACCPT+INCMISS+INCOME+CREDITAD+SETPRC,
            data = training,family = binomial)
summary(fit3)
#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37524  on 27969  degrees of freedom
#AIC: 37573
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit4 <- glm(CHURNDEP~MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+THREEWAY+
              INCALLS+PEAKVCE+DROPBLK+MONTHS+UNIQSUBS+ACTVSUBS+
              PHONES+EQPDAYS+AGE1+CHILDREN+CREDITC+CREDITDE+
              PRIZMUB+REFURB+WEBCAP+MARRYUN+
              RETCALLS+RETACCPT+INCMISS+INCOME+CREDITAD+SETPRC,
            data = training,family = binomial)
summary(fit4)

#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37526  on 27970  degrees of freedom
#AIC: 37586
#Number of Fisher Scoring iterations: 4

# Drop some varible which are not co-related with CHURNDEP
fit5 <- glm(CHURNDEP~MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+
              THREEWAY+PEAKVCE+DROPBLK+MONTHS+UNIQSUBS+ACTVSUBS+
              PHONES+EQPDAYS+AGE1+CHILDREN+CREDITC+CREDITDE+
              PRIZMUB+REFURB+WEBCAP+MARRYUN+RETCALLS+
              INCMISS+INCOME+SETPRC,
            data = training,family = binomial)
summary(fit5)

#    Null deviance: 38816  on 27999  degrees of freedom
#Residual deviance: 37529  on 27971  degrees of freedom
#AIC: 37575
#Number of Fisher Scoring iterations: 4

ls("package:InformationValue")

train1<- cbind(training, Prob=predict(fit5, type="response")) 
#View(train1)

Concordance(train1$CHURNDEP, train1$Prob)

somersD(train1$CHURNDEP, train1$Prob)
#0.2440699
table(train1$CHURNDEP)
#  0     1 
# 14073 13927 

AUROC(train1$CHURNDEP, train1$Prob)
#0.6209191
cut1<-optimalCutoff(train1$CHURNDEP, train1$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)
# 0.485
 ROCTable<-data.frame(cut1$sensitivityTable)
# View(ROCTable)

train1$pred_y = ifelse(train1$Prob>0.475,1,0)
#View(train1)

confusionMatrix(train1$CHURNDEP, train1$pred_y)
#     0    1
# 0 7093 4610
# 1 6980 9317

confusionMatrix(train1$CHURNDEP, train1$Prob, threshold=0.495)
#    0    1
# 0 8256 5708
# 1 5817 8219

sum(train1$CHURNDEP)

plotROC(train1$CHURNDEP, train1$Prob, Show.labels=F)

ks_table<-ks_stat(train1$CHURNDEP, train1$Prob, returnKSTable=TRUE)
write.csv(ks_table, "ks_table.csv")

ks_stat(train1$CHURNDEP, train1$Prob, returnKSTable=FALSE)
#0.1774

# Apply pridictive model on testing data set and validate data and check accuracy :

test1<- cbind(testing, Prob=predict(fit5,newdata=testing, type="response")) 
#View(test1)
Concordance(test1$CHURNDEP, test1$Prob)
somersD(test1$CHURNDEP, test1$Prob)

test1$pred_y = ifelse(test1$Prob>0.485,1,0)
#View(test1)

confusionMatrix(test1$CHURNDEP, test1$pred_y)
#View(test1)
ks_table<-ks_stat(test1$CHURNDEP, test1$Prob, returnKSTable=FALSE)

t(confusionMatrix(test1$CHURNDEP, test1$Prob, threshold=0.485))

sum(test1$CHURNDEP)

plotROC(test1$CHURNDEP, test1$Prob, Show.labels=F)


### Apply pridictive model on Validation data set

test2<- cbind(Validation, Prob1=predict(fit5,newdata=Validation, type="response")) 
#View(test2)

test2$pred_y = ifelse(test2$Prob1>0.485,1,0)
#View(test2)

#3. What are the key factors that predict customer churn? Do these factors make sense?

# According to my model these are key factors for churn customer.
#1.MOU,2.OVERAGE,3.CHANGEM,4.CHANGER,6.THREEWAY,7.PEAKVCE,8.DROPBLK,
#9.UNIQSUBS,10.ACTVSUBS,11.PHONES,12.EQPDAYS,13.REFURB


#4. What offers should be made to which customers to encourage them to remain with Cell2Cell?
# Assume that your objective is to generate net positive cash flow, i.e., generate additional
#customer revenues after subtracting out the cost of the incentive.

# These offers encourage customers remain wit cell to cell.
#1.RECCHRGE, 2.ROAM,3.MONTHS, 4.AGE1,5.CHILDREN, 6.CREDITC, 7.CREDITDE, 
#8. PRIZMUB 9. MARRYUN, 10.RETCALLS, 11. RETACCPT, 12.INCMISS, 13.INCOME
#14.CREDITAD 15.SETPRC 

#5. Assuming these actions were implemented, how would you determine whether they had
#worked?

# 1.Take customer feedback which are predict to be churner,
# 2.Provide special discount and offer coupon to selected customers who are
# likely to be churners.





