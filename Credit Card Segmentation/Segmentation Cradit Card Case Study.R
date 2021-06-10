#--------------CASE STUDY – CREDIT CARD-------------------------------------------
#-----------------------------SEGMENTATION----------------------------------####

rm(list=ls())

# Requied packages 
require("psych")
require("Hmisc")
require(dplyr)
require(reshape2)
require(ggplote2)
require(caret)
require(glm2)
require("influence.ME")
require("stats")
require("lme4")
require("Matrix")
require(GPArotation)
require(cluster)
require(tables)
# set working directory 
setwd("/home/dipesh/BA /Final Projects/Data /Final Case Study 1 - Credit Card Segmentation")


file_a <- read.csv(file.choose(),header = TRUE)
file <- file_a[,-1]
View(file_a)
dim(file)
colnames(file)
str(file)

#Missing Values Deletion
file$MINIMUM_PAYMENTS[is.na(file$MINIMUM_PAYMENTS)] <- 0
file$CREDIT_LIMIT[is.na(file$CREDIT_LIMIT)] <- 0
dim(file)
str(file_a)
summary(file$CREDIT_LIMIT)
#View(file)

#Creating New Variables & Advanced Data Preparation
file$Monthly_Avg_expns <- file$PURCHASES/(file$PURCHASES_FREQUENCY*file$TENURE)
file$Monthly_CASH_ADVANCE <- file$CASH_ADVANCE/(file$CASH_ADVANCE_FREQUENCY*file$TENURE)
file$Monthly_INSTALLMENTS_PURCHASES  <- file$INSTALLMENTS_PURCHASES/(file$PURCHASES_INSTALLMENTS_FREQUENCY*file$TENURE)
file$Monthly_ONEOFF_PURCHASES  <- file$ONEOFF_PURCHASES/(file$ONEOFF_PURCHASES_FREQUENCY*file$TENURE)

file$limit_usage <- file$BALANCE/file$CREDIT_LIMIT
file$limit_usage[file$limit_usage==Inf] <- 0
file$ptmp <- file$PAYMENTS/file$MINIMUM_PAYMENTS
file$ptmp[file$ptmp==Inf] <- 0
file$no_purchase <- file$PURCHASES/file$PURCHASES_TRX
file$Monthly_Avg_expns[is.nan(file$Monthly_Avg_expns)] <- 0
file$Monthly_CASH_ADVANCE[is.nan(file$Monthly_CASH_ADVANCE)] <- 0
file$Monthly_INSTALLMENTS_PURCHASES[is.nan(file$Monthly_INSTALLMENTS_PURCHASES)] <- 0
file$Monthly_ONEOFF_PURCHASES[is.nan(file$Monthly_ONEOFF_PURCHASES)] <- 0
file$ptmp[is.nan(file$ptmp)] <- 0
file$no_purchase[is.nan(file$no_purchase)] <- 0
file$no_purchase[file$no_purchase==Inf] <- 0
a <- as.factor(file$no_purchase)
a
# Corelation matrix
View(file)
corrm <- cor(file)
View(corrm)
write.csv(corrm,"corrm.csv")

#Data Preparation Outlier Treatment 
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  med <- median(a)
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, median=med,mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

class(file)
diag_stats<-t(data.frame(apply(file, 2, mystats)))
write.csv(diag_stats, "diag_stats.csv")
names(file)

diag_stats <- as.data.frame(diag_stats)
file$BALANCE[file$BALANCE>7809.07046604776] <- 7809.07046604776
file$PURCHASES[file$PURCHASES>7413.10917913822] <- 7413.10917913822
file$INSTALLMENTS_PURCHASES[file$INSTALLMENTS_PURCHASES>3124.08199021888] <- 3124.08199021888
file$CREDIT_LIMIT[file$CREDIT_LIMIT>15410.7149052053] <- 15410.7149052053
file$CASH_ADVANCE[file$CASH_ADVANCE>9588.1633568] <- 9588.1633568
file$PAYMENTS[file$PAYMENTS>10418.3351227384] <- 10418.3351227384
file$MINIMUM_PAYMENTS[file$MINIMUM_PAYMENTS>7841.95320738377] <- 7841.95320738377
#file$Monthly_Avg_expns[file$Monthly_Avg_expns>986.029883028158] <- 986.029883028158
#file$Monthly_CASH_ADVANCE[file$Monthly_CASH_ADVANCE>3200.48675416176] <- 3200.48675416176
#file$Monthly_INSTALLMENTS_PURCHASES[file$Monthly_INSTALLMENTS_PURCHASES>439.463506363285] <- 439.463506363285
#file$Monthly_ONEOFF_PURCHASES[file$Monthly_ONEOFF_PURCHASES>1039.55862704017] <- 1039.55862704017
#file$ptmp[file$ptmp>363.554382806177] <- 363.554382806177
#file$no_purchase[file$no_purchase>555.44550033815] <- 555.44550033815


#Factor Analysis

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
data.frame(eigen(corrm)$values )  

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
summary(file)
FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(file),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME
write.csv(Loadings, "loadings1.csv") ### SAVING THE FILE

vars <- c("ONEOFF_PURCHASES","PAYMENTS","PURCHASES_TRX","CASH_ADVANCE","Monthly_CASH_ADVANCE",
          "CASH_ADVANCE_FREQUENCY","no_purchase","Monthly_Avg_expns","PURCHASES_INSTALLMENTS_FREQUENCY","PURCHASES_FREQUENCY",
            "Monthly_INSTALLMENTS_PURCHASES","limit_usage","BALANCE","BALANCE_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY")
inputdata_final <-file[vars]

#Prepare final Data
#standardizing the data
inputdata_final = scale(inputdata_final)
View(inputdata_final)
#building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

file_new<-cbind(file,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(file_new)

#Graph based on k-means - Optional

clusplot(inputdata_final, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

tt<-cbind(tt<-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                            ~ Heading()*length*All(file[1]), data=file_new)),
          tabular( 1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
                   ~ Heading()*mean*All(file[vars]), data=file_new))
tt1<-as.data.frame.matrix(tt)
View(tt1)

rownames(tt1)<-c("Overall", "KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
colnames(tt1)<-c("Count","ONEOFF_PURCHASES","PURCHASES","PAYMENTS","no_purchase",
                 "Monthly_Avg_expns","PURCHASES_INSTALLMENTS_FREQUENCY","PURCHASES_FREQUENCY","PURCHASES_TRX",
                 "INSTALLMENTS_PURCHASES","BALANCE","CASH_ADVANCE_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY")
cluster_profiling<-t(tt1)

write.csv(cluster_profiling, "cluster_profiling_withoutoutliers.csv") 

#------------------------------------------------------
#



