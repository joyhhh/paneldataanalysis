#import data
library(xlsx)
data<-read.xlsx(
  "C:/Users/joyhu/Dropbox/Summer/New Pro/all_variables_new.xlsx",1)

#NA-value Analysis and distribution
which(is.na(data))
library("pheatmap")
str(data)
data1<-data[,c(3:26,30:38)]
data1[is.na(data)] <- NA
pheatmap(data1,scale="column",cluster_rows = FALSE,
         cluster_cols = FALSE,show_rownames=FALSE,na_col="black")

#Data missing percentage-and delete variables missing over 30%
miss <- function(data1){sum(is.na(data))/length(data)*100}
apply(data1,2,miss)

data2<-data1[,-c(3:6,28)]
pheatmap(data2,scale="column",cluster_rows = FALSE,
         cluster_cols = FALSE,show_rownames=FALSE,na_col="black")

#imput data
library(lattice)
library(MASS)
library(nnet)
library(mice)
init = mice(data2, maxit=0)
predM = init$predictorMatrix

#Method 1 PMM (Predictive mean matching)
imputed1 = mice(data2, method="pmm", predictorMatrix=predM, m=10)
imputed1 <- complete(imputed1)
sapply(imputed1, function(x) sum(is.na(x)))
#export dataset
write.table(imputed1,file="C:/Users/joyhu/Dropbox/Summer/New Pro/111.csv",
            sep = ",",row.names=FALSE)

#Method 2 rf (Random Forest)
install.packages("randomForest")
imputed2 = mice(data2, method="rf", predictorMatrix=predM, m=10)
imputed2 <- complete(imputed2)
sapply(imputed2, function(x) sum(is.na(x)))
#export dataset
write.table(imputed2,file="C:/Users/joyhu/Dropbox/Summer/New Pro/222.csv",
            sep = ",",row.names=FALSE)

#Method 3 cart (Classification and regression trees)
imputed3 = mice(data2, method="cart", predictorMatrix=predM, m=10)
imputed3 <- complete(imputed3)
sapply(imputed3, function(x) sum(is.na(x)))
#export dataset
write.table(imputed3,file="C:/Users/joyhu/Dropbox/Summer/New Pro/333.csv",
            sep = ",",row.names=FALSE)

#Method 4 2l.norm (Level-1 normal heteroskedastic)
imputed4 = mice(data2, method="norm.boot", predictorMatrix=predM, m=10)
imputed4 <- complete(imputed4)
sapply(imputed4, function(x) sum(is.na(x)))
#export dataset
write.table(imputed4,file="C:/Users/joyhu/Dropbox/Summer/New Pro/444.csv",
            sep = ",",row.names=FALSE)

