#IT
data1<- read.csv("C:/Users/joyhu/Dropbox/Summer/New Pro/IT.csv")
library(plm)
library(tseries)
library(xts)

##ADF test- stationary
v <- c(3:27)
a1<- function(x) {
  xts(data1[,x],as.Date(data1[,2]))
}
b1<-function(x){
  adf.test(a1(x))$p.value
}
o1<-function(x){
  adf.test(a1(x))$statistic
}
m1<-lapply(v,o1)
n1<-lapply(v,b1)
df11 <- data.frame(matrix(unlist(m1), byrow=T))
df12 <- data.frame(matrix(unlist(n1), byrow=T))
q1<-data.frame(df11,df12)
colnames(q1)<-c("test","p.value")
q1
names(data1)[1]<-"country"


library(MASS)
library(sandwich)
library(strucchange)
library(vars)
library(lmtest)

#multicollinearity
cor1<-cor(data1[,-1])
length(which(cor1>0.5))/(nrow(cor1)*ncol(cor1))
library(ggplot2)
library(corrplot)
library(RColorBrewer)
p.mat1<-cor.mtest(data1[,-1])$p
?corrplot
corrplot(cor1, method = "color",
         type = "upper", order = "hclust",tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance 
         p.mat=p.mat1, sig.level = 0.01, insig = "blank", number.cex =0.5, pch.col="grey",
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
library(usdm)
vif(data1[,-c(1:3)])
vif(data1[,-c(1:3,5,8,13,20,25)])
data1<-data1[,-c(5,8,13,20,25)]

?corrplot.mixed
cor1.1<-cor(data1[,-1])
corrplot.mixed(cor1.1, lower="number",upper = "color",
         order = "hclust",tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance 
         p.mat=p.mat1, sig.level = 0.01, insig = "blank", number.cex =0.1,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
## Model building
form1<- data1$EXPORT_IT~data1$BERG.+data1$GOVERD.+data1$GOVER_RESEACHER.+data1$HERD.+data1$HIGH_RESEARCHER.+data1$PATENT_ICT+data1$PATENT_TRIADIC+data1$COOP_COINVENTOR.+data1$COOP_OWN.+data1$COOP_ABROAD.+data1$PATENT_.ENV+data1$INCOME_LEVEL+data1$OECD_MEM+data1$GERD_GOV_PERFORM+data1$GERD_HIGH_PERFORM+data1$GERD_OTHER_FINANCE+data1$GERD_GOV_FINANCE+data1$GERD_ABROAD_FINANCE+data1$PATENT_EPO
rankdata1<-pdata.frame(data1,index=c("country","YEAR"))

#mixed effect model
pool.1 <- plm(form1,data=rankdata1,model="pooling")

#fixed effect model
fei.1<-plm(form1,data=rankdata1,effect="individual",model="within")
fet.1<-plm(form1,data=rankdata1,effect="time",model="within")
fe2.1<-plm(form1,data=rankdata1,effect="twoways",model="within")

pooltest(form1,data=rankdata1,effect="individual",model="within")
pooltest(form1,data=rankdata1,effect="time",model="within")

#compare mixed and fixed-F-test
pooltest(pool.1,fei.1)
pooltest(pool.1,fet.1)
pooltest(pool.1,fe2.1)
pooltest(fet.1,fei.1)
pooltest(fei.1,fe2.1)

#random effect model
rei.1<-plm(form1,data=rankdata1,model="random")

#random-LM
pbgtest(form1,data=rankdata1,model="within")

#compare fixed and random-Hausman
phtest(form1,data=rankdata1)

#
summary(rei.1)

#AERO
data2<- read.csv("C:/Users/joyhu/Dropbox/Summer/New Pro/AERO.csv")

##ADF test- stationary
v<- c(3:27)
a2<- function(x) {
  xts(data2[,x],as.Date(data2[,2]))
}
b2<-function(x){
  adf.test(a2(x))$p.value
}
o2<-function(x){
  adf.test(a2(x))$statistic
}
m2<-lapply(v,o2)
n2<-lapply(v,b2)
df21 <- data.frame(matrix(unlist(m2), byrow=T))
df22 <- data.frame(matrix(unlist(n2), byrow=T))
q2<-data.frame(df21,df22)
colnames(q2)<-c("test","p.value")
q2
names(data2)[1]<-"country"

#multicollinearity
cor2<-cor(data2[,-1])
length(which(cor2>0.5))/(nrow(cor2)*ncol(cor2))
p.mat2<-cor.mtest(data2[,-1])$p
corrplot(cor2, method = "color",
         type = "upper", order = "hclust",tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance 
         p.mat=p.mat2, sig.level = 0.01, insig = "blank", number.cex =0.1,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
vif(data2[,-c(1:3)])
vif(data2[,-c(1:3,5,8,13,20,25)])
data2<-data2[,-c(5,8,13,20,25)]

cor2.1<-cor(data2[,-1])
corrplot(cor2.1, method = "color",
         type = "upper", order = "hclust",tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance 
         p.mat=p.mat2, sig.level = 0.01, insig = "blank", number.cex =0.1,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
## Model building
form2<- data2$EXPORT_AERO~data2$BERG.+data2$GOVERD.+data2$GOVER_RESEACHER.+data2$HERD.+data2$HIGH_RESEARCHER.+data2$PATENT_ICT+data2$PATENT_TRIADIC+data2$COOP_COINVENTOR.+data2$COOP_OWN.+data2$COOP_ABROAD.+data2$PATENT_.ENV+data2$INCOME_LEVEL+data2$OECD_MEM+data2$GERD_GOV_PERFORM+data2$GERD_HIGH_PERFORM+data2$GERD_OTHER_FINANCE+data2$GERD_GOV_FINANCE+data2$GERD_ABROAD_FINANCE+data2$PATENT_EPO
rankdata2<-pdata.frame(data2,index=c("country","YEAR"))

#mixed effect model
pool.2 <- plm(form2,data=rankdata2,model="pooling")

#fixed effect model
fei.2<-plm(form2,data=rankdata2,effect="individual",model="within")
fet.2<-plm(form2,data=rankdata2,effect="time",model="within")
fe2.2<-plm(form2,data=rankdata2,effect="twoways",model="within")

pooltest(form2,data=rankdata2,effect="individual",model="within")
pooltest(form2,data=rankdata2,effect="time",model="within")

#compare mixed and fixed-F-test
pooltest(pool.2,fei.2)
pooltest(pool.2,fet.2)
pooltest(pool.2,fe2.2)
pooltest(fet.2,fei.2)
pooltest(fei.2,fe2.2)

#random effect model
rei.2<-plm(form2,data=rankdata2,model="random")

#random-LM
pbgtest(form2,data=rankdata2,model="within")

#compare fixed and random-Hausman
phtest(form2,data=rankdata2)

#
summary(fei.2)
summary(rei.2)

dis<-fixef(fei.2,effect="individual")
dis.2<-as.data.frame(dis)
dis.2


#PHARM
data3<- read.csv("C:/Users/joyhu/Dropbox/Summer/New Pro/PHARM.csv")

##ADF test- stationary
v<- c(3:27)
a3<- function(x) {
  xts(data3[,x],as.Date(data3[,2]))
}
b3<-function(x){
  adf.test(a3(x))$p.value
}
o3<-function(x){
  adf.test(a3(x))$statistic
}
m3<-lapply(v,o3)
n3<-lapply(v,b3)
df31 <- data.frame(matrix(unlist(m3), byrow=T))
df32 <- data.frame(matrix(unlist(n3), byrow=T))
q3<-data.frame(df31,df32)
colnames(q3)<-c("test","p.value")
q3
names(data3)[1]<-"country"

#multicollinearity
cor3<-cor(data3[,-1])
length(which(cor3>0.5))/(nrow(cor3)*ncol(cor3))
p.mat3<-cor.mtest(data3[,-1])$p
corrplot(cor3, method = "color",
         type = "upper", order = "hclust",tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance 
         p.mat=p.mat3, sig.level = 0.01, insig = "blank", number.cex =0.1,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
vif(data3[,-c(1:3)])
vif(data3[,-c(1:3,5,8,11,13,20,25)])
data3<-data3[,-c(5,8,11,13,20,25)]

cor3.1<-cor(data3[,-1])
corrplot(cor3.1, method = "color",
         type = "upper", order = "hclust",tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 60, # Text label color and rotation
         # Combine with significance 
         p.mat=p.mat3, sig.level = 0.01, insig = "blank", number.cex =0.1,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
## Model building
form3<- data3$EXPORT_PHAR~data3$BERG.+data3$GOVERD.+data3$GOVER_RESEACHER.+data3$HERD.+data3$HIGH_RESEARCHER.+data3$PATENT_TRIADIC+data3$COOP_COINVENTOR.+data3$COOP_OWN.+data3$COOP_ABROAD.+data3$PATENT_PHARMA+data3$INCOME_LEVEL+data3$OECD_MEM+data3$GERD_GOV_PERFORM+data3$GERD_HIGH_PERFORM+data3$GERD_OTHER_FINANCE+data3$GERD_GOV_FINANCE+data3$GERD_ABROAD_FINANCE+data3$PATENT_EPO
rankdata3<-pdata.frame(data3,index=c("country","YEAR"))

#mixed effect model
pool.3 <- plm(form3,data=rankdata3,model="pooling")

#fixed effect model
fei.3<-plm(form3,data=rankdata3,effect="individual",model="within")
fet.3<-plm(form3,data=rankdata3,effect="time",model="within")
fe2.3<-plm(form3,data=rankdata3,effect="twoways",model="within")

pooltest(form3,data=rankdata3,effect="individual",model="within")
pooltest(form3,data=rankdata3,effect="time",model="within")

#compare mixed and fixed-F-test
pooltest(pool.3,fei.3)
pooltest(pool.3,fet.3)
pooltest(pool.3,fe2.3)
pooltest(fet.3,fei.3)
pooltest(fei.3,fe2.3)

#random effect model
rei.3<-plm(form3,data=rankdata3,model="random")

#random-LM
pbgtest(form3,data=rankdata3,model="within")

#compare fixed and random-Hausman
phtest(form3,data=rankdata3)

#
summary(fei.3)
summary(rei.3)
library(lmtest)
dwtest(fei.3)

dis<-fixef(fei.3,effect="individual")
dis<-as.data.frame(dis)
dis
