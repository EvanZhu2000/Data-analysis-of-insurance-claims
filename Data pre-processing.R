# This section is responsible for data loading and data pre-processing, as well as loading from different kinds of libruary.
install.packages("readxl")
install.packages("corrplot")
install.packages("caTools")
install.packages("e1071")
install.packages("MASS")
install.packages("leaps")
install.packages("glmnet")
install.packages("class")
install.packages("pls")
install.packages("neuralnet")
install.packages('plot.matrix')
install.packages('devtools')
install.packages('ggplot2')
install.packages('dplyr')

library(dylyr)
library(ggplot2)
library(devtools)
library(readxl)
library(caTools)
library(e1071)
library(MASS)
library(leaps)
library(glmnet)
library(class)
library(pls)
library(corrplot)
library(boot)
library(neuralnet)
library(plot.matrix)




# Initial settings
data <- read_excel("insurance3r2.xlsx")

#Separate the features and the prediction variable
dat<-data[,1:m]
y <- as.vector(as.matrix(data[,m + 1]))
rsq <- function (x, y) cor(x, y) ^ 2
results <- matrix(0,2,7, dimnames = list(c('mean','sd'),c('kNN', 'Logistic','LDA','PLS&PCR','Ridge&Lasso','SVM','NN')))

#===========================Data Cleaning=======================
summary(data)
boxplot(data)
hist(data$charges)


#===========================Data Transformation=======================
#Feature normalization
m <- 8
dat <- scale(data[,1:m])  #Separate the feature variables
y <- as.vector(as.matrix(data[,m + 1]))
data <- data.frame(cbind(dat, y))

#plot the data
par(mfrow=c(3,3))
hist(data$age);hist(data$sex);hist(data$bmi);hist(data$steps);
hist(data$children);hist(data$smoker);hist(data$region);hist(data$charges);hist(data$y)



#===========================Data Reduction=======================
#Subset Selection
regfit.full = regsubsets(y~., data)
summary(regfit.full)
summary(regfit.full)$rsq # The R^2 statistics
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")  # plot the r^2 statistics
coef(regfit.full,6)
    #Cross validation
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])                
  mat=model.matrix(form,newdata)                   
  coefi=coef(object,id=id)                         
  xvars=names(coefi)
  mat[,xvars]%*%coefi
} #Reference from the internet, manually define the predict.regsubsets
k = 10 #10-fold
set.seed(1)
folds = sample(1:k,nrow(data),replace=TRUE)
cv.errors = matrix(NA,k,m, dimnames=list(NULL, paste(1:m)))
for(j in 1:k){
  best.fit = regsubsets(y~.,data=data[folds!=j,],nvmax=m)
  for(i in 1:m){
    pred = predict(best.fit, data[folds==j,],id=i)
    cv.errors[j,i]=mean( (data$y[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(y~.,data=data, nvmax=m)
coef(reg.best,which.min(as.matrix(mean.cv.errors)))
#data$sex = NULL

#-------------------------------------------------------------------------
#The correlation matrix
corrplot::corrplot(cor(dat), method = 'circle')

#Principle Component Analysis
pr.out <- prcomp(dat, scale = TRUE)
#biplot(pr.out, scale=0)
pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type="b")
plot(cumsum(pve), xlab="Principal Component", ylab=" Cumulative Proportion of Variance Explained", ylim=c(0,1), type="b")
m_pca <-8
dat_pca <- dat %*% pr.out$rotation[,1:m_pca] #Regenerate a model
data_pca <- data.frame(cbind(dat_pca, y))






#Set up learning set
m <- 8 
data <- data[sample(nrow(data)),]#shuffle the data
dat <- as.matrix(data[,1:m]) 
y <- as.vector(as.matrix(data[,m + 1]))

data_pca <- data_pca[sample(nrow(data_pca)),]#shuffle the data
dat_pca <- as.matrix(data_pca[,1:m_pca]) 
y_pca <- as.vector(as.matrix(data_pca[,m_pca + 1]))

m_sub <- 7
data_sub <- data
data_sub$sex = NULL
dat_sub <- as.matrix(data_sub[,1:m_sub]) 
 