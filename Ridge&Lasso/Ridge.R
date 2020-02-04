
data <- data[sample(nrow(data)),]#shuffle the data
dat <- as.matrix(data[,1:m]) 
y <- as.vector(as.matrix(data[,m + 1]))
train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75) # split the training set


grid=10^seq(10,-2,length=100) #lambda sequence
ridge.fit=glmnet(dat,y,alpha=0,lambda=grid) # alpha=0: ridge; alpha=1: lasso
dim(coef(ridge.fit)) #coefficients matrix

#Cross Validation
set.seed(1)
cv.out=cv.glmnet(dat[train,],y[train],nfolds = 30, alpha=0)
# par(mfrow=c(1,1))
# plot(cv.out)
bestlam=cv.out$lambda.min #best lambda in terms of minimizing mean cross-validated error

ridge.pred=predict(ridge.fit,s = bestlam,newx = dat[train,])
mean((ridge.pred-y[train])^2) #corresponding train RSS
ridge.pred=predict(ridge.fit,s = bestlam,newx = dat[!train,])
mean((ridge.pred-y[!train])^2) #corresponding test RSS

out = glmnet(dat,y,alpha=0)
predict(out,type="coefficients",s = bestlam) #corresponding coefficient
