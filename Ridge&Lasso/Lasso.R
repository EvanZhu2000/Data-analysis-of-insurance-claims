
data <- data[sample(nrow(data)),]#shuffle the data
dat <- as.matrix(data[,1:m]) 
y <- as.vector(as.matrix(data[,m + 1]))

train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75) # split the training set

grid=10^seq(10,-2,length=100) #lambda sequence
lasso.mod=glmnet(dat,y,alpha=1,lambda=grid)
dim(coef(lasso.mod)) #coefficients matrix

#Cross Validation
set.seed(1)
cv.out=cv.glmnet(dat[train,],y[train],nfolds = 30, alpha=1)
# par(mfrow=c(1,1))
# plot(cv.out)
bestlam=cv.out$lambda.min #best lambda in terms of minimizing mean cross-validated error
# bestlam

lasso.pred=predict(lasso.mod,s = bestlam,newx = dat[train,])
mean((lasso.pred-y[train])^2) # training RSS
lasso.pred=predict(lasso.mod,s = bestlam,newx = dat[!train,])
mean((lasso.pred-y[!train])^2) # test RSS
out = glmnet(dat,y,alpha=1)
lasso.coef <- predict(out,type="coefficients",s = bestlam) #corresponding coefficient
lasso.coef

