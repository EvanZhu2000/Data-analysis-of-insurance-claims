
data_pca <- data_pca[sample(nrow(data_pca)),]#shuffle the data
dat_pca <- as.matrix(data_pca[,1:m_pca]) 
y <- as.vector(as.matrix(data_pca[,m_pca + 1]))
train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_pca)), SplitRatio = 0.75) # split the training set


grid=10^seq(10,-2,length=100) #lambda sequence
ridge.fit.pca=glmnet(dat_pca,y,alpha=0,lambda=grid) # alpha=0: ridge; alpha=1: lasso
dim(coef(ridge.fit.pca)) #coefficients matrix

#Cross Validation
set.seed(1)
cv.out=cv.glmnet(dat_pca[train,],y[train],nfolds = 30, alpha=0)
# par(mfrow=c(1,1))
# plot(cv.out)
bestlam=cv.out$lambda.min #best lambda in terms of minimizing mean cross-validated error

ridge.pred.pca=predict(ridge.fit.pca,s = bestlam,newx = dat_pca[train,])
mean((ridge.pred.pca-y[train])^2) #corresponding train RSS
ridge.pred.pca=predict(ridge.fit.pca,s = bestlam,newx = dat_pca[!train,])
mean((ridge.pred.pca-y[!train])^2) #corresponding test RSS

out_pca = glmnet(dat_pca,y,alpha=0)
predict(out_pca,type="coefficients",s = bestlam) #corresponding coefficient
