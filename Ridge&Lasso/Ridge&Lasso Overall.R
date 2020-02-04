k=10
cv.rl.comparison <- matrix(0,k,8, dimnames = list(paste(1:k),c('R RSS','R accuracy','L RSS', 'L accuracy', 'R_pca RSS','R_pca accuracy','L_pca RSS', 'L_pca accuracy')))
for (i in 1:k){
  data <- data[sample(nrow(data)),]#shuffle the data
  dat <- as.matrix(data[,1:m]) 
  y <- as.vector(as.matrix(data[,m + 1]))
  data_pca <- data_pca[sample(nrow(data_pca)),]#shuffle the data
  dat_pca <- as.matrix(data_pca[,1:m_pca]) 
  y_pca <- as.vector(as.matrix(data_pca[,m_pca + 1]))
  
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75) # split the training set
  grid=10^seq(10,-2,length=100) #lambda sequence
  
  #Ridge
  ridge.mod=glmnet(dat,y,alpha=0,lambda=grid) 
  set.seed(1)
  cv.out=cv.glmnet(dat[train,],y[train],nfolds = 10, alpha=0)
  bestlam=cv.out$lambda.min 
  ridge.pred=predict(ridge.mod,s = bestlam,newx = dat[!train,])
  cv.rl.comparison[i,1] <- mean((ridge.pred-y[!train])^2) # test RSS
  cv.rl.comparison[i,2] <- mean(round(ridge.pred) == y[!train]) # corresponding accuracy
  
  #Lasso
  lasso.mod=glmnet(dat,y,alpha=1,lambda=grid)
  set.seed(1)
  cv.out=cv.glmnet(dat[train,],y[train],nfolds = 10, alpha=1)
  bestlam=cv.out$lambda.min 
  lasso.pred=predict(lasso.mod,s = bestlam,newx = dat[!train,])
  cv.rl.comparison[i,3] <- mean((lasso.pred-y[!train])^2) # test RSS
  cv.rl.comparison[i,4] <- mean(round(lasso.pred) == y[!train]) #accuracy
  
  #Ridge pca
  ridge.mod=glmnet(dat_pca,y_pca,alpha=0,lambda=grid) 
  set.seed(1)
  cv.out=cv.glmnet(dat_pca[train,],y_pca[train],nfolds = 10, alpha=0)
  bestlam=cv.out$lambda.min 
  ridge.pred=predict(ridge.mod,s = bestlam,newx = dat_pca[!train,])
  cv.rl.comparison[i,5] <- mean((ridge.pred-y_pca[!train])^2) #corresponding test RSS
  cv.rl.comparison[i,6] <- mean(round(ridge.pred) == y_pca[!train]) # corresponding accuracy
  
  #Lasso pca
  lasso.mod=glmnet(dat_pca,y_pca,alpha=1,lambda=grid)
  set.seed(1)
  cv.out=cv.glmnet(dat_pca[train,],y_pca[train],nfolds = 10, alpha=1)
  bestlam=cv.out$lambda.min 
  lasso.pred=predict(lasso.mod,s = bestlam,newx = dat_pca[!train,])
  cv.rl.comparison[i,7] <- mean((lasso.pred-y_pca[!train])^2) # test RSS
  cv.rl.comparison[i,8] <- mean(round(lasso.pred) == y_pca[!train]) #accuracy
  
}
cv.rl <- matrix(0,2,8,dimnames = list(c('mean','sd'),c('R RSS','R accuracy','L RSS', 'L accuracy', 'R_pca RSS','R_pca accuracy','L_pca RSS', 'L_pca accuracy')))
cv.rl[1,] <- apply(cv.rl.comparison,2,mean)
cv.rl[2,] <- apply(cv.rl.comparison,2,sd)

cv.rl
results[,5] <- cv.rl[,which.max(cv.rl[1,])]





