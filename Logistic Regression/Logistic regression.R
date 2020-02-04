# k-Fold Cross Validation for examning which dataset is the best
set.seed(17)
k=50
cv.error = matrix(0,k,2)
colnames(cv.error) <- c('training','test')
cv.error.sub = matrix(0,k,2)
colnames(cv.error.sub) <- c('training','test')
cv.error.pca = matrix(0,k,2)
colnames(cv.error.pca) <- c('training','test')
for (i in 1:k){
  data <- data[sample(nrow(data)),]
  dat <- as.matrix(data[,1:m]) 
  y <- as.vector(as.matrix(data[,m + 1]))
  
  data_sub <- data_sub[sample(nrow(data_sub)),]
  dat_sub <- as.matrix(data_sub[,1:m_sub]) 
  y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
  
  data_pca <- data_pca[sample(nrow(data_pca)),]
  dat_pca <- as.matrix(data_pca[,1:m_pca]) 
  y <- as.vector(as.matrix(data_pca[,m_pca + 1]))
  
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75) # split the training set
  #--------------------------------------------------------------------
  glm.fit <- glm(y ~., data = data, family = binomial, subset = train)
  glm.train <- ifelse(predict(glm.fit, newdata = data[train,], type = "response") > 0.5, 1 ,0)
  glm.test <- ifelse(predict(glm.fit, newdata = data[!train,], type = "response") > 0.5, 1 ,0)
  cv.error[i,1] = mean((glm.train-data$y[train])^2) # Training set Error
  cv.error[i,2] = mean((glm.test-data$y[!train])^2) # Test set Error
  
  glm.fit <- glm(y ~., data = data_sub, family = binomial, subset = train)
  glm.train <- ifelse(predict(glm.fit, newdata = data_sub[train,], type = "response") > 0.5, 1 ,0)
  glm.test <- ifelse(predict(glm.fit, newdata = data_sub[!train,], type = "response") > 0.5, 1 ,0)
  cv.error.sub[i,1] = mean((glm.train-data_sub$y[train])^2) # Training set Error
  cv.error.sub[i,2] = mean((glm.test-data_sub$y[!train])^2) # Test set Error
  
  glm.fit <- glm(y ~., data = data_pca, family = binomial, subset = train)
  glm.train <- ifelse(predict(glm.fit, newdata = data_pca[train,], type = "response") > 0.5, 1 ,0)
  glm.test <- ifelse(predict(glm.fit, newdata = data_pca[!train,], type = "response") > 0.5, 1 ,0)
  cv.error.pca[i,1] = mean((glm.train-data_pca$y[train])^2) # Training set Error
  cv.error.pca[i,2] = mean((glm.test-data_pca$y[!train])^2) # Test set Error
  
}
cv <- rbind(apply(cv.error,2,mean), apply(cv.error,2,sd))
rownames(cv) <- c('mean','sd')
cv.sub <- rbind(apply(cv.error.sub,2,mean), apply(cv.error.sub,2,sd))
rownames(cv.sub) <- c('mean','sd')
cv.pca <- rbind(apply(cv.error.pca,2,mean), apply(cv.error.pca,2,sd))
rownames(cv.pca) <- c('mean','sd')
cv
cv.sub
cv.pca

