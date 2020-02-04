cv.error.original
cv.error.sub
cv.error.pca

#Cross-validation for the mean RSS and accuracy and standard deviaion of RSS and accuracy
k = 50
cv <- matrix(0,nrow = k, ncol = 2)
colnames(cv) <- c('RSS','Accuracy')
for (loop in 1:k){
  data_sub <- data_sub[sample(nrow(data_sub)),]#shuffle the data
  dat_sub <- as.matrix(data_sub[,1:m_sub]) 
  y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_sub)), SplitRatio = 0.75) # split the training set
  
  #Prediction
  set.seed(123)
  lda.fit <- lda(y ~., data = data_sub, subset = train)
  lda.train <- predict(lda.fit, newdata = data_sub[train,], type = "response")
  lda.test <- predict(lda.fit, newdata = data_sub[!train,], type = "response")
  cv[loop,1] <- mean((as.numeric(lda.test$class)-1 - data_sub$y[!train])^2)#Test set RSS
  cv[loop,2] <- mean(lda.test$class == data_sub$y[!train]) # Accuracy
}
mean.cv = apply(cv, 2, mean)
sd.cv = apply(cv, 2, sd)
cv.LDA <- matrix(0,2,2,dimnames = list(c('mean','sd'),c('RSS','Accuracy')))
cv.LDA[1,] <- mean.cv
cv.LDA[2,] <- sd.cv
cv.LDA
results[,3] <- cv.LDA[,2]

