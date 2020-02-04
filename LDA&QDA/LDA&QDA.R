
#Cross validation 
k = 30
cv.error = matrix(0,k,4)
colnames(cv.error) <- c('LDA train', 'LDA test','QDA train','QDA test')
for (i in 1:k){
  data <- data[sample(nrow(data)),]
  dat <- as.matrix(data[,1:m]) 
  y <- as.vector(as.matrix(data[,m + 1]))
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75) # split the training set
  
  set.seed(123)
  #LDA
  lda.fit <- lda(y ~., data = data, subset = train)
  lda.train <- predict(lda.fit, newdata = data[train,], type = "response")
  lda.test <- predict(lda.fit, newdata = data[!train,], type = "response")
  cv.error[i,1] = mean((as.numeric(lda.train$class)-1 - data$y[train])^2)#Training set RSS
  cv.error[i,2] = mean((as.numeric(lda.test$class)-1 - data$y[!train])^2)#Test set RSS
  
  
  #QDA
  qda.fit <- qda(y ~., data = data, subset = train)
  qda.train <- predict(qda.fit, newdata = data[train,], type = "response")
  qda.test <- predict(qda.fit, newdata = data[!train,], type = "response")
  cv.error[i,3] = mean((as.numeric(qda.train$class)-1 - data$y[train])^2)#Training set RSS
  cv.error[i,4] = mean((as.numeric(qda.test$class)-1 - data$y[!train])^2)#Test set RSS
}
mean.cv.error = apply(cv.error, 2, mean)
sd.cv.error = apply(cv.error, 2, sd)
cv.error.original = rbind(mean.cv.error, sd.cv.error)
rownames(cv.error.original) = c('mean','sd')
