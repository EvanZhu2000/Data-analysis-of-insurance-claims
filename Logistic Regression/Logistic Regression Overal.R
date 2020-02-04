
#Cross-validation for the mean RSS and accuracy and standard deviaion of RSS and accuracy
k = 30
cv <- matrix(0,nrow = k, ncol = 2)
colnames(cv) <- c('RSS','Accuracy')
for (loop in 1:k){
  data_sub <- data_sub[sample(nrow(data_sub)),]
  dat_sub <- as.matrix(data_sub[,1:m_sub]) 
  y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
  
  #Prediction
  set.seed(123)
  glm.fit <- glm(y ~., data = data_sub, family = binomial, subset = train)
  glm.pred <- ifelse(predict(glm.fit, newdata = data_sub[!train,], type = "response") > 0.5, 1 ,0)
  cv[loop,1] <-mean((glm.pred-data_sub$y[!train])^2) # Test set Error
  cv[loop,2] <-mean(glm.pred == data_sub$y[!train]) #Accuracy

}
mean.cv = apply(cv, 2, mean)
sd.cv = apply(cv, 2, sd)
cv.logistic <- matrix(0,2,2,dimnames = list(c('mean','sd'),c('RSS','Accuracy')))
cv.logistic[1,] <- mean.cv
cv.logistic[2,] <- sd.cv
cv.logistic
results[,2] <- cv.logistic[,2]

