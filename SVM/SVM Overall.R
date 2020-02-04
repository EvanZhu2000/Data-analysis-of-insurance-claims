k = 5
cv <- matrix(0,k,4,dimnames = list(1:k,c('data RSS','data Accuracy','data_sub RSS','data_sub Accuracy')))
for (i in 1:k){
  data <- data[sample(nrow(data)),]#shuffle the data
  dat <- as.matrix(data[,1:m]) 
  y <- as.vector(as.matrix(data[,m + 1]))
  data_sub <- data_sub[sample(nrow(data_sub)),]#shuffle the data
  dat_sub <- as.matrix(data_sub[,1:m_sub]) 
  y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
  
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75)
  
  # data
  set.seed(1)
  tune.sigmoid <- tune(svm, y~., data = data[train,], kernel = "radial", 
                       ranges = list(cost=c(21,31,41,51,61,71)),
                       gamma=c(0.07,0.09))
  bestmod <- tune.sigmoid$best.model
  svm.radial <- predict(bestmod, data[!train,])
  cv[i,1] <- mean((svm.radial-data$y[!train])^2) # RSS
  cv[i,2] <- mean(ifelse(svm.radial > 0.5 , 1, 0) == data$y[!train]) # Accuracy
  
  
  # data_sub
  set.seed(1)
  tune.sigmoid <- tune(svm, y~., data = data_sub[train,], kernel = "radial", 
                       ranges = list(cost=c(21,31,41,51,61,71)),
                       gamma=c(0.07,0.09))
  bestmod <- tune.sigmoid$best.model
  svm.radial <- predict(bestmod, data_sub[!train,])
  cv[i,3] <- mean((svm.radial-data_sub$y[!train])^2) # RSS
  cv[i,4] <- mean(ifelse(svm.radial > 0.5 , 1, 0) == data_sub$y[!train]) # Accuracy
  
}
cv.svm <- matrix(0,2,4,dimnames = list(c('mean','sd'),c('data RSS','data Accuracy','data_sub RSS','data_sub Accuracy')))
cv.svm[1,] <- apply(cv, 2, mean)
cv.svm[2,] <- apply(cv, 2, sd)
cv.svm
results[,6] <- cv.svm[,4]


