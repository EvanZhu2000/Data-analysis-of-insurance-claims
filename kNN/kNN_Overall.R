
#Cross-validation for the mean RSS and accuracy and standard deviaion of RSS and accuracy
k = 30
cv <- matrix(0,nrow = k, ncol = 2)
colnames(cv) <- c('RSS','Accuracy')
for (loop in 1:k){
  m_sub <-7
  data_sub <- data_sub[sample(nrow(data_sub)),]#shuffle the data
  dat_sub <- as.matrix(data_sub[,1:m_sub]) 
  y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_sub)), SplitRatio = 0.75) # split the training set
  
  #Prediction
  best.k=3
  knn.fit <- knn(dat_sub[train,],dat_sub[!train,],data_sub$y[train],k=best.k)
  cv[loop,1] <- mean((as.numeric(knn.fit)-1 - data_sub$y[!train])^2) #RSS
  cv[loop,2] <- mean(knn.fit == data_sub$y[!train]) #Accuracy
}
mean.cv = apply(cv, 2, mean)
sd.cv = apply(cv, 2, sd)
cv.knn <- matrix(0,2,2,dimnames = list(c('mean','sd'),c('RSS','Accuracy')))
cv.knn[1,] <- mean.cv
cv.knn[2,] <- sd.cv
cv.knn
results[,1] <- cv.knn[,2]

