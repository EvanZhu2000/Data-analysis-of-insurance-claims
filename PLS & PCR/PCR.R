
#Cross Validation
k = 30
cv.pcr.rss <- matrix(0,k,4,dimnames = list(1:k,c('data_8_train','data_8_test','datasub_7_train','datasub_7_test')))
for (i in 1:k){
  #Set up learning set
  data <- data[sample(nrow(data)),]
  dat <- as.matrix(data[,1:m_sub]) 
  y <- as.vector(as.matrix(data[,m_sub + 1]))
  
  data_sub <- data_sub[sample(nrow(data_sub)),]
  dat <- as.matrix(data_sub[,1:m_sub]) 
  y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
  
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_sub)), SplitRatio = 0.75) # split the training set be 0.75
  
  set.seed(1)
  #data
  pcr.fit=pcr(y~., data=data,subset=train,scale=TRUE, validation="CV")
  pcr.train = predict(pcr.fit, data[train,], ncomp = 8) 
  cv.pcr.rss[i,1] <- mean((round(pcr.train)-data$y[train])^2) #RSS 
  pcr.test = predict(pcr.fit, data[!train,], ncomp = 8) 
  cv.pcr.rss[i,2] <- mean((round(pcr.test)-data$y[!train])^2) #RSS 
  
  #data_sub
  pcr.fit=pcr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
  pcr.train = predict(pcr.fit, data_sub[train,], ncomp = 7) 
  cv.pcr.rss[i,3] <- mean((round(pcr.train)-data_sub$y[train])^2) #RSS 
  pcr.test = predict(pcr.fit, data_sub[!train,], ncomp = 7) 
  cv.pcr.rss[i,4] <- mean((round(pcr.test)-data_sub$y[!train])^2) #RSS 
}
cv.rss <- matrix(0,2,4,dimnames = list(c('mean','sd'),c('data_8_train','data_8_test','datasub_7_train','datasub_7_test')))
cv.rss[1,] <- apply(cv.pcr.rss, 2, mean)
cv.rss[2,] <- apply(cv.pcr.rss, 2, sd)
cv.rss

