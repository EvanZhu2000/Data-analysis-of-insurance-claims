
#Cross Validation
k = 30
cv.pls.rss <- matrix(0,k,8,dimnames = list(1:k,c('data_8_train','data_8_test','data_7_train','data_7_test','datasub_7_train','datasub_7_test','datasub_6_train','datasub_6_test')))
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
  pls.fit=plsr(y~., data=data,subset=train,scale=TRUE, validation="CV")
  # 8 components
  pls.pred = predict(pls.fit, data[train,], ncomp = 8) 
  cv.pls.rss[i,1] <- mean((round(pls.pred)-data$y[train])^2) #RSS 
  pls.pred = predict(pls.fit, data[!train,], ncomp = 8) 
  cv.pls.rss[i,2] <- mean((round(pls.pred)-data$y[!train])^2) #RSS 
  
  # 7 components
  pls.pred = predict(pls.fit, data[train,], ncomp = 7) 
  cv.pls.rss[i,3] <- mean((round(pls.pred)-data$y[train])^2) #RSS 
  pls.pred = predict(pls.fit, data[!train,], ncomp = 7) 
  cv.pls.rss[i,4] <- mean((round(pls.pred)-data$y[!train])^2) #RSS 
  
  #data_sub
  #7 component
  pls.fit.sub=plsr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
  pls.pred.sub = predict(pls.fit.sub, data_sub[train,], ncomp = 7) 
  cv.pls.rss[i,5] <- mean((round(pls.pred.sub)-data_sub$y[train])^2) #RSS 
  pls.pred.sub = predict(pls.fit.sub, data_sub[!train,], ncomp = 7) 
  cv.pls.rss[i,6] <- mean((round(pls.pred.sub)-data_sub$y[!train])^2) #RSS 
  pls.fit.sub=plsr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
  
  #6 component
  pls.pred.sub = predict(pls.fit.sub, data_sub[train,], ncomp = 6) 
  cv.pls.rss[i,7] <- mean((round(pls.pred.sub)-data_sub$y[train])^2) #RSS 
  pls.pred.sub = predict(pls.fit.sub, data_sub[!train,], ncomp = 6) 
  cv.pls.rss[i,8] <- mean((round(pls.pred.sub)-data_sub$y[!train])^2) #RSS 
}

cv.rss <- matrix(0,2,8,dimnames = list(c('mean','sd'),c('data_8_train','data_8_test','data_7_train','data_7_test','datasub_7_train','datasub_7_test','datasub_6_train','datasub_6_test')))
cv.rss[1,] <- apply(cv.pls.rss, 2, mean)
cv.rss[2,] <- apply(cv.pls.rss, 2, sd)
cv.rss[,1:4]
cv.rss[,5:8]



