
#Cross Validation
k = 30
cv.rss <- matrix(0,k,4,dimnames = list(1:k,c('pls rss','pls accuracy','pcr rss','pcr accuracy')))
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
  #Prediction
  #PLS
  pls.fit.sub=plsr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
  pls.pred.sub = predict(pls.fit.sub, data_sub[!train,], ncomp = 7) 
  cv.rss[i,1] <- mean((round(pls.pred.sub)-data_sub$y[!train])^2) #RSS 
  cv.rss[i,2] <- mean(round(pls.pred.sub) == data_sub$y[!train]) 
  
  #PCR
  pcr.fit=pcr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
  pcr.test = predict(pcr.fit, data_sub[!train,], ncomp = 7) 
  cv.rss[i,3] <- mean((round(pcr.test)-data_sub$y[!train])^2) #RSS 
  cv.rss[i,4] <- mean(round(pcr.test) == data_sub$y[!train]) 
  
}
cv <- matrix(0,2,4,dimnames = list(c('mean','sd'),c('pls rss','pls accuracy','pcr rss','pcr accuracy')))
cv[1,] <- apply(cv.rss, 2, mean)
cv[2,] <- apply(cv.rss, 2, sd)
cv
results[,4] <- cv[,2]
