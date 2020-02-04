
best.i = 3 # Choose the best i
k = 8
cv.comparison <- matrix(0,k,1)
for (i in 1:k){
  data <- data[sample(nrow(data)),]
  train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75) # split the training set be 0.75
  
  nn.fit <- neuralnet(y~ ., data=data[train,], hidden = best.i, linear.output = FALSE, err.fct = 'sse')
  nn.test <- ifelse(predict(nn.fit, newdata = data[!train,], type = "response")>0.5, 1, 0)
  cv.comparison[i,1] <- mean(nn.test == data$y[!train]) # Development set error
}
mean(cv.comparison[,1])
sd(cv.comparison[,1])
results[,7]<-cbind(mean(cv.comparison[,1]),sd(cv.comparison[,1]))
