
set.seed(123)
data <- data[sample(nrow(data)),]
test <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.2) 
data_holdout <- data[!test,]
data_holdout <- data_holdout[sample(nrow(data_holdout)),]
y <- as.vector(as.matrix(data_holdout[,m + 1]))
train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_holdout)), SplitRatio = 0.75) # split the training set be 0.75

h = 10 # number of hidden layer
cv.nn.accuracy = matrix(NA,k,h, dimnames=list(NULL, paste(1:h)))
cv.nn = matrix(NA,h,2, dimnames=list(paste(1:h),c('training','test')))
for (i in 1:h){
  nn.fit <- neuralnet(y~ ., data=data_holdout[train,], hidden = i, linear.output = FALSE, err.fct = 'sse')
  nn.train <- ifelse(predict(nn.fit, newdata = data_holdout[train,], type = "response")>0.5, 1, 0)
  cv.nn[i,1] <- mean(nn.train == data_holdout$y[train]) # Training set error
  nn.test <- ifelse(predict(nn.fit, newdata = data_holdout[!train,], type = "response")>0.5, 1, 0)
  cv.nn[i,2] <- mean(nn.test == data_holdout$y[!train]) # Development set error
}
par(mfrow = c(1,1))
plot(cv.nn[,1], type='b',col = 2,main = 'Same training data set',sub = 'Training: red; Development: green; black for regression line',ylim=range( c(cv.nn[,1], cv.nn[,2])))
lines(cv.nn[,2], type='b',col = 3)
cv <- as.data.frame(cv.nn)
lm.fit <- lm(cv$test~as.numeric(1:h))
abline(lm.fit)
plot(cv.nn[,1] - cv.nn[,2],type = 'b',main = 'Training - Development', sub = 'Regression line: black, 5% line: blue',col = 2)
lm.fit <- lm(cv.nn[,1] - cv.nn[,2]~as.numeric(1:h))
abline(lm.fit)
abline(0.05,0,col = 'blue')




best.i = 3 # Choose the best i
nn.fit <- neuralnet(y~ ., data=data_holdout[train,], hidden = best.i, linear.output = FALSE, err.fct = 'sse')
nn.pred <- ifelse(predict(nn.fit, newdata = data[test,], type = "response")>0.5, 1, 0)
mean(nn.pred == data$y[test]) # Accuracy
