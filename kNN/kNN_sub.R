m_sub <-7
data_sub <- data_sub[sample(nrow(data_sub)),]#shuffle the data
dat_sub <- as.matrix(data_sub[,1:m_sub]) 
y <- as.vector(as.matrix(data_sub[,m_sub + 1]))
train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_sub)), SplitRatio = 0.75) # split the training set


#Cross Validation for k
k = 30 #k-fold
n = 20
set.seed(1)
folds = sample(1:k,nrow(data_sub),replace=TRUE)
cv.train.accuracy = matrix(NA,k,n, dimnames=list(NULL, paste(1:n)))
cv.test.accuracy = matrix(NA,k,n, dimnames=list(NULL, paste(1:n)))
for (j in 1:k){
  for (i in 1:n){
    knn.fit <- knn(dat_sub[folds==j,],dat_sub[folds==j,],data_sub$y[folds==j], k=i)
    cv.train.accuracy[j,i] <- mean(knn.fit == data_sub$y[folds==j])
    knn.fit <- knn(dat_sub[folds!=j,],dat_sub[folds==j,],data_sub$y[folds!=j], k=i)
    cv.test.accuracy[j,i] <- mean(knn.fit == data_sub$y[folds==j])
  }
}
mean.cv.train.accuracy = apply(cv.train.accuracy,2,mean)
mean.cv.test.accuracy = apply(cv.test.accuracy,2,mean)
plot(mean.cv.train.accuracy, type = 'b', col = 3, main='data_sub', sub= 'Green for training; Red for test')
lines(mean.cv.test.accuracy, type = 'b', col = 2)
#best.k = 3

