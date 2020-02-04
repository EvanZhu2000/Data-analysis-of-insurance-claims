m_pca <-8
data_pca <- data_pca[sample(nrow(data_pca)),]#shuffle the data
dat_pca <- as.matrix(data_pca[,1:m_pca]) 
y <- as.vector(as.matrix(data_pca[,m_pca + 1]))
train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_pca)), SplitRatio = 0.75) # split the training set


#Cross Validation for k
k = 30 #k-fold
n = 20
set.seed(1)
folds = sample(1:k,nrow(data_pca),replace=TRUE)
cv.train.accuracy = matrix(NA,k,n, dimnames=list(NULL, paste(1:n)))
cv.test.accuracy = matrix(NA,k,n, dimnames=list(NULL, paste(1:n)))
for (j in 1:k){
  for (i in 1:n){
    knn.fit <- knn(dat_pca[folds==j,],dat_pca[folds==j,],data_pca$y[folds==j], k=i)
    cv.train.accuracy[j,i] <- mean(knn.fit == data_pca$y[folds==j])
    knn.fit <- knn(dat_pca[folds!=j,],dat_pca[folds==j,],data_pca$y[folds!=j], k=i)
    cv.test.accuracy[j,i] <- mean(knn.fit == data_pca$y[folds==j])
  }
}
mean.cv.train.accuracy = apply(cv.train.accuracy,2,mean)
mean.cv.test.accuracy = apply(cv.test.accuracy,2,mean)
plot(mean.cv.train.accuracy, type = 'b', col = 3, main='data_pca', sub= 'Green for training; Red for test')
lines(mean.cv.test.accuracy, type = 'b', col = 2)
#best.k = 3
