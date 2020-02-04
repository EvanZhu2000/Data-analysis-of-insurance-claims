
par(mfrow = c(2,2))
# g<-c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100 )
# c<-c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100 )
g<- seq(0.01, 0.1, by = 0.02)
c<- seq(1,100,by = 10)
radial.train <- matrix(0,length(g),length(c),dimnames = list(g,c))
radial.test <- matrix(0,length(g),length(c),dimnames = list(g,c))
for (i in 1:length(g)){
  for (j in 1:length(c)){
    data_pca <- data_pca[sample(nrow(data_pca)),]#shuffle the data
    dat_pca <- as.matrix(data_pca[,1:m_pca]) 
    y <- as.vector(as.matrix(data_pca[,m_pca + 1]))
    
    train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data_pca)), SplitRatio = 0.75)
    
    svmfit <- svm(y~., data=data_pca[train,], kernel = "radial", gamma = g[i], cost = c[j],type = 'eps-regression')
    svm.radial <- predict(svmfit, data_pca[train,])
    radial.train[i,j] <- mean(ifelse(svm.radial > 0.5 , 1, 0) == data_pca$y[train])
    svm.radial <- predict(svmfit, data_pca[!train,])
    radial.test[i,j] <- mean(ifelse(svm.radial > 0.5 , 1, 0) == data_pca$y[!train])
  }
}
plot(radial.train, xlab = 'c', ylab = 'g', breaks = 10)
plot(radial.test, xlab = 'c', ylab = 'g', breaks = 10)
plot(radial.train - radial.test, xlab = 'c', ylab = 'g', col=topo.colors, breaks = 10)

