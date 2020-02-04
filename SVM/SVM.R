data <- data[sample(nrow(data)),]#shuffle the data
dat <- as.matrix(data[,1:m]) 
y <- as.vector(as.matrix(data[,m + 1]))

train <- sample.split(matrix(0, nrow=1 , ncol = nrow(data)), SplitRatio = 0.75)


#SVM with radio kernel
# Choose the best cost
set.seed(1)
tune.sigmoid <- tune(svm, y~., data = data[train,], kernel = "radial", 
                 ranges = list(cost=c(0.01,0.1,1,10,100),
                 gamma=c(0.01,0.1,1,10,100)))
bestmod <- tune.sigmoid$best.model
bestmod

#Predictions
svm.radial.train <- predict(bestmod, data[train,])
mean((svm.radial.train-data$y[train])^2) # Training
svm.radial.test <- predict(bestmod, data[!train,])
mean((svm.radial.test-data$y[!train])^2) # Test
plot(bestmod,data)



#=================================================================================
#SVM with linear kernel
# Choose the best cost
set.seed(1)
tune.out <- tune(svm, y~., data = data[train,], kernel = "linear",
                 ranges = list(cost=c(0.01,0.1,1,10,100)))
bestmod <- tune.out$best.model
bestmod

#Predictions
svm.linear.train <- predict(bestmod, data[train,])
mean((svm.linear.train - data$y[train])^2) # Training
svm.linear.test <- predict(bestmod, data[!train,])
mean((svm.linear.test - data$y[!train])^2) # Test



#=================================================================================
#SVM with sigmoid kernel

# Choose the best cost
set.seed(1)
tune.sigmoid <- tune(svm, y~., data = data[train,], kernel = "sigmoid",
                 ranges = list(cost=c(0.01,0.1,1,10,100),
                 gamma=c(0.01,0.1,1,10,100)))
bestmod <- tune.sigmoid$best.model
bestmod

#Predictions
svm.sigmoid.train <- predict(bestmod, data[train,])
mean((svm.sigmoid.train-data$y[train])^2) # Training
svm.sigmoid.test <- predict(bestmod, data[!train,])
mean((svm.sigmoid.test-data$y[!train])^2) # Test
