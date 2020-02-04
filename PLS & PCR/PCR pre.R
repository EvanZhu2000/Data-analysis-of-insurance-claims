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
summary(pcr.fit)
validationplot(pls.fit,val.type="MSEP", col = 2, main = 'data')
pcr.pred = predict(pcr.fit, data[!train,], ncomp = 8) 
mean((round(pcr.pred)-data$y[!train])^2) #RSS 

#data_sub
pcr.fit=pcr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pls.fit,val.type="MSEP", col = 3, main = 'data_sub')
pcr.pred = predict(pcr.fit, data_sub[!train,], ncomp = 7) 
mean((round(pcr.pred)-data_sub$y[!train])^2) #RSS 