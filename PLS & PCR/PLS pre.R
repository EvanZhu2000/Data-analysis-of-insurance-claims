
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
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP', col = 2, main = 'data')
# 8 components
pls.pred = predict(pls.fit, data[!train,], ncomp = 8) 
# 7 components
pls.pred = predict(pls.fit, data[!train,], ncomp = 7) 

#data_sub
pls.fit.sub=plsr(y~., data=data_sub,subset=train,scale=TRUE, validation="CV")
summary(pls.fit.sub)
validationplot(pls.fit.sub, val.type = 'MSEP', col = 3, main = 'data_sub')
pls.pred.sub = predict(pls.fit.sub, data_sub[!train,], ncomp = 4) # 7 components




