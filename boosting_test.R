library(grf)
n = 2000; p = 10
X = matrix(runif(n * p), n, p)
mu = 2 * X[,1] * X[,2] + 3 * X[,3] + 4 * X[,4]
Y = mu + rnorm(n)
X.test = X[1801:2000,]
Y.test = Y[1801:2000]
X.train = X[1:1800,]
Y.train = Y[1:1800]

regf1 = regression_forest(X.train,Y.train)
Y.hat1 = regf1$predictions

regf2 = regression_forest(X.train,Y.train-Y.hat1)
Y.hat2 = Y.hat1 + regf2$predictions

regf3 = regression_forest(X.train,Y.train-Y.hat2)
Y.hat3 = Y.hat2 + regf3$predictions 

regf4 = regression_forest(X.train,Y.train-Y.hat3)
Y.hat4 =  Y.hat3 + regf4$predictions 

#Training error 
c(mean((mu[1:1800]- Y.hat1)^2), mean((mu[1:1800]- Y.hat2)^2), mean((mu[1:1800]- Y.hat3)^2), mean((mu[1:1800] - Y.hat4)^2))

#Test error 
Y.test.hat1 = predict(regf1,X.test)$predictions
Y.test.hat2 = Y.test.hat1 + predict(regf2,X.test)$predictions
Y.test.hat3 = Y.test.hat2+predict(regf3,X.test)$predictions
Y.test.hat4 = Y.test.hat3 +predict(regf4,X.test)$predictions

mean((mu[1801:2000]-Y.test.hat1)^2)
mean((mu[1801:2000]-Y.test.hat2)^2)
mean((mu[1801:2000]-Y.test.hat3)^2)
mean((mu[1801:2000]-Y.test.hat4)^2)



