rm(list=ls())


#----------------- SVM lineal ---------------------# 


#----------------- Pregunta 1 ---------------------# 

set.seed(10111)
x = matrix(rnorm(NULL), NULL, NULL)
y = rep(c(-1, 1), c(NULL))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)



#----------------- Pregunta 2 ---------------------# 

dat = data.frame(x, y = as.factor(y))
svmfit = svm(NULL, data = NULL, kernel = "NULL", cost = 10, scale = FALSE)
print(svmfit)


#----------------- Pregunta 3 ---------------------# 


plot(NULL, NULL)



make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}



xgrid = make.grid(x)
xgrid[1:10,]




ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)


#----------------- Pregunta 4 ---------------------# 


beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho


#----------------- Pregunta 5 ---------------------# 


plot(NULL, col = c("red", "blue")[as.numeric(NULL)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)



#----------------- SVM no lineal ---------------------# 




#----------------- Pregunta 1 ---------------------#  

rm(list=ls())


data(iris)
n <- nrow(NULL)  # Number of observations
ntrain <- round(NULL)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, NULL)   # Create a random index
train_iris <- iris[NULL,]   # Create training set
test_iris <- iris[-NULL,]   # Create test set


svmfit <- svm(NULL, data=NULL, 
              method="C-classification", kernal=NULL, cost=10)


summary(NULL)
svmfit$SV



#slice mantiene las dimensiones constantes para los valores indicados
plot(NULL, NULL, Petal.Width ~ Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))



# hacemos la prediccion y entendemos la matriz de confusion
prediction <- predict(NULL, NULL)
conf_matrix <- table(NULL, NULL)

