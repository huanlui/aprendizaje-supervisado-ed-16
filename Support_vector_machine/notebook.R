rm(list=ls())

"
2asfsa
"
#----------------- SVM lineal ---------------------# 


#----------------- Pregunta 1 ---------------------# 

# 1)	Crear un data set de 20 registros con distribución N(0,1) y variable respuesta -1 y 1. Realice un plot para observar lo realizado.

set.seed(10111)
x = matrix(rnorm(40), nrow=20,ncol=2)
hist(x)
y = rep(c(-1, 1), c(10,10)) # generarme valore sde .1 y de 1 diez de cada uno
hist(y)
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)



#----------------- Pregunta 2 ---------------------# 

# 2)	Ajuste una SVM con kernel lineal para las variables anteriores. Analice los resultados.
dat = data.frame(x, y = as.factor(y)) #como factor, porque es o 1 o -1
head(dat)
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
#cost tiene que ver con la velocidad de convergencia del algortimo. 
print(svmfit)

"
Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  linear 
       cost:  10 

Number of Support Vectors:  6 ha enctronado 6 vectores de soporte. 
"
#----------------- Pregunta 3 ---------------------# 
# 3)	Realice un plot de la banda de decisión y señale los vetores soporte en el plot.

plot(svmfit, dat)

# Las x son los vectores de sopote. No entiendo como un vector es un punto

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

#los vector soporte son los putnos marcados en cuadrado. Los índices de los vectores soporte se almacenan en svmfit$index

#----------------- Pregunta 4 ---------------------# 

# 4)	Extraiga la ecuación de la recta ajustada.
vectores_soporte = x[svmfit$index,]
# %*% es producto escalar
beta = drop(t(svmfit$coefs)%*%vectores_soporte) # producto escalar de los coefcimientos por cada uno de los vectores soporte
beta0 = svmfit$rho # término independiente

# y = beta0 + beta[0] * x1 + beta[1] * x2


#----------------- Pregunta 5 ---------------------# 

# 5)	Con los coeficientes obtenidos realice un plot de la banda de decisión y los vectores soporte,
# ¿es el conjunto separable?.

plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)#subplano de decisión
points(x, col = y + 3, pch = 19) #mis datos. #col = color. 
points(x[svmfit$index,], pch = 5, cex = 2) # señalo vectores soporte
abline(beta0 / beta[2], -beta[1] / beta[2]) # esti sale de iguala la ecuación de la recta a Y = 0
#y despejar. Y= 0 es la recta en medio. 
# Aquí le sumo .1 y 1 para mostrar la otra banda
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

#no es separable, proque hay valores dentro de la banda de decisión. 
#qué hacer: cambiar el kernel

#----------------- SVM no lineal ---------------------# 




#----------------- Pregunta 1 ---------------------#  

# 1)	Con el famoso dataset de iris (flores), realice una svm multiclase y repita el procedimiento.
rm(list=ls())


data(iris)
names(iris)
n <- nrow(iris)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index # Cogemos 75% de números hasata n aleatoriamente
train_iris <- iris[tindex,]   # Create training set
test_iris <- iris[-tindex,]   # Create test set

#no linea: por ejemplo, voy a coger el radial
svmfit <- svm(Species~., data=train_iris, method="C-classification", kernel="radial", cost=10)


summary(svmfit)
svmfit$SV



#slice mantiene las dimensiones constantes para los valores indicados
#ploteo la svmfit con los datos de train, pintandome varabiel width contrs variable length
# el resto de valores los fijo : Width=la media de los witdhs de iris Length = la media de los length
# Estoy pintando una loncha!!! Si no, no podría pintar en 3d
plot(svmfit, train_iris, Petal.Width ~ Petal.Length, slice=list(Sepal.Width=mean(iris$Sepal.Width), Sepal.Length=mean(iris$Sepal.Length)))



# hacemos la prediccion y entendemos la matriz de confusion
prediction <- predict(svmfit, test_iris) #estos son datos de especies
#Matriz de confusción hay que pasarle predicción contrs real
conf_matrix <- table(prediction, test_iris$Species)

conf_matrix

#Vemos que el conjunto no es separable, pero el modelo acierto muy bien. 