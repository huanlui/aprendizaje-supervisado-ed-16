#Borramos los datos
rm(list=ls())

#-------------- Pregunta 1: Lectura de datos ----------#

library(MASS)
data(package="MASS")
boston<-Boston
dim(boston)
names(boston)


#-------------- Pregunta 2: datos de train ----------#
set.seed(101)
train = sample(1:nrow(boston), 300) #seleccionamos 300 valores para entrenar


#-------------- Pregunta 3: Ajuste del modelo ----------#

rf.boston = randomForest(medv~., data = boston, subset = train,ntree=500)
rf.boston

#de donde viene variance?
predicted=rf.boston$predicted
y=boston$medv[train]
1 - sum((y-predicted)^2)/sum((y-mean(y))^2)

#-------------- Pregunta 4: Arboles vs. error ----------#

plot(rf.boston)



#-------------- Pregunta 5: oob error vs test error ----------#

#vamos a intentar para cada valor de mtry posible
oob.err = double(13)
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(medv~., data = boston, subset=train, mtry=mtry, ntree = 350)
  oob.err[mtry] = fit$mse[350] #por que elijo aqui 350?
  
  pred = predict(fit, boston[-train,])
  test.err[mtry] = with(boston[-train,], mean( (medv-pred)^2 ))
}




#-------------- Pregunta 6: Grafico oob error vs test error ----------#

matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))







