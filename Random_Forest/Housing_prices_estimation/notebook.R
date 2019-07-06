#Borramos los datos
rm(list=ls())

#-------------- Pregunta 1: Lectura de datos ----------#

# 1)	Mediante la librería MASS cargue los datos de Boston.
library("MASS")
data(package="MASS")
boston<-Boston
dim(boston)
names(boston)


#-------------- Pregunta 2: datos de train ----------#
# 2)	Separe el dataset en un conjunto de train de 300 registros y el resto tómelo como test.
set.seed(101)
#randomizo
boston<-boston[sample(1:nrow(boston)),] # sample sin  parámetro, coge todos
#Cojo trescientos para entrenamiento (estoy haciendo doble random que no haría falta. )
train = sample(1:nrow(boston), 300) # ojo, train es un conjunto de índices


#-------------- Pregunta 3: Ajuste del modelo ----------#
# medv~. Significa que quiero predecir la variable medv. el . es que no quiero eliminar variables
# No elimino ningunaporque el mismo algortirmo de random forest me cogerá aleatoriamente las variables
# para cada árbol y luego combinará. 
#Subset dice los índices a coger para entrenar. 
rf.boston = randomForest(medv~., data = boston, subset = train)
rf.boston

"
 randomForest(formula = medv ~ ., data = boston, subset = train) 
               Type of random forest: regression
                     Number of trees: 500 esto eslo que hace por defecto, la función randomForest te permite personalizarlo
No. of variables tried at each split: 4  ha usado 4 variables para entrenar cada 

          Mean of squared residuals: 12.31419 . 
          Es la media de los errores cuadráticos. Intentamos minimizarla
                    % Var explained: 85.35
"



#-------------- Pregunta 4: Arboles vs. error ----------#

plot(NULL)



#-------------- Pregunta 5: oob error vs test error ----------#


oob.err = double(13)
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(NULL, data = NULL, subset=NULL, mtry=NULL, ntree = 350)
  oob.err[mtry] = fit$mse[350]
  pred = predict(fit, boston[-NULL,])
  test.err[mtry] = with(boston[-NULL,], mean( (medv-pred)^2 ))
}




#-------------- Pregunta 6: Grafico oob error vs test error ----------#

matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))





