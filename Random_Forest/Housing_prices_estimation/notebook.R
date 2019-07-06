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
#3)	Ajuste un modelo de random Forest con todas las variables.
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
"
4)	Realice un plot del error vs número de árboles empleados.
Ahora podemos comparar los errores de Out of Bag Sample Errors and Error on Test set en la prueba.
El modelo anterior de Random Forest eligió aleatoriamente 4 variables para ser consideradas en cada división. Ahora podríamos probar todos los 13 predictores posibles que se pueden encontrar en cada división.

"

# lo cursios es que la misma función randomForest ya te diagnostica el error según el número de árboles.

plot(rf.boston)
#ahí vemos que ea partir de 100 no merece mucho la pena, así que podemos hacer uno sólo de 100
# y vemos que el error es el mismo con menor coste computacional
randomForest(medv~., data = boston, subset = train, ntree=100)  


#-------------- Pregunta 5: oob error vs test error ----------#
# 5)	Analice el test error y el out of bag error.

"
*Out of bag error* = the mean prediction error on each training sample xᵢ, using only the trees that did not have xᵢ in their bootstrap sample.[1]. Error medio para cada muestra usando sólo los árboles que no contenían dicha muestra en su bag. 
*Test error*= error con los datos que RF no ha tocado. 
"
number_of_variables = ncol(boston) - 1
oob.err = double(number_of_variables)
test.err = double(number_of_variables)
for(nvariables in 1:number_of_variables){
  print(paste(Sys.time(), "Entrenando random Foresst con ",nvariables,"variables"))
  fit = randomForest(medv~., data = boston, subset=train, mtry=nvariables, ntree = 350)
  print(paste(Sys.time(), "Fin Entrenando random Foresst con ",nvariables,"variables"))
  # el oob error ya lo calculo automáticamente la función random forest. 
  oob.err[nvariables] = fit$mse[350] # aquí cojo el mse para 500 árbol, porque aquí se guarda el mse para usar 1m 2, etc
  pred = predict(fit, boston[-train,])# uso los de test
  # a los de test les aplico la media de los errores cuadráticos. medv es la varaible observada, pred es la predcion
  test.err[nvariables] = with(boston[-train,], mean( (medv-pred)^2 ))
}




#-------------- Pregunta 6: Grafico oob error vs test error ----------#

matplot(1:number_of_variables, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))
#veo que con 6 variables tengo lo míiom de ambos y además no están demasiado separados. 





