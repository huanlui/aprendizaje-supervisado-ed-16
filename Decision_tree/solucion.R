#instalamos
install.packages("ISLR")
library("ISLR")
library("tree")
#Borramos los datos
rm(list=ls())

#Lectura de los datos
data(package="ISLR")
carseats<-Carseats
names(carseats)

#---- Pregunta 1: Previsualizacion de los datos ------#
hist(carseats$Sales)
hist(carseats$Sales, probability = TRUE) #para hacerlo desde 0 a 1 las alturas. 
lines(density(carseats$Sales)) #para que salgan las líneas
#Clase de cada variable 
sapply(carseats,class)

#algunas son factores


#---- Pregunta 2: Clssification decision tree ------#
#Vamos a clasiicar en ventas altas o bajas, poniendo un umbral de 8. 
#Creo nueva variable (binaria)
High = ifelse(carseats$Sales<=8, "No", "Yes")
carseats = data.frame(carseats, High)

#Creo un árbol de decisión, quitando las ventas, que son las que quiero predecir. 
tree.carseats = tree(High~.-Sales, data=carseats)


summary(tree.carseats)

"
Number of terminal nodes:  27  . Muchos nodos: overtfiitng seguramente
Residual mean deviance:  0.4575 = 170.7 / 373 
Misclassification error rate: 0.09 = 36 / 400 . Sale muy bien,pero por el overfitting. 
"

#---- Pregunta 3: Analisis grafico ------#

plot(tree.carseats)
text(tree.carseats, pretty = 0)

#Es muy tupido. 
tree.carseats



#---- Pregunta 4 y 5: Estimaci?n con decision trees ------#

set.seed(101) #para que el ejercicio sea reproduclbvle
train=sample(1:nrow(carseats), 250) #cojo 250 valores del dataset de forma aleatoria


tree.carseats = tree(High~.-Sales, carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats
summary(tree.carseats)
tree.pred = predict(tree.carseats, carseats[-train,], type="class") #hago predicción con test (todos . train)

#matriz de confuzión sólo para los datos de test
conf_matrix=with(carseats[-train,], table(tree.pred, High))
conf_matrix
accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy # ya not engo tanta.Antes era de un 90 %. Overfitting= tengo que podar. 


#---- Pregunta 5: Cost-complexity pruning ------#



# Vamos a usar la validación cruzada para podar el arbol de manera optima.
# prune.misclass usa el error de clasificación errónea como la base parahacer la poda.
#hacemos la poda. Aquí digo que podeoptimianso el error en luga rde la deviand
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats

#size: numero de nodos terminales
#dev: numero de valores mal clasificados (esto es porque 
#hemos cambiado la funcion de pruning)
#mas info: en http://mlwiki.org/index.php/Cost-Complexity_Pruning

plot(cv.carseats) #número de nodos terminales vs error de clasificación. Veo que a partir de nueve no mejror mucho, elijo 9


#seleccionamos finalmente el número de nodos terminales
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty=0)


#clasificamos los valores de prediccion. A ver si ahora hya overfitting
tree.pred = predict(prune.carseats, carseats[-train,], type="class")
with(carseats[-train,], table(tree.pred, High))
(75 + 36) / 150
#El accuracy sigue siendo el mismo, pero el árbol es menjos tupido (menos overfitting)



#---- Pregunta 6: TAREA ------#
#HAcer esto mismo pero para uno de regresión. Hacerlo dierctament con sales


