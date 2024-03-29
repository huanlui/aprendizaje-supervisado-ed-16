#instalamos
install.packages("ISLR")
library("ISLR")
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

High = ifelse(NULL<=NULL, "No", "Yes")
carseats = data.frame(carseats, High)

tree.carseats = tree(NULL, data=carseats)


summary(NULL)



#---- Pregunta 3: Analisis grafico ------#

plot(NULL)
text(tree.carseats, pretty = 0)


tree.carseats



#---- Pregunta 4 y 5: Estimaci?n con decision trees ------#

set.seed(101)
train=sample(1:nrow(NULL), 250)


tree.carseats = tree(High~.-Sales, NULL, subset=train)
plot(NULL)
text(tree.carseats, pretty=0)


tree.pred = predict(tree.carseats, carseats[-NULL,], type="class")


with(carseats[-train,], table(tree.pred, High))

cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats



plot(cv.carseats)




prune.carseats = prune.misclass(tree.carseats, best = 12)
plot(prune.carseats)
text(prune.carseats, pretty=0)



tree.pred = predict(prune.carseats, carseats[-train,], type="class")
with(carseats[-train,], table(tree.pred, High))
(74 + 39) / 150

