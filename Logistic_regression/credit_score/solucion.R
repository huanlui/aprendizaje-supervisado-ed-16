rm(list=ls())
install.packages('data.table')
library('data.table')
#load dataset, you can use choose file also
my_data<-as.data.frame(fread("./data_in/data_scoring.csv",sep=";"))
# my_data<-as.data.frame(fread(choose.files()))

my_data_train<-my_data[1:400,]

my_data_train$Y<-ifelse(my_data_train$Y=="malo",0,1)



##########################Pregunta 1########################################

#Creamos los modelos de Regresión Logística simples

m1<-glm(Y~X1, family=binomial(link = "logit") ,data = my_data_train)
anova(m1, test="Chi")

m2<-glm(Y~X2, family=binomial(link = "logit") ,data = my_data_train)
anova(m2, test="Chi")

m3<-glm(Y~X3, family=binomial(link = "logit") ,data = my_data_train)
anova(m3, test="Chi")

m4<-glm(Y~X4, family=binomial(link = "logit") ,data = my_data_train)
anova(m4, test="Chi")

m5<-glm(Y~X5, family=binomial(link = "logit") ,data = my_data_train)
anova(m5, test="Chi")

m6<-glm(Y~X6, family=binomial(link = "logit") ,data = my_data_train)
anova(m6, test="Chi")

m7<-glm(Y~X7, family=binomial(link = "logit"), data = my_data_train)
anova(m7, test="Chi")


##################Pregunta 2#############################

#Buscamos hacer una seleccion de las varibles.

#Modelo utilizando todas las  variables menos X4 y X7

m8<-glm(Y~X1+X2+X3+X4+X5+X6+X7, binomial(link = "logit"), data = my_data_train)
anova(m8, test="Chi")

#Eliminamos X1
model<-glm(Y~X2+X3+X4+X5+X6+X7, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X2
model<-glm(Y~X1+X3+X4+X5+X6+X7, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X3
model<-glm(Y~X1+X2+X4+X5+X6+X7, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X4
model<-glm(Y~X1+X2+X3+X5+X6+X7, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X5
model<-glm(Y~X1+X2+X3+X4+X6+X7, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X6
model<-glm(Y~X1+X2+X3+X4+X5+X7, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X7
model<-glm(Y~X1+X2+X3+X4+X5+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Renombramos el modelo m8 quitando la variable X7 y repetimos el procedimiento

m8<-glm(Y~X1+X2+X3+X4+X5+X6, binomial(link = "logit"), data = my_data_train)
anova(m8, test="Chi")

#Eliminamos X1
model<-glm(Y~X2+X3+X4+X5+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X2
model<-glm(Y~X1+X3+X4+X5+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X3
model<-glm(Y~X1+X2+X4+X5+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X4
model<-glm(Y~X1+X2+X3+X5+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X5
model<-glm(Y~X1+X2+X3+X4+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X6
model<-glm(Y~X1+X2+X3+X4+X5, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Renombramos el modelo m8 quitando la variable X5 y repetimos el procedimiento

m8<-glm(Y~X1+X2+X3+X4+X6, binomial(link = "logit"), data = my_data_train)
anova(m8, test="Chi")

#Eliminamos X1
model<-glm(Y~X2+X3+X4+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X2
model<-glm(Y~X1+X3+X4+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X3
model<-glm(Y~X1+X2+X4+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X4
model<-glm(Y~X1+X2+X3+X6, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Eliminamos X6
model<-glm(Y~X1+X2+X3+X4, binomial(link = "logit"), data = my_data_train)

#Comparando Con el modelo con todas las variables
anova(m8,model, test="Chi")

#Renombramos el modelo m8 quitando la variable X3

m8<-glm(Y~X1+X2+X4+X6, binomial(link = "logit"), data = my_data_train)
anova(m8, test="Chi")

#Usamos la función step
stepAIC(glm(Y~X1+X2+X3+X4+X5+X6+X7,binomial(link = "logit"), data = my_data_train))


#########################Pregunta 3###############################

exp(confint(m8,level=0.9))

##########################Pregunta 4###################################

exp(coefficients(m8))


############################Pregunta 5#######################################


my_pred<-predict(m8,newdata= my_data_train,type = "response")


############################Pregunta 6#######################################


pred_cutoff<-function(epsilon,pred){
  result<-ifelse(pred < epsilon,0,1)
  return(result)
}

# Calculo predicciones cogiendo distintos umbrales
pred_cutoff_03 <- pred_cutoff(0.3,my_pred)
pred_cutoff_05 <- pred_cutoff(0.5,my_pred)
pred_cutoff_08 <- pred_cutoff(0.8,my_pred)

#Ahora voy a ver las matrices de confusión para cada uno de los umbrales, a ver
#si están bien
#Es decir, voy a comparar mi predcción con mi real (my_data_train)
conf_matrix_03<-table(pred=pred_cutoff_03,actual= my_data_train$Y)
conf_matrix_05<-table(pred=pred_cutoff_05,actual= my_data_train$Y)
conf_matrix_08<-table(pred=pred_cutoff_08,actual= my_data_train$Y)

conf_matrix_03
conf_matrix_05
conf_matrix_08

#Diagnoal/Total = Accuracy = TP + TN / TODO
#TPR= TP / P. Los que he dicho que son positivos entre todos los positivos. 
#TNR= TN / N. 
#Un model es el que tiene un alto accurddaay y una tasa de TPR y TNR balanceados. 
# Ver https://en.wikipedia.org/wiki/Sensitivity_and_specificity

accuracy = sum(diag(conf_matrix_03))/sum(conf_matrix_03)
accuracy
conf_matrix_03[2,2]/(conf_matrix_03[2,2]+conf_matrix_03[1,2]) #tpr
conf_matrix_03[1,1]/(conf_matrix_03[1,1]+conf_matrix_03[2,1]) #tnr
#Este modelo me está dando "siempre quelluevue"

sum(diag(conf_matrix_05))/sum(conf_matrix_05)
conf_matrix_05[2,2]/(conf_matrix_05[2,2]+conf_matrix_05[1,2]) #tpr
conf_matrix_05[1,1]/(conf_matrix_05[1,1]+conf_matrix_05[2,1]) #tnr

sum(diag(conf_matrix_08))/sum(conf_matrix_08)
conf_matrix_08[2,2]/(conf_matrix_08[2,2]+conf_matrix_08[1,2]) #tpr
conf_matrix_08[1,1]/(conf_matrix_08[1,1]+conf_matrix_08[2,1]) #tnr

#De momento me da el 0ñ5 unos valores de prt y tnr más balanceados. 

############################Pregunta 7#######################################
install.packages('verification')
library('verification')

r<-roc.area(my_data_train$Y,my_pred)
roc.plot(my_data_train$Y,my_pred,main=paste0("Curva ROC con AUC=", r$A))

#AUC: Area bajo la curva ROC. Curva roc, pinta la hit rate frente a la false alarm rate. 
#Si tengo 


############################Pregunta 8#######################################

#¿Cómo buscamos el mejor umbral
epsilon<-seq(length=100, from=0, to=1)

df<-data.frame(accuracy=rep(0,100),epsilon)
Y<-my_data_train$Y

# Cojo umbral de uno en 1 y voy probando
#el que me maximize el accuracy
for(i in 1:100){
  print(i)
  PYeq1<-ifelse(my_pred<epsilon[i],0,1)
  confusion_matrix<-table(PYeq1,Y)
  df$accuracy[i]<-sum(diag(confusion_matrix))/(sum(confusion_matrix))
  
}

plot(epsilon,df$accuracy,xlab="Epsilon value",ylab="Accuracy",type="l",main="Optimizaci\u00f3n accuracy \n datos de entrenamiento")
df[df$accuracy==max(df$accuracy),] #salen dos valores que maximiazn.


PYeq1<-ifelse(my_pred<df[df$accuracy==max(df$accuracy),]$epsilon,0,1)
confusion_matrix<-table(PYeq1,Y)
confusion_matrix
confusion_matrix[2,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #fpr
confusion_matrix[1,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) #fnr
confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) #tpr
confusion_matrix[1,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #tnr



r<-roc.area(my_data_train$Y,my_pred)
roc.plot(my_data_train$Y,my_pred,main=paste0("Curva ROC con AUC=", r$A))




############################Pregunta 9#######################################

# Como tenemos buena predicción para los positivos, pero peor para el mínino, convendría
# balancear las clases. Para ello, vamos a entranar un modelo con una muesdta que tenga
# un mismo número de negativos y positivos. 
# De los buenos, me voy a coger sólo unos cuantos para tener el mismo número de los malos. 
#### a) Balanceo de muestras

rm(list=ls())

#load dataset, you can use choose file also
my_data<-as.data.frame(fread("./data_in/data_scoring.csv",sep=";"))
#convertimos texto a 1 y 0, para que sea más matemático todo
my_data$Y<-ifelse(my_data$Y=="malo",0,1)
# importante randomizar para no depender de orden, le cambiamos el orden.
my_data<-my_data[sample(1:nrow(my_data)),] 
# cogemos 20% para test
my_data_test<-my_data[1:(nrow(my_data)*0.2),]
rownames(my_data)#los nonbres del todo el dataset
rownames(my_data_test)#los nombres del dataset de tests
setdiff(rownames(my_data),rownames(my_data_test)) # dame del todoal los que no estén en test
#Y ahora , el de entrenamiento
my_data_train<-my_data[setdiff(rownames(my_data),rownames(my_data_test)),]
#De entrenamiento, cógeme solo los malos
my_data_train_0<-my_data_train[which(my_data_train$Y==0),]
# Y los bueno.s
my_data_train_1<-my_data_train[which(my_data_train$Y==1),]

#cógeme de los buenos sólo tantos como haya malos
my_sample<-sample(rownames(my_data_train_1),nrow(my_data_train_0))
#meto esos buenos en el de entrenamiento
my_data_train_sampled<-my_data_train[my_sample,]
#añado los malos
my_data_train<-rbind(my_data_train_sampled,my_data_train_0)
table(my_data_train$Y)

#SMOTE: otra técnica para balancear. En lugar de quitas, creo datos sintéticos.

#RESUMEN: SI NO BALANCEAS, SE SOBREENTRENA LA PARTE DOMINANTE. 

#y los que me sobran buenosmloes lelvo al de test (para no perder esos datos)
my_data_test<-my_data[setdiff(rownames(my_data),rownames(my_data_train)),]

nrow(my_data_train)+nrow(my_data_test)==nrow(my_data)
print(paste0("Filas train=",nrow(my_data_train),"     Filas test=",nrow(my_data_test)))

#### b) Modelo todas las variables
my_model<-glm(Y~X1+X2+X3+X4+X5+X6+X7, binomial(link = "logit"), data = my_data_train)


#### c) Prediccion
my_pred<-predict(my_model,newdata= my_data_train,type = "response")



epsilon<-seq(length=100, from=0, to=1)

df<-data.frame(accuracy=rep(0,100),epsilon)
Y<-my_data_train$Y



for(i in 1:100){
  print(i)
  PYeq1<-ifelse(my_pred<epsilon[i],0,1)
  confusion_matrix<-table(PYeq1,Y)
  df$accuracy[i]<-sum(diag(confusion_matrix))/(sum(confusion_matrix))
  
}

plot(epsilon,df$accuracy,xlab="Epsilon value",ylab="Accuracy",type="l",main="Optimizaci\u00f3n accuracy")
df[df$accuracy==max(df$accuracy),]

epsilon_optimal<-df[df$accuracy==max(df$accuracy),]$epsilon[1]
PYeq1<-ifelse(my_pred<epsilon_optimal,0,1)
confusion_matrix<-table(PYeq1,Y)
confusion_matrix
confusion_matrix[2,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #fpr
confusion_matrix[1,2]/(confusion_matrix[1,2]+confusion_matrix[2,2]) #fnr
confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) #tpr
confusion_matrix[1,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #tnr

#Ahora en test:
my_pred<-predict(my_model,newdata= my_data_test,type = "response")
Y<-my_data_test$Y
PYeq1<-ifelse(my_pred<epsilon_optimal,0,1)
confusion_matrix<-table(PYeq1,Y)
accuracy_test<-sum(diag(confusion_matrix))/(sum(confusion_matrix))
tpr_test<-confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) #tpr
tnr_test<-confusion_matrix[1,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #tnr

### d) Cross-validation

#K-Fold cross validation



rm(list=ls())

#load dataset, you can use choose file also
my_data<-as.data.frame(fread("./data_in/data_scoring.csv",sep=";"))
my_data$Y<-ifelse(my_data$Y=="malo",0,1)
#randomizo
my_data<-my_data[sample(1:nrow(my_data)),]


n_fold<-5 # es 5 es el K

#creo dos dataframes vacías con columnas accuracy, epsion, ptr,tnr y AUC. 
# y cinco columnas (vamos a hacer 5 veces el algortimo de arriba)
my_performance_data_train<-data.frame(accuracy=rep(0,n_fold),epsilon=rep(0,n_fold),tpr=rep(0,n_fold),tnr=rep(0,n_fold),AUC=rep(0,n_fold))
my_performance_data_test<-data.frame(accuracy=rep(0,n_fold),epsilon=rep(0,n_fold),tpr=rep(0,n_fold),tnr=rep(0,n_fold),AUC=rep(0,n_fold))



for(i in 1:n_fold){
  print(i)
  
  #Balanceo clasess
  #Ventana de datos, ver imange  de k-fold
  my_data_test<-my_data[((i-1)*nrow(my_data)/n_fold+1):(i*nrow(my_data)/n_fold),]
  #Restantes para el training.
  my_data_train<-my_data[setdiff(rownames(my_data),rownames(my_data_test)),]
  
  my_data_train_0<-my_data_train[which(my_data_train$Y==0),]
  my_data_train_1<-my_data_train[which(my_data_train$Y==1),]
  
  #Balanceo, (undersampling)
  my_sample<-sample(rownames(my_data_train_1),nrow(my_data_train_0))
  my_data_train_sampled<-my_data_train[my_sample,]
  #a los datos de entredamientole añados los sapleados de 1 más todos lode 0
  my_data_train<-rbind(my_data_train_sampled,my_data_train_0)
  #para test, los demas
  my_data_test<-my_data[setdiff(rownames(my_data),rownames(my_data_train)),]


  
  #Entreno modelo 
  my_model<-glm(Y~X1+X2+X3+X4+X5+X6+X7, binomial(link = "logit"), data = my_data_train)

  
  #Prediccion y cutoff optimo
  #Precit coge los valores y los pasa por el modelo. 
  my_pred<-predict(my_model,newdata= my_data_train,type = "response")
  
  
  #Vamos a budcar el cutoff optimo usando pasos de 1 en 1. 
  epsilon<-seq(length=100, from=0, to=1)
  
  df<-data.frame(accuracy=rep(0,100),epsilon)
  Y<-my_data_train$Y
  
  
  
  for(j in 1:100){
    PYeq1<-ifelse(my_pred<epsilon[j],0,1)
    confusion_matrix<-table(PYeq1,Y)
    df$accuracy[j]<-sum(diag(confusion_matrix))/(sum(confusion_matrix))
  }
  
  #Me quedo con el valor de accuracy mejor(de entre esos 100 umbrales, el que me da mejor)
  my_performance_data_train$accuracy[i]<-unique(max(df$accuracy))
  
  epsilon_optimal<-df[df$accuracy==max(df$accuracy),]$epsilon[1]
  my_performance_data_train$epsilon[i]<-epsilon_optimal
  
  
  PYeq1<-ifelse(my_pred<epsilon_optimal,0,1)
  confusion_matrix<-table(PYeq1,Y)
  my_performance_data_train$tpr[i]<-confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) #tpr
  my_performance_data_train$tnr[i]<-confusion_matrix[1,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #tnr
  # no soy capaz de instalar my_performance_data_train$AUC[i]<-roc.area(my_data_train$Y,my_pred)$A
  
  
  #ahora paso el modelo con le umbral oṕtimo a los datos de test
  my_pred<-predict(my_model,newdata= my_data_test,type = "response")
  Y<-my_data_test$Y
  PYeq1<-ifelse(my_pred<epsilon_optimal,0,1)
  confusion_matrix<-table(PYeq1,Y)
  my_performance_data_test$accuracy[i]<-sum(diag(confusion_matrix))/(sum(confusion_matrix))
  my_performance_data_test$epsilon[i]<-epsilon_optimal
  
  my_performance_data_test$tpr[i]<-confusion_matrix[2,2]/(confusion_matrix[2,2]+confusion_matrix[1,2]) #tpr
  my_performance_data_test$tnr[i]<-confusion_matrix[1,1]/(confusion_matrix[2,1]+confusion_matrix[1,1]) #tnr
 # my_performance_data_test$AUC[i]<-roc.area(my_data_test$Y,my_pred)$A
  
  
}

sapply(my_performance_data_train, mean) # cogemos la media porque esta es sensisble a outliers
sapply(my_performance_data_test, mean)




############################ Pregunta 10 #######################################

# my_new_data<-data.frame(X1="cuenta.mala",X2= 48, X3="pre.malo", X4="profesional",X5=2169, X6="hombre", X7="no.solo")
# my_new_data<-data.frame(X1="cuenta.buena",X2= 33, X3="pre.bueno", X4="profesional",X5=2764, X6="mujer", X7="solo")

my_new_data<-data.frame(X1="cuenta.buena",X2=24,X3="pre.bueno",X4="personal",X5=3000,X6="hombre",X7="no.solo")
my_pred<-predict(my_model,newdata= my_new_data,type = "response")
PYeq1<-ifelse(my_pred<epsilon_optimal,0,1)

if(PYeq1==1){print("¡Enhorabuena! su cr\u00e9dito ha sido aprobado")}else{"Lo sentimos... no es elegible para el cr\u00e9dito que solicita"}


############################ Pregunta 11 #######################################



my_model<-glm(Y~X1+X2+X3+X4+X5+X6+X7, binomial(link = "logit"), data = my_data)
exp(coefficients(my_model))



