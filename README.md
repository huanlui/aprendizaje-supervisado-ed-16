# aprendizaje-supervisado-ed-16


# Repaso

* Supervisado vs No supervisado.

![](http://www.diegocalvo.es/wp-content/uploads/2017/07/clasificacion-de-machine-learning.jpg)

* Dataset de train y test: entrenamos nuestro modelo, ¿cómo sabemos que es bueno?
* Overffiting: Si intentas reajustar demasiado no predices. 

![distintos grados](https://i2.wp.com/elnuevoparquet.com/wp-content/uploads/2017/08/b1-1.jpg?resize=344%2C308)

* Matriz de confusión y medidas de rendimiento: ¿cómo mido qué tan bueno es mi modelo?

![matriz de confusion](https://3.bp.blogspot.com/--jLXutUe5Ss/VvPIO6ZH2tI/AAAAAAAACkU/pvVL4L-a70gnFEURcfBbL_R-GnhBR6f1Q/s1600/ConfusionMatrix.png)

![otra con TNR](https://www.researchgate.net/publication/277034344/figure/fig7/AS:267804859432977@1440861062731/A-Confusion-matrix-and-its-relation-to-predictive-accuracy-terms-TPRTrue-Positive-Rate.png)

Lo suyo es que el TRR y el TNR estuvieran balanceados. Para ello, se hacen que en el conjunto de entrenamiento tengamos los mismos positivos que negativos (quitando del mayoritario de forma aleatoria, undersampling).  Los que les sobra, los lleva a lo de test. 

También se podría hacer oversampling usando una técnica que se llama SMOTE (Syntetic Minority Oversamplig Technique). 

Ver por ejemplo https://www.researchgate.net/publication/229045207_SMOTE-I_mejora_del_algoritmo_SMOTE_para_balanceo_de_clases_minoritarias. 

* Cross - validation: ¿depende mi modelo de los datos de entrenamiento?

   Es decir, ¿tiene mucha overfitting? Voy intentnado coger todos los datos de mi dataset divididos por ventanas.

* El Random Forest lo considera muy potente. 

* Identificar outliers con distancia de cook. 

* Se pretende que la variable respuesta/independiente tenga distribución normal. 

* Si tenemos variables que están relacionadas una con otra, nuestro poder de predicción empeorará, por ello habrá que hacer una selección de variables. 

* R^2 : Mean Squeared Errors / NumMuestras

* Residuales: diferencia entre estimado y real. Tenían que tener varianza constante, es decir, que quedaran entreuna banda. 

![img](https://4.bp.blogspot.com/-TMqNoVSGUkE/WKNfxxbtXEI/AAAAAAAAAR4/SFbAr0JEI6Yw0qVIdJo63zAF1UpFwkzuQCLcB/s640/normalidad.jpg)

Este enlace es muy bueno: http://doestatistics.blogspot.com/2017/02/comprobacion-de-la-adecuacion-del.html

* Elección de cutoff: elegir el cuttof que maximice el accuracy, usando 100 cutoofs distintos. 

* Curva ROC:

![roc curve](http://www.hrc.es/bioest/roc_21.gif)

![area under curve](https://cdn-images-1.medium.com/max/842/1*pk05QGzoWhCgRiiFbz-oKQ.png)

Ver: http://www.hrc.es/bioest/roc_1.html
Ver: https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5

# Decission Tree

Tanto para estimar como para clasificar. 

Para **estimar** es un poco raro. Lo que se hace es dividir en zonas y en cada zona se pone la media de cada región.  

En estimación, intentan minimizar el error cuadrático medio. 

![estimación](https://s3.amazonaws.com/acadgildsite/wordpress_images/Data+Science/Decision+Tree+Regresssor/image1.png)

Para clasificar:


# Random Forest: el favorito de Henry. 

En lugar de usar el tío más fuerte del mundo para mover un camión coges a 100 tíos normales, te irá mejor. 

Suelen ser **robustos**, es decir, **no suelen tener overfitting**. 

![random forest con bagging](https://bookdown.org/content/2031/images/bootstraping.png)

Ver https://bookdown.org/content/2031/ensambladores-random-forest-parte-i.html

Para clasificación, hay dos formas de hacer la media:
* Por mayoría:
* Por probalidad. Es decir, si tenemos 400 Sís y 1 noes, tenemos 4/5. Pondremos un umbral y elegiremos por eejmploque desde 3/5 es sí. 

# SVM
Máquinas de vector de soporte. Elegimos la recta que separa la distancia entre los vectores soporte. 

Realmente los vector de soporte son _puntos frontera_ . Intentamos encontrar la reta que maximie la media de esas istancias. 

* Kernel Tricks*
La manera más simple de realizar la separación es mediante una línea recta, un plano recto o un hiperplano N-dimensional.

Desafortunadamente los universos a estudiar no se suelen presentar en casos idílicos de dos dimensiones como en el ejemplo anterior, sino que un algoritmo SVM debe tratar con a) más de dos variables predictoras, b) curvas no lineales de separación, c) casos donde los conjuntos de datos no pueden ser completamente separados, d) clasificaciones en más de dos categorías.

![svm](https://dlegorreta.files.wordpress.com/2015/07/mmc.png)







