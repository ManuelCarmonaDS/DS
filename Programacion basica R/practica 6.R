###PRACTICA 6

#BLOQUE 4 TEMA 1


a<-c(1,1,6); b<-c(4,5,1)
a-b
sum((a-b)^2)
sqrt(sum((a-b)^2))

#pag 7
#Lo primero que hacemos es descargar la libreria. 

source("http://www.bioconductor.org/biocLite.R")
biocLite()
biocLite("multtest")
library(multtest);
data(golub)
golub.gnames[1042,]

#pag 8 

#Queremos buscar el patron Cyclin en el vector. Las posiciones nos las da INDEX.
#grep: busca coincidencias con el patrón de argumento dentro de cada elemento de un vector de caracteres: 
# difieren en el formato de una cantidad de detalles en los resultados.
index<-grep("Cyclin", golub.gnames[,2])       #esto es un ejemplo de funcion de distribucion de secuencias
index
golub.gnames[index,2]

#pag9

#ahora vamos a calcular la distancia euclídea (se pueden usar otras distancias) entre todos los genes cuyo
#descriptor contengala palabra Cyclin, hacemos:
dist.cyclin<-dist(golub[index,],method="euclidian")    #esto nos da las distancias entre datos
diam<-as.matrix(dist.cyclin)    #aunque ya nos lo da como una matriz, lo organizamos como matriz (de otro modo)
colnames(diam)<-golub.gnames[index,3]      #el nnombre de las columnas va a ser...
rownames(diam)<-colnames(diam)             #el nombre de las filas va a ser...(los mismos)
diam[1:5,1:5]                              #muestrame de fila 1 a fila 5 y de col 1 a col 5 (porque es muy larga)


#pag10

# Ahora vamos a encontrar los diez genes más cercanos a uno que ya tenemos. 
#Vamos a calcular la dist. euclídea del gen 1389_at a todos los genes
#Esto se puede hacer con la funcion genefinder especificando un índice o un nombre.

#biocLite("genefilter")
library("genefilter")
#biocLite("ALL")
library("ALL")
data(ALL)
closeto1389_at<-genefinder(ALL, "1389_at", 10, method="euc")   #nos da los 10 mas parecidos
closeto1389_at[[1]]$indices
round(closeto1389_at[[1]]$dists,1)
featureNames(ALL)[closeto1389_at[[1]]$indices]
str(closeto1389_at)

#pag 13

#Mínima distancia:
#considerando tantos clústeres como nodos, calculamos la matriz de distancia entre cada par de clústeres. Luego
#unimos en un clúster los clústeres más cercanos y calculamos el centroide, que será la referencia para la
#siguiente matriz de distancias. Si el número de clústers es K parar, si no volvemos al paso 2.

names<-list(c("g1","g2","g3","g4","h5"),c("p1","p2")) #valores de genes de 2 personas p1 y p2
sl.clus.dat<-matrix(c(1,1,1,1.1,3,2,3,2.3,5,5),ncol=2,byrow=TRUE,dimnames=names)
sl.clus.dat
#el siguiente grafico Calcula las distancias entre los genes y realiza un análisis de enlace único
#(single linkage)
plot(sl.clus.dat,type="n",xlim=c(0,6), ylim=c(0,6))
text(sl.clus.dat,labels=row.names(sl.clus.dat))
print(dist(sl.clus.dat,method="euclidian"),digits=3)

############pag 122 Krijnen
#Al comienzo, cada punto de datos se ve como un clúster separado. Entonces los dos puntos más cercanos (genes) de
#la matriz de distancia euclidiana son g1 y g2, teniendo d (g1; g2) = 0:10. 
#Estos dos puntos de datos se fusionan en un grupo, digamos I = fg1; g2g. 
#En el gráfico, esto se ilustra mediante la línea horizontal a la altura 0.10 en el árbol. 
#Los otros tres puntos de datos g3; g4; g5 se ven como tres grupos diferentes. A continuación, 
#la distancia mínima entre conglomerados se puede leer desde la matriz de distancia euclidiana. 
#El más pequeño es d (g3; g4) = 0:30, así que el nuevo grupo J = fg3; g4g corresponde a la línea horizontal
#a la altura 0.30.
#Ahora hay tres clusters, I, J y K = fx5g. Desde la matriz de distancia euclidiana, se puede observar que
#la distancia entre el grupo I y J es de 2.19. Por lo tanto, el clúster I y J se fusionaron en uno. 
#Finalmente, la distancia entre el grupo fg1; g2; g3; g4g, y el punto de datos g5 es igual a d (g4; g5) = 3:36
        
sl.out<-hclust(dist(sl.clus.dat,method="euclidian"), method="single")
plot(sl.out)


#pag 15

#cabiamos el imput para observar cambios en el dendograma:
sl.out<-hclust(dist(rnorm(20,0,1),method="euclidian"),method="single")
plot(sl.out)
#Del árbol resultante podríamos tener la impresión de que hay cinco grupos separados en los datos. 
#Sin embargo, no existe un proceso subyacente de generación de datos que produzca clusters separados
#de diferentes poblaciones.


#pag 16
#Sin embargo, si los datos son generados por diferentes distribuciones normales, entonces hay diferentes 
#procesos que producen clusters separados. Para ilustrar esto, se tomamos diez puntos de datos de la
#población N (0; 0: 1), diez de N (3; 0: 5) y diez de N (10; 1).
#Obtenemos un dendograma con muchos mas subgrupos

x<-c(rnorm(10,0,0.1),rnorm(10,3,0.5),rnorm(10,10,1.0))
plot(hclust(dist(x,method="euclidian"),method="single"))

#Del árbol que obtenemos se puede observar que existen claramente tres grupos. 
#Estos ejemplos nos hacen ver que los resultados del análisis de conglomerados pueden revelar
#muy bien las propiedades de la población, pero que es necesaria cierta precaución.

#Pag 17

#El siguiente gráfico muestra que los grupos de pacientes ALL y AML difieren con respecto a los valores de 
#expresión genética "CCND3 Cyclin D3" and "Zyxin"

data(golub, package="multtest")
clusdata<-data.frame(golub[1042,],golub[2124,])
colnames(clusdata)<-c("CCND3 Cyclin D3", "Zyxin")
gol.fac<-factor(golub.cl,levels=0:1, labels=c("ALL","AML"))
plot(clusdata, pch=as.numeric(gol.fac))

#En este sentido, el siguiente árbol corresponde al "single linkage cluster anlysisis" completo. Ademas de las tres
#expresiones, el arbol muestra 2 clusters correspondientes a los dos grupos de pacientes.
legend("topright", legend=c("ALL","AML"),pch=1:2)
plot(hclust(dist(clusdata,method="euclidian"),method="single"))


#pag 20

# El análisis de conglomerados K-means es un método que se define minimizando la suma de cuadrados
#dentro de los clústers K.
data<-rbind(matrix(rnorm(100,0,0.5),ncol=2),matrix(rnorm(100,2,0.5),ncol=2))
cl<-kmeans(data,2)
cl

#El output del análisis de clúster k-means se asigna a la lista cl. Observamos que las medias de los clusters
#son bastante cercanas a la las de la población (0; 0) y (2; 2). El vector Clustering indica a qué clúster pertenece
#cada punto de datos (gen)


#pag21

#mirar ejemplo clusters del documento de 300pags. pag 127

#La variable cl$cluster se puede usar para especificar el color de cada punto de datos de un diagrama
plot(data, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 10, cex=2)
#Los puntos de datos están representados por circulos negros y rojos en función del cluster al que pertenezcan y 
#los clusters estan representados por las dianas. 


#pag23

library(TeachingDemos)
put.points.demo()
#esta libreria me permite establcer puntos para observar la tendencia que sigue la distribución.



#ejecutar ejemplo 2 pagina 137 de Krijnen

#Los autovalores (eigenvalues) representan una cantidad de varianza relacionada con el componente.
eigen(cor(golub))$values[1:5]


#Dado que los autovalores están ordenados en orden decreciente, el sexto al trigésimo son más pequeños que uno. 
#Motivo por el cual estos serán descartados. El primer valor propio es de lejos el más grande, lo que indica que 
#las personas dependen en gran medida. Aplicando los métodos bootstrap para estimar el intervalo del 95% 
#para los autovalores, obtenemos los siguientes intervalos:
data <- golub; p <- ncol(data); n <- nrow(data) ; nboot<-1000
eigenvalues <- array(dim=c(nboot,p))
for (i in 1:nboot){dat.star <- data[sample(1:n,replace=TRUE),]
eigenvalues[i,] <- eigen(cor(dat.star))$values}

for (j in 1:p) print(quantile(eigenvalues[,j],c(0.025,0.975)))

        for (j in 1:5) cat(j,as.numeric(quantile(eigenvalues[,j], #cat da salida a los objetos y concatena las representaciones
                                                 + c(0.025,0.975))),"\n" )



#Bloque 4 tema 2
#pagina 148 de Krijnen, chapter 8

library(ROCR)
gol.true<-factor(golub.cl,levels=0:1,labels=c("TRUE","FALSE")) #aqui medimos si se parece al gen "ciclina" el resto de resgistros
pred<-prediction(golub[1042,],gol.true)
perf<-performance(pred,"tpr","fpr")
plot(perf) #nos sale que el test es de muy buena calidad, cuanto mas pegado a la zona izquierda y superior, mejor. 

data(golub, package = "multtest")
gol.true <- factor(golub.cl,levels=0:1,labels= c(" ALL","not ALL"))
gol.pred <- factor(golub[1042,]>1.27,levels=c("TRUE","FALSE"), #si el factor sale con valor >1.27, TRUE, que se corresponde con ALL
                   labels=c("ALL","notALL"))
table(gol.pred,gol.true) #en el resultado: 27 pacientes de leucemia ALL, 25 bien clasificados, 2 falsos negativo. 
#De los que no tienen ALL, 10 se han clasificado bien y uno es un falso negativo

#En este sentido, la true positive rate es 0.93 (25/27) y la false positive rate es  0.09 (1/11)
