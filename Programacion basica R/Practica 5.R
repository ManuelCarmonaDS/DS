
######EJERCICIO 1.Búsqueda y Selección. PRACTICA 5

datos<-read.csv("C:/Users/Manuel/Desktop/CUNEF/Programacion R/bloque 3/PRACTICA5/student_census.csv", skip=0,header=T, sep=",")
View(datos)

#Para formar el equipo con 5 jugadores, primero queremos localizar a los estudiantes que jueguen al baloncesto.
#Si el estudiante juega a baloncesto, lo meto en posiciones.
datos$Physical == "Basketball"
posiciones<-NULL
for(i in 1:length(datos$Physical)){
        if(datos$Physical[i]=="Basketball"){
                posiciones<-c(posiciones,i)
        }
}
print(posiciones)

#Una vez que sabemos cuales son los jugadores de baloncesto, seleccionamos 5 de forma aleatoria.

#la ! significa que se hace lo contrario (en este caso, "si no está en elegidos lo metes")

#La función election nos va a permitir incluir en el equipo a jugadores seleccionados de forma aleatoria siempre
#que el equipo no esté completo (sus componentes sean menos de 5). Además, si de forma aleatoria topa con un jugador
#ya incluido en el equipo, lo descartará y pasará a buscar otro.
election<-function(x){
        elegidos<-NULL
        while(length(elegidos)<5){
                elec<-as.integer(runif(1,1,length(x)))
                if(!(x[elec] %in% elegidos)){              
                        elegidos<-c(elegidos,x[elec])}}
        return(elegidos)
}

election(posiciones)

#VIA RAPIDA
equipaso<-sample(posiciones,5,replace=TRUE)
equipaso


######EJERCICIO 2.Ordenación y Regresión. PRACTICA 5

frame<-data.frame(datos$Height, datos$Arm.Span, datos$Foot.Size)

corHA<-cor(frame$datos.Height, frame$datos.Arm.Span,method = "pearson")
corHF<-cor(frame$datos.Height, frame$datos.Foot.Size,method = "pearson")
corAF<-cor(frame$datos.Arm.Span, datos$Foot.Size, method = "pearson")
corHA
corHF
corAF

#ordeamos los datos como pide el ejercicio, por estatura para identificar los alumnos más altos

frame.4<-data.frame(frame, datos$Province)
frame_ordenado<-frame.4[order(frame.4$datos.Height, decreasing = T),]  

#Indicamos los resultados por provincias en un diagrama de barras

plot(frame_ordenado$datos.Province,frame_ordenado$datos.Height,  main = "ALTURA POR PROVINCIA")

student_census<-table(datos$Province, datos$Height)

colours<-c("red", "blue", "yellow", "green", "orange", "black", "grey", "purple", "white", "pink", "brown")

barplot(student_census, main = "ALTURA", legend=rownames(student_census), col=colours)




##################EJERCICIO 3. MapReduce. Simulación. PRACTICA 5###############

#Primero hacemos el Map para conseguir una lista de listas que contiene 20 listas ordenadas.

seq <- seq(1:20)

v_clases <- c()
for(num in seq){
        a <- rep.int(num, times =  nrow(datos)/20) #rep.int replica los valores de num el numero de veces indicado.
        v_clases <- c(v_clases, a)
}
datos <- cbind(datos, v_clases) #añado el vector v_clases a datos

altura <- c()
orderedHeights <- c()
initClase <- 1 #el número de fila
lista <- list()

for(row in 1:nrow(datos)){
        if(datos$v_clases[row] == initClase){
                altClase <- datos$Height[row]
                altura <- c(altClase, altura) 
                if(row == nrow(datos)){
                        a1 <- sort(altura, decreasing = T, method = "quick")
                        a2 <- list(a1)
                        lista <- append(lista, a2)
                        orderedHeights <- c(a1, orderedHeights)   
                }
        }
        else{
                a1 <- sort(altura, decreasing = T, method = "quick")
                a2 <- list(a1)
                lista <- append(lista, a2)
                orderedHeights <- c(a1, orderedHeights)
                altura <- c()
                altClase <- datos$Height[row]
                altura <- c(altClase, altura)
                initClase <- initClase+1
        }
}
print(lista)

#Ahora hacemos el reduce con el objetivo de obtener un vector con las alturas ordenadas
#para ello creo una funcion (f_OrdenaTodos) que mete los elementos de una lista en el vector que queremos conseguir y los ordena
#antes de devolver el vector.

f_OrdenaTodos <- function(listaX){
        v <- c()
        i <- 1
        for(i in 1:length(listaX)){
                v1 <- unlist(listaX[[i]])
                v <- c(v1, v)
                i <- i+1
        }
        a <- sort(v, decreasing = T, method = 'quick')
        return(a)
}
f_OrdenaTodos(lista)


##########################EJERCICIO 4 PRACTICA 5######################

#https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
#https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html


library(dplyr)
library(magrittr)

#divido el dataset en 20 subconjuntos o partes
partes <- split(datos, rep(1:20, each=10))

#No he conseguido hacerlo con método Map-Reduce de forma estricta. Sin embargo, he logrado el mismo resultado creando un nuevo objeto
#"transicion" que contiene los elementos de todas las partes con su provincia y su fisico. En segundo lugar, he combinado "transicion" con un nuevo objeto
#"tabla" de forma que, agrupando por provincia y tipo de deporte, a partir de la suma de frecuencias obtenga el nº de estudiantes que 
#practican cada deporte por provincia. 

tabla <- data.frame()
for(i in 1:length(partes)){
        transicion <- with(partes[[i]], table(Province, Physical))
        tabla <- rbind(tabla, transicion) #combino
        if(i == length(partes)){
                tablafinal <- tabla %>% 
                        group_by(Province, Physical) %>% 
                        summarise(Jugadores = sum(Freq))
        }
}
View(transicion)
View(tablafinal)


###################EJERCICIO 5 PRACTICA 5 ################

# Primero se recogen los datos y el valor n del input
x<- c(1:80, 78:34, 1,2,3,4,5,6,7,5,4,3,2)
n<-5
x
# Segundo se divide el dataset en n datasets de tamaños aproximados. Para ello utilizaremos la función divideenBloques,
#que coge como input los datos (x) y el valor n del input (n). 
divideenBloques <- function( x, n ){
			tam<-length(x)%/%n
			list<-NULL
			
			for (i in 0:(n-2)){
					list<-c(list,c(i*tam+1, (i+1)*tam))
					
							}
			list<-c(list,c(((i+1)*tam)+1, length(x)))
			return(list)
}
lista<-divideenBloques(x,n)
lista
########################

#Tercero se calcula la suma de cada bloque y su tamaño

i<-1
pares_sumas<-function (lista, x, n){
 sumas<-NULL
 tams<-NULL
 for (i in 0:(n-1)){
		ini<-lista[i*2+1]
		fin<-lista[i*2+2]
		sumas<-c(sumas, sum(x[ini:fin]))
		tams<-c(tams,fin-ini+1)
		}
return(data.frame(sumas, tams))
}
ps<-pares_sumas(lista, x, n)
ps
# Cuarto se combinan los resultados anteriores para obtener la media del 
#dataset original.

reduce<-function(ps){
resultado<- as.double(sum(ps$sumas)/sum(ps$tams))
return(resultado)
}
resultado_media<-reduce(ps)
resultado_media
		
	





