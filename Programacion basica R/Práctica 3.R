################################


#Saber si un número es par

par<-function(n){
        
        if(n%%2==0) {sol<-"el numero es par"}
        else{sol<-"el numero es impar"}
        return(sol)
}
par2<-function(n){
        if (n%%2==0){
                sol<-1}
        else {sol<-0}
        return(sol)
}


contarpares<-function(v){
        contador<-0
        for(i in (1:length(v))){
                if(par2(v[i])==1) {contador<-contador+1}}
        return(contador)
        }
}


#####################################

factorial<-function(n){
        if (n<0) {f<-"el numero debe ser positivo o cero"}
        else {
                f<-1
                while (n>0)
                {
                        f<-f*n
                        n<-n-1
                }}
        return(f)}

factorial(5)
factorial(0)
factorial(-5)

###############################

#####Construir una agenda de teléfonos##############
nueva.agenda<-function(){
        ###esta función crea una agenda 
        ###que almacenará en un dataframe con dos campos: nombres, telefonos
        nombres<-"Victoria"
        telefonos<-"234432234"
        continuar<-"S";
        while (continuar =="S") {
                nombre<-readline(prompt="Dime el nombre: ")
                telefono<-readline(prompt="Dime el teléfono: ")
                nombres<-c(nombres,nombre)
                telefonos<-c(telefonos,telefono)
                continuar <- readline(prompt="¿Continuar(S/N)? ")
        }
        mdf<-data.frame(nombres, telefonos)
        mdf$nombres<-as.character(mdf$nombres)
        mdf$telefonos<-as.character(mdf$telefonos)
        return (mdf)
}
miagenda<-nueva.agenda()
#####Construir una agenda de teléfonos##############
add.agenda<-function(agenda.ant){
        ###esta función añade nombres y teléfonos a una agenda 
        ###que está almacenada en un dataframe con dos campos: nombres, telefonos
        nombres<-agenda.ant$nombres
        telefonos<-agenda.ant$telefonos
        continuar<-"S";
        while (continuar =="S") {
                nombre<-readline(prompt="Dime el nombre: ")
                telefono<-readline(prompt="Dime el teléfono: ")
                nombres<-c(nombres,nombre)
                telefonos<-c(telefonos,telefono)
                continuar <- readline(prompt="¿Continuar(S/N)? ")
        }
        return (data.frame(nombres, telefonos))
}

miagenda2<-add.agenda(miagenda)

#################################################

#Construir un vector que contenga los n números pares siguientes a m (par)
m<-30
n<-7
v<-m+2
m<-m+2
while (n>1)
{
        m<-m+2
        v<-c(v,m)
        n<-n-1
}
print(v) # usar print si se quiere ejecutar como un script desde consola 

#tenemos q crear un vector con los numeros 30,32,34,36,38,40,42,44
#a traves de la construccion de una funcion que sea construye<-function(m,n), siendo m,n el imput



####### EJercicio: #Construir un vector que contenga los n números pares siguientes a m (par) 

construye.pares<- function(n,m){
        
        if (m%%2==1){v<-"error"}
               
        else {
                v<-m+2
                m<-m+2
        }
        while (n>1){
                m<-m+2
                v<-c(v,m)
                n<-n-1
        }
        return(v)}  





####### EJercicio: modificar codigo para q sea construye pares pero metiendo numeros impares o pares indistintamente

construye.pares<- function(n,m){
        
        if (m%%2==1){
                v<-m+1
                m<-m+1
        }
        else {
                v<-m+2
                m<-m+2
        }
        while (n>1){
                m<-m+2
                v<-c(v,m)
                n<-n-1
        }
        return(v)}  



#Practica 3 ejercicio 3

#explicación pizarra en el word explicacion practica 3

#A)

v=c(6,3,15,0,9,1,2)

maximo<-function(v){
max<-v[1]
N<-length(v)
for(i in 2:N){
        if(v[i]>max){max<-v[i]}}
return(max)
}
maximo(v)


#B) #explicacion en el word explicacion pagina 3

v<-c(6,5,0,3,5,3,0,5,0,0,1,6,5,5,0)

x<-5

frecuencia<-function(v,x){
        Frec<-0
        N<-length(v)
        for(i in 1:N){
                if (v[i]==x)  (Frec<-Frec+1)}
        return(Frec)
}
#Para x=5 por ejemplo
frecuencia(v,5)

msg<-sprintf("la frecuencia de %.i en el vector v es %.i",x,frecuencia(v,x))
print(msg)


#C) Encontrar la frecuencia del maximo de un vector
#tengo un vector v
m<-maximo(v)
f<-frecuencia(v,m)


v<-c(6,5,0,3,5,3,0,5,0,0,1,6,5,5,0)


m<-maximo(v)
frecuencia(v,m)


#otra forma:
freqmax <-function(v){
        max<-v[1]
        N<-length(v)
        for(i in 2:N){
                if(v[i]>max){max<-v[i]}}
        fq<- frecuencia(v,max)
        z<-sprintf( "El número máximo del vector es el %d, y aparece con una frecuencia de %d",max,fq)
        return(z)
        
        
}