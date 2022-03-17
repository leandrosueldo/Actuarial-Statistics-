
## MÉTODO CONGRUENCIAL MULTIPLICATIVO

n <- 10
x <- rep(0,n)
x[1] = 5
a = 3
m = 150

for(i in 1:n){
  ## MÓDULO: RESTO DE DIVIDIR UN NÚMERO POR OTRO
  x[i + 1] <- (a * x[i]) %% m  
}

print(x)

## GENERADOR CONGRUENCIAL MIXTO (ADITIVO Y MULTIPLICATIVO)

n <- 10
y <- rep(0,n)
y[1] = 3
a = 5
m = 200
c = 7

for(i in 1:n){
  y[i + 1] <- (a * y[i] + c) %% m
}

print(y)

## 

n = 10000
z = rep(0,n)
for(i in 1:n){
  z[i] <- runif(1)
}
# print(z)
# lessR::Histogram(z,breaks = 30)
tseries::runs.test(as.factor(z>median(z)))
## NO HAY EVIDENCIAS PARA CONSIDERAR LOS DATOS NO ALEATORIOS

X = rep(0,100000)
for(i in 1:length(X)){
  U = runif(1)
  if(U < 0.40){
    X[i] = 4
  } else if(U < 0.65){
    X[i] = 3
  } else if(U < 0.85){
    X[i] = 1
  } else {
    X[i] = 2
  }
}

## CON ALGORITMO RECURSIVO
rate = 1;X = rep(0,10)
for(i in 1:10){
  U = runif(1);X[i] = -1/rate * log(U)
}
X

rexp(10,rate = 1)



set.seed(10)
rnorm(100)

set.seed(15)
rnorm(100)

set.seed(11)
n = 10
X = rep(0,n)
for(i in 1:n){
  U = runif(1)
  if(U < 0.20){
    X[i] = 1
  } else if(U < 0.35){
    X[i] = 2
  } else if(U < 0.60){
    X[i] = 3
  } else {
    X[i] = 4
  }
}

print(X)

hist(X)


X = rep(0,10000) ## se crea un vector númerico relleno de 0
for(i in 1:length(X)){
  U = runif(1) # no se fija la semilla
  if(U < 0.40){
    X[i] = 4
  } else if(U < 0.65){
    X[i] = 3
  } else if(U < 0.85){
    X[i] = 1
  } else {
    X[i] = 2
  }
}
print(X)

set.seed(200)
X = rep(0,10)
for(i in 1:length(X)){
  U = runif(1)
  if(U < 0.82){
    X[i] = 300
  } else if(U < 0.91){
    X[i] = 250
  } else if(U < 0.97){
    X[i] = 150
  } else {
    X[i] = 50
  }
}

print(x)


rgeom(1,0.30)

p = 0.30
q = 1 - p
U = runif(1)
log(U)
log(q)

X = round(log(U)/log(q),0) + 1
print(X)


log(0.30)


inic <- Sys.time()
rbinom(10,size = 30,prob = 0.10)
fin <- Sys.time()
fin - inic

#EJERCICIO PARCIAL  RECU 2021 2)
set.seed(200)
X = rep(0,10)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.136875){
      X[i] = 100
    } else if(U < 0.238125){
      X[i] = 150
    } else if(U < 0.33){
      X[i] = 200
    } else if(U < 0.40875){
      X[i] = 180
    } else if(U < 0.48){
      X[i] = 50
    } else if(U < 0.5475){
      X[i] = 80
    } else if(U < 0.60375){
      X[i] = 130
    } else if(U < 0.649375){
      X[i] = 300
    } else if(U < 0.694375){
      X[i] = 0
    } else if(U < 0.736875){
      X[i] = 250
    } else if(U < 0.770625){
      X[i] = 350
    } else if(U < 0.796875){
      X[i] = 380
    } else if(U < 0.823125){
      X[i] = 120
    } else if(U < 0.84875){
      X[i] = 400
    } else if(U < 0.87125){
      X[i] = 20
    } else if(U < 0.89375){
      X[i] = 230
    } else if(U < 0.91625){
      X[i] = 280
    } else if(U < 0.935){
      X[i] = 70
    } else if(U < 0.95375){
      X[i] = 330
    } else if(U < 0.9625){
      X[i] = 320
    } else if(U < 0.97){
      X[i] = 170
    } else if(U < 0.9775){
      X[i] = 220
    } else if(U < 0.985){
      X[i] = 430
    } else if(U < 0.99125){
      X[i] = 270
    } else if(U < 0.9975){
      X[i] = 450
    } else {
      X[i] = 370
    }
}

print(X)

#3)

#Para 100 iteraciones
set.seed(200)
X = rep(0,100)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.136875){
    X[i] = 100
  } else if(U < 0.238125){
    X[i] = 150
  } else if(U < 0.33){
    X[i] = 200
  } else if(U < 0.40875){
    X[i] = 180
  } else if(U < 0.48){
    X[i] = 50
  } else if(U < 0.5475){
    X[i] = 80
  } else if(U < 0.60375){
    X[i] = 130
  } else if(U < 0.649375){
    X[i] = 300
  } else if(U < 0.694375){
    X[i] = 0
  } else if(U < 0.736875){
    X[i] = 250
  } else if(U < 0.770625){
    X[i] = 350
  } else if(U < 0.796875){
    X[i] = 380
  } else if(U < 0.823125){
    X[i] = 120
  } else if(U < 0.84875){
    X[i] = 400
  } else if(U < 0.87125){
    X[i] = 20
  } else if(U < 0.89375){
    X[i] = 230
  } else if(U < 0.91625){
    X[i] = 280
  } else if(U < 0.935){
    X[i] = 70
  } else if(U < 0.95375){
    X[i] = 330
  } else if(U < 0.9625){
    X[i] = 320
  } else if(U < 0.97){
    X[i] = 170
  } else if(U < 0.9775){
    X[i] = 220
  } else if(U < 0.985){
    X[i] = 430
  } else if(U < 0.99125){
    X[i] = 270
  } else if(U < 0.9975){
    X[i] = 450
  } else {
    X[i] = 370
  }
}

mean(X)
sd(X)
quantile(X,0.95)

##Para 1000 iteraciones
set.seed(200)
X = rep(0,1000)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.136875){
    X[i] = 100
  } else if(U < 0.238125){
    X[i] = 150
  } else if(U < 0.33){
    X[i] = 200
  } else if(U < 0.40875){
    X[i] = 180
  } else if(U < 0.48){
    X[i] = 50
  } else if(U < 0.5475){
    X[i] = 80
  } else if(U < 0.60375){
    X[i] = 130
  } else if(U < 0.649375){
    X[i] = 300
  } else if(U < 0.694375){
    X[i] = 0
  } else if(U < 0.736875){
    X[i] = 250
  } else if(U < 0.770625){
    X[i] = 350
  } else if(U < 0.796875){
    X[i] = 380
  } else if(U < 0.823125){
    X[i] = 120
  } else if(U < 0.84875){
    X[i] = 400
  } else if(U < 0.87125){
    X[i] = 20
  } else if(U < 0.89375){
    X[i] = 230
  } else if(U < 0.91625){
    X[i] = 280
  } else if(U < 0.935){
    X[i] = 70
  } else if(U < 0.95375){
    X[i] = 330
  } else if(U < 0.9625){
    X[i] = 320
  } else if(U < 0.97){
    X[i] = 170
  } else if(U < 0.9775){
    X[i] = 220
  } else if(U < 0.985){
    X[i] = 430
  } else if(U < 0.99125){
    X[i] = 270
  } else if(U < 0.9975){
    X[i] = 450
  } else {
    X[i] = 370
  }
}

mean(X)
sd(X)
quantile(X,0.95)

##Para 10000 iteraciones
set.seed(200)
X = rep(0,10000)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.136875){
    X[i] = 100
  } else if(U < 0.238125){
    X[i] = 150
  } else if(U < 0.33){
    X[i] = 200
  } else if(U < 0.40875){
    X[i] = 180
  } else if(U < 0.48){
    X[i] = 50
  } else if(U < 0.5475){
    X[i] = 80
  } else if(U < 0.60375){
    X[i] = 130
  } else if(U < 0.649375){
    X[i] = 300
  } else if(U < 0.694375){
    X[i] = 0
  } else if(U < 0.736875){
    X[i] = 250
  } else if(U < 0.770625){
    X[i] = 350
  } else if(U < 0.796875){
    X[i] = 380
  } else if(U < 0.823125){
    X[i] = 120
  } else if(U < 0.84875){
    X[i] = 400
  } else if(U < 0.87125){
    X[i] = 20
  } else if(U < 0.89375){
    X[i] = 230
  } else if(U < 0.91625){
    X[i] = 280
  } else if(U < 0.935){
    X[i] = 70
  } else if(U < 0.95375){
    X[i] = 330
  } else if(U < 0.9625){
    X[i] = 320
  } else if(U < 0.97){
    X[i] = 170
  } else if(U < 0.9775){
    X[i] = 220
  } else if(U < 0.985){
    X[i] = 430
  } else if(U < 0.99125){
    X[i] = 270
  } else if(U < 0.9975){
    X[i] = 450
  } else {
    X[i] = 370
  }
}

mean(X)
sd(X)
quantile(X,0.95)



##Parcial 2021 
#2)

set.seed(200)
X = rep(0,10)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.64){
    X[i] = 0
  } else if(U < 0.90276){
    X[i] = 300
  } else if(U < 0.93156){
    X[i] = 250
  } else if(U < 0.958456){
    X[i] = 600
  } else if(U < 0.977656){
    X[i] = 150
  } else if(U < 0.987256){
    X[i] = 50
  } else if(U < 0.99316){
    X[i] = 550
  } else if(U < 0.997096){
    X[i] = 450
  } else if(U < 0.999064){
    X[i] = 350
  } else if(U < 0.999496){
    X[i] = 400
  } else if(U < 0.99982){
    X[i] = 500
  } else if(U < 0.999964){
    X[i] = 200
  } else {
    X[i] = 100
  }
}

print(X)
 
#probabilidad frecuencialista

set.seed(200)
Y = rep(0,10)
for(i in 1:length(Y)){
  if(X[i]<10){
    Y[i] = 0
  } else {
  Y[i]= 1
  }
}

print(Y)

Probabilidad<- sum(Y)/length(Y)
print(Probabilidad)


##3)

#Para 100 iteraciones
set.seed(200)
X = rep(0,100)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.64){
    X[i] = 0
  } else if(U < 0.90276){
    X[i] = 300
  } else if(U < 0.93156){
    X[i] = 250
  } else if(U < 0.958456){
    X[i] = 600
  } else if(U < 0.977656){
    X[i] = 150
  } else if(U < 0.987256){
    X[i] = 50
  } else if(U < 0.99316){
    X[i] = 550
  } else if(U < 0.997096){
    X[i] = 450
  } else if(U < 0.999064){
    X[i] = 350
  } else if(U < 0.999496){
    X[i] = 400
  } else if(U < 0.99982){
    X[i] = 500
  } else if(U < 0.999964){
    X[i] = 200
  } else {
    X[i] = 100
  }
}

mean(X)
sd(X)

#Para 1000 iteraciones
set.seed(200)
X = rep(0,1000)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.64){
    X[i] = 0
  } else if(U < 0.90276){
    X[i] = 300
  } else if(U < 0.93156){
    X[i] = 250
  } else if(U < 0.958456){
    X[i] = 600
  } else if(U < 0.977656){
    X[i] = 150
  } else if(U < 0.987256){
    X[i] = 50
  } else if(U < 0.99316){
    X[i] = 550
  } else if(U < 0.997096){
    X[i] = 450
  } else if(U < 0.999064){
    X[i] = 350
  } else if(U < 0.999496){
    X[i] = 400
  } else if(U < 0.99982){
    X[i] = 500
  } else if(U < 0.999964){
    X[i] = 200
  } else {
    X[i] = 100
  }
}

mean(X)
sd(X)


#Para 10000 iteraciones
set.seed(200)
X = rep(0,10000)
for(i in 1:length(X)){
  U = runif(1)
  if(U<0.64){
    X[i] = 0
  } else if(U < 0.90276){
    X[i] = 300
  } else if(U < 0.93156){
    X[i] = 250
  } else if(U < 0.958456){
    X[i] = 600
  } else if(U < 0.977656){
    X[i] = 150
  } else if(U < 0.987256){
    X[i] = 50
  } else if(U < 0.99316){
    X[i] = 550
  } else if(U < 0.997096){
    X[i] = 450
  } else if(U < 0.999064){
    X[i] = 350
  } else if(U < 0.999496){
    X[i] = 400
  } else if(U < 0.99982){
    X[i] = 500
  } else if(U < 0.999964){
    X[i] = 200
  } else {
    X[i] = 100
  }
}

mean(X)
sd(X)







