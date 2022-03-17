#Caso ARMA(1,0)

predicción=function(phi0,phi1,Yt,l){
  predictor=c()
  predictor[1]=Yt
  for (i in 2:(l+1)) {
    predictor[i]=phi0+phi1*predictor[i-1]
  }
  a=predictor[2:(l+1)]
  varianza=c()
  varianza[1]=1
  b=c()
  b[1]=1
  for (i in 2:l) {
  varianza[2:i]=varianza[2:i]*phi1
  varianza[i]=phi1
  b[i]=sum(varianza^2)
  }
  M=cbind(a,b)
  colnames(M)=c("Predictores","Varianza")
  print(M)
}

predicción(35.0685,0.7147,38,5)


intervalos=function(phi0,phi1,Yt,l,Z){
  M=predicción(phi0,phi1,Yt,l)
  Int=matrix(0,ncol = 2,nrow = l)
  for (i in 1:l) {
    Int[i,1]=M[i,1]-Z*sqrt(M[i,2])
    Int[i,2]=M[i,1]+Z*sqrt(M[i,2])
  }
  colnames(Int)=c("Límite inferior","Límite superior")
  return(Int)
}
intervalos(35.0685,0.7147,38,5,1.96)

options(digits = 15)
