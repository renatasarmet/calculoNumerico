lagrange <- function(X,Y,p){
  n <- length(X);
  
  #Criando vetor L de tamanho n
  L <- rep(1,times=n);
  
  #Calculando cada L
  for(j in 1:n){
    for(i in 1:n){
      if(i!=j){
        L[j] <- L[j]*(p-X[i]);
      }
    }
    for(i in 1:n){
      if(i!=j){
        L[j] <- L[j]/(X[j]-X[i]); 
      }
    }
  }
  
  #Calculando o polinomio
  A <- 0;
  for(j in 1:n){
    A <- A + Y[j]*L[j];
  }
  
  return (A);
 
}