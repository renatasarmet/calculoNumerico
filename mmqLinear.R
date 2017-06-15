eliminacao <- function(A,B){
  n <- nrow(A);
  for(k in 1:(n-1)){
    for(i in (k+1):n){
      m <- A[i,k]/A[k,k];
      A[i,k] <- 0;
      for(j in (k+1):n){
        A[i,j] <- A[i,j] - m*A[k,j];
      }
      B[i] <- B[i] - m*B[k];
      
    }
  }
  
  X <- matrix(0,n,1);
  
  X[n] <- B[n]/A[n,n];
  for(k in (n-1):1){
    s <- 0;
    for(j  in (k+1):n){
      s <- s + A[k,j]*X[j];
      X[k] <- (B[k] - s)/ A[k,k];
    }
  }
  return (X);
}

mmq <- function(X, Y, n){
  a1 = 0;
  a2 = 0;
  b1 = 0;
  b2 = 0;
  for(i in 1:n){
    a1 <- a1 + X[i] * X[i]; 
    a2 <- a2 + X[i];
    b1 <- b1 + Y[i]*X[i];
    b2 <- b2 + Y[i];
  }
  A <- matrix(c(a1,a2,a2,n),2,2);
  B <- matrix(c(b1,b2));
  
  R <- eliminacao(A,B);
  return (R);
}