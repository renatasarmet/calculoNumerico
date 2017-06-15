tcl <- function(A){
  n <- ncol(A);
  maior = 0;
  
  for(i in 1:n){
    result[i] <- 0;
    for(j in 1:n){
      if(i!=j){
        result[i] <- result[i] + abs(A[i,j]);
      }
    }
    result[i] <- result[i]/abs(A[i,i]);
    if(result[i]>maior){
      maior = result[i];
    }
  }
  
  
  if(maior<1){
    return (1);
  }else{
    return (0);
  }
}


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

interpolacaoPolinomial <- function(X,B){
  n <- nrow(X);
  A <- matrix(c(1),n,n)
  for(j in 2:n){
    for (i in 1:n){
      A[i,j] <- X[i]^(j-1); 
    }
  }
  C <- eliminacao(A,B);
  
  return(C);
}











