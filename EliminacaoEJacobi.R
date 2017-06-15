valida_matriz <- function(A){
  n <- ncol(A);
  if(ncol(A)!=nrow(A)){
    return (0);
  }
  
  for(i in 1:n){
    if(A[i,i]==0){
      return (0);
    }
  }
  
  if(det(A)==0){
    return (0);
  }
  
  return (1);
}


tcl <- function(A){
  n <- ncol(A);
  maior = 0;
  
  result <- c(0,n,1);
  
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
  if(!valida_matriz(A)){
    return(NULL);
  }
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


jacobi <- function(A,B,erro,chute){
  if(!tcl(A)){
    return(NULL);
  }
  n<-nrow(A);
  k <- 1;
  d <- erro+1;
  old_xk <- matrix(chute,n,1);
  xk <- matrix(0,n,1);
  while(d>erro){
    for(i in 1:n){
      xk<-old_xk;
      soma <- 0;
      for(j in 1:n){
        if(i!=j){
          cat("k: ", k, " e i: ", i, " e j: ", j, "\n");
          soma <- soma + A[i,j]*xk[j,k];
        }
      }
      xk[i,k] <- (1/A[i,i])*(b[i]-soma);
      if(k==1){
         old_xk <- cbind(chute,xk);
      }else{
       old_xk <- cbind(old_xk, xk[,k]);
      }
      if(k>1){
        max1 <- max(abs(xk[,k]-xk[,k-1]));
        max2 <- max(abs(xk[,k]));
        d <- max1/max2;
      }
      k <- k+1;
    }
    cat("Historico de aproximacoes(iniciando em k=1");
    print(old_xk);
    cat("Erro relativo (d) na instancia k=", k, "é:", d, "\n");
    cat("Lembre-se de que nos livros o algoritmo se inicia em k=0 e em R k=1\n");
    
  }
  
}