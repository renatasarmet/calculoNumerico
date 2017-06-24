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
  A <- matrix(1,n,n)
  for(j in 2:n){
    for (i in 1:n){
      A[i,j] <- X[i]^(j-1); 
    }
  }
  print(A)
  print(B)
  C <- eliminacao(A,B);
  
  return(C);
}

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
interpolacaoNewton <- function(X,Y,p){
    n <- length(X);
    
    #Criando matrix nxn
    D <- matrix(c(0),n,n);
    
    #Criando o vetor de tamanho n
    A <- rep(1,times=n);
    
    #Montando a tabela de diferenças divididas
    for(i in 1:n){
        D[i,1] <- Y[i];
    }
    
    for(j in 2:n){
        for(i in 1:(n-j+1)){
        #   cat("TEMOS AQUI O j = ", j, "\n");
            D[i,j] <- (D[i+1,j-1] - D[i,j-1])/(X[i+j-1]-X[i]);
        #   print(D[i,j]);
        #cat("(f[", i+1,"] - f[", i, "])/(X[",i+j-1,"] - X[",i,"])\n");
        }
    }
    #print(D);
    
    
    #Calculando o vetor A com (x-xk)
    A[i] <- 1;
    for(i in 2:n){
        A[i] <- A[i-1] * (p-X[i-1]);
    }
    
    #Calculando o polinomio
    r <- 0;
    for(i in 1:n){
        r <- r + A[i] * D[1,i];
    }
    
    return(r);   
}

automatizaInterpola<-function(p1,p2){
    MyData <- read.csv(file="dados.csv", header=FALSE , sep=",")
    x<-data.matrix(MyData[["V1"]], rownames.force = NA)
    y<-data.matrix(MyData[["V2"]], rownames.force = NA)
    n<-nrow(x)
    titulo<-NULL
    resultados<-NULL
    metodo<-matrix(c("Sistemas",p1,p2),3,1)
    #interpolacaoPolinomial
    for (i in 2:n ){
        titulo<-cbind(titulo,paste(i,"º grau",sep=""))
        a<-interpolacaoPolinomial(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1))
        m<-nrow(a)
        sum<-a[1]
        sum1<-a[1]
        for(j in 2:m){
            sum<-sum + a[j] * (p1 ^ (j-1))
            sum1<-sum1 + a[j] * (p2 ^ (j-1))
        }
        valores<-rbind(sum,sum1)
        resultados<-cbind(resultados,valores)
        
    }
    interpolacao<-rbind(titulo,resultados)
    data<-cbind(metodo,interpolacao)
    caminho<-paste(getwd(),"/planilha.csv",sep="")
    write.table(data,file =caminho  , sep=",", row.names=FALSE, na="",col.names=FALSE);
    
    
    
    

}









