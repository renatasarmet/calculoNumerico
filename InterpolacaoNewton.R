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