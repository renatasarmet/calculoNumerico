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

interpolacaoPolinomial <- function(X,B,chute){
  n <- nrow(X);
  A <- matrix(1,n,n)
  for(j in 2:n){
    for (i in 1:n){
      A[i,j] <- X[i]^(j-1); 
    }
  }
  C <- eliminacao(A,B);
  m<-nrow(C)
  sum<-C[1]
  for(j in 2:m){
   sum<-sum + C[j] * (chute ^ (j-1))
    }
  return(sum);
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
    tempoP1<-NULL
    tempoP2<-NULL
    
    #monta as tabelas para o metodo de sistemas lineares
    metodo<-matrix(c("Sistemas",p1,p2),3,1)
    #interpolacaoPolinomial
    for (i in 2:n ){
        titulo<-cbind(titulo,paste("Grau",i,sep=""))
        tempo<-proc.time()
        res<-interpolacaoPolinomial(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1),p1)
        tempo<-proc.time()-tempo
        tempo<-matrix(tempo,1,5)
        tempoP1<-rbind(tempoP1,tempo)
        tempo1<-proc.time()
        res1<-interpolacaoPolinomial(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1),p2)
        tempo1<-proc.time()-tempo1
        tempoP2<-rbind(tempoP2,tempo1)
        valores<-rbind(res,res1)
        resultados<-cbind(resultados,valores)
        
    }
 

    interpolacao<-rbind(titulo,resultados)
    dadoTempoP1<-matrix(tempoP1,n-1,5)
    
    titulo<-NULL
    dataProc<-NULL
    
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    titulo1<-cbind(titulo,paste("Processamento de x = ",p1,sep=""))
     titulo<-NULL
    for(i in 2:n){
        titulo<-NULL
        titulo<-cbind(titulo,paste("Processamento da função de grau ",i,sep=""))
        dataProc<-rbind(dataProc,matrix(c(titulo1,titulo,"","","",""),1,6))
        dataProc<-rbind(dataProc,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        dataProc<-rbind(dataProc,matrix(c("Time:",dadoTempoP1[i - 1,1],dadoTempoP1[i - 1,2],dadoTempoP1[i - 1,3],dadoTempoP1[i - 1,4],dadoTempoP1[i - 1,5]),1,6));
        dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    }
    titulo<-NULL
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    dadoTempoP2<-matrix(tempoP2,n-1,5)
    titulo1<-cbind(titulo,paste("Processamento de x = ",p2,sep=""))
     for(i in 2:n){
        titulo<-NULL
        titulo<-cbind(titulo,paste("Processamento da função de grau ",i,sep=""))
        dataProc<-rbind(dataProc,matrix(c(titulo1,titulo,"","","",""),1,6))
        dataProc<-rbind(dataProc,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        dataProc<-rbind(dataProc,matrix(c("Time:",dadoTempoP2[i - 1,1],dadoTempoP2[i - 1,2],dadoTempoP2[i - 1,3],dadoTempoP2[i - 1,4],dadoTempoP2[i - 1,5]),1,6));
        dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    }
    data<-cbind(metodo,interpolacao)
    m<-ncol(data)
    while(ncol(dataProc) != m)
        dataProc<-cbind(dataProc,matrix(c(""),4,1))
    data<-rbind(data,dataProc)
    
    
    
    #reset nas variaveis utilizadas para montar a tabela de um metodo
    titulo<-NULL
    resultados<-NULL
    tempoP1<-NULL
    tempoP2<-NULL
    tempo<-NULL
    tempo1<-NULL
    interpolacao<-NULL
    valores<-NULL
    resultados<-NULL
     #monta as tabelas para o metodo de lagrange
    metodo<-matrix(c("Lagrange",p1,p2),3,1)

    for (i in 2:n ){
        titulo<-cbind(titulo,paste("Grau",i,sep=""))
        tempo<-proc.time()
        res<-lagrange(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1),p1)
        tempo<-proc.time()-tempo
        tempo<-matrix(tempo,1,5)
        tempoP1<-rbind(tempoP1,tempo)
        tempo1<-proc.time()
        res1<-lagrange(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1),p2)
        tempo1<-proc.time()-tempo1
        tempoP2<-rbind(tempoP2,tempo1)
        valores<-rbind(res,res1)
        resultados<-cbind(resultados,valores)
        
    }
 

    interpolacao<-rbind(titulo,resultados)
    dadoTempoP1<-matrix(tempoP1,n-1,5)
    
    titulo<-NULL
    dataProc<-NULL
    
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    titulo1<-cbind(titulo,paste("Processamento de x = ",p1,sep=""))
     titulo<-NULL
    for(i in 2:n){
        titulo<-NULL
        titulo<-cbind(titulo,paste("Processamento da função de grau ",i,sep=""))
        dataProc<-rbind(dataProc,matrix(c(titulo1,titulo,"","","",""),1,6))
        dataProc<-rbind(dataProc,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        dataProc<-rbind(dataProc,matrix(c("Time:",dadoTempoP1[i - 1,1],dadoTempoP1[i - 1,2],dadoTempoP1[i - 1,3],dadoTempoP1[i - 1,4],dadoTempoP1[i - 1,5]),1,6));
        dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    }
    titulo<-NULL
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    dadoTempoP2<-matrix(tempoP2,n-1,5)
    titulo1<-cbind(titulo,paste("Processamento de x = ",p2,sep=""))
     for(i in 2:n){
        titulo<-NULL
        titulo<-cbind(titulo,paste("Processamento da função de grau ",i,sep=""))
        dataProc<-rbind(dataProc,matrix(c(titulo1,titulo,"","","",""),1,6))
        dataProc<-rbind(dataProc,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        dataProc<-rbind(dataProc,matrix(c("Time:",dadoTempoP2[i - 1,1],dadoTempoP2[i - 1,2],dadoTempoP2[i - 1,3],dadoTempoP2[i - 1,4],dadoTempoP2[i - 1,5]),1,6));
        dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    }
    data1<-cbind(metodo,interpolacao)
    m<-ncol(data1)
    while(ncol(dataProc) != m)
        dataProc<-cbind(dataProc,matrix(c(""),4,1))
    data1<-rbind(data1,dataProc)
    
    #reset nas variaveis utilizadas para montar a tabela de um metodo
    titulo<-NULL
    resultados<-NULL
    tempoP1<-NULL
    tempoP2<-NULL
    tempo<-NULL
    tempo1<-NULL
    interpolacao<-NULL
    valores<-NULL
    resultados<-NULL
    
     #monta as tabelas para o metodo de newton
    metodo<-matrix(c("Newton",p1,p2),3,1)

    for (i in 2:n ){
        titulo<-cbind(titulo,paste("Grau",i,sep=""))
        tempo<-proc.time()
        res<-interpolacaoNewton(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1),p1)
        tempo<-proc.time()-tempo
        tempo<-matrix(tempo,1,5)
        tempoP1<-rbind(tempoP1,tempo)
        tempo1<-proc.time()
        res1<-interpolacaoNewton(matrix(x[1:i,1],i,1),matrix(y[1:i,1],i,1),p2)
        tempo1<-proc.time()-tempo1
        tempoP2<-rbind(tempoP2,tempo1)
        valores<-rbind(res,res1)
        resultados<-cbind(resultados,valores)
        
    }
 

    interpolacao<-rbind(titulo,resultados)
    dadoTempoP1<-matrix(tempoP1,n-1,5)
    
    titulo<-NULL
    dataProc<-NULL
    
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    titulo1<-cbind(titulo,paste("Processamento de x = ",p1,sep=""))
     titulo<-NULL
    for(i in 2:n){
        titulo<-NULL
        titulo<-cbind(titulo,paste("Processamento da função de grau ",i,sep=""))
        dataProc<-rbind(dataProc,matrix(c(titulo1,titulo,"","","",""),1,6))
        dataProc<-rbind(dataProc,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        dataProc<-rbind(dataProc,matrix(c("Time:",dadoTempoP1[i - 1,1],dadoTempoP1[i - 1,2],dadoTempoP1[i - 1,3],dadoTempoP1[i - 1,4],dadoTempoP1[i - 1,5]),1,6));
        dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    }
    titulo<-NULL
    dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    dadoTempoP2<-matrix(tempoP2,n-1,5)
    titulo1<-cbind(titulo,paste("Processamento de x = ",p2,sep=""))
     for(i in 2:n){
        titulo<-NULL
        titulo<-cbind(titulo,paste("Processamento da função de grau ",i,sep=""))
        dataProc<-rbind(dataProc,matrix(c(titulo1,titulo,"","","",""),1,6))
        dataProc<-rbind(dataProc,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        dataProc<-rbind(dataProc,matrix(c("Time:",dadoTempoP2[i - 1,1],dadoTempoP2[i - 1,2],dadoTempoP2[i - 1,3],dadoTempoP2[i - 1,4],dadoTempoP2[i - 1,5]),1,6));
        dataProc<-rbind(dataProc,matrix(c("","","","","",""),1,6));
    }
    data2<-cbind(metodo,interpolacao)
    m<-ncol(data2)
    while(ncol(dataProc) != m)
        dataProc<-cbind(dataProc,matrix(c(""),4,1))
    data2<-rbind(data2,dataProc)
    
    n<-max(nrow(data),nrow(data1),nrow(data2));
        
    while(nrow(data) != n)
        data<-rbind(data,matrix(c("","","","","",""),1,6));
        
    while(nrow(data1) != n)
        data1<-rbind(data1,matrix(c("","","","","",""),1,6));

    while(nrow(data2) != n)
        data2<-rbind(data2,matrix(c("","","","","",""),1,6));
    
    planilha<-cbind(data,matrix("",n,1));
    planilha<-cbind(planilha,matrix("",n,1));
    planilha<-cbind(planilha,data1);
    planilha<-cbind(planilha,matrix("",n,1));
    planilha<-cbind(planilha,matrix("",n,1));
    planilha<-cbind(planilha,data2);
    caminho<-paste(getwd(),"/planilha.csv",sep="")
    write.table(planilha,file =caminho  , sep=",", row.names=FALSE, na="",col.names=FALSE);
    
    
    
    

}









