bissecao_convergencia <- function(f,a,b){
  if(f(a)*f(b)<0){
    cat("Converge \n");
    return (1);
  }else{
    cat("Nao converge \n");
    return (0);
  }
}


bissecao <- function(f,a,b,tol1,tol2,tol3,data){
  if(bissecao_convergencia(f,a,b)==1){
    old_x <- a+b; 
    k <- 0;
    repeat{
      x <- (a+b)/2;
      if(f(a)*f(x)<0){
        b = x;
      }else{
        a = x;
      }
      #cat("K = ", k, "\n");
      #cat("x =", x, "\n");
      rtol1 <- abs(b-a);
      rtol2 <- abs(f(x));
      rtol3 <- abs((x-old_x)/abs(x));
      #cat("Tol2 = ", rtol2, "\n");
      data<-rbind(data,c(x,rtol1,rtol2,rtol3));
      old_x <- x;
      k <- k + 1;
      if(rtol2<tol2)  break; #decidir se quer quais tipos
    }
    return (data);
  }
  return(NULL);
}


testeDDF <- function(ddf,x,a){
  b <- ddf(x);
  if(a*b < 0){
    return(0);
  }
  return(1);
}


newton <- function(f, df, ddf, x, tol1, tol2, tol3,data){
  if(!f(x)==0){
    rtol1 <- tol1 + 1;
    rtol2 <- tol2 + 1;
    rtol3 <- tol3 + 1;
    a <- ddf(x);
    k <- 0;
    #No while alterar entre 'e' e 'ou' dependendo do problema
    while(rtol2>tol2){ 
      #cat("K = ", k , "\n");
      #cat("x = ", x, "\n");
      old_x <- x;
      
      if(df(old_x)==0){
        cat("\n Derivada em ", old_x, "igual a 0!");
        return(NULL);
      }
      
      x <- old_x - f(old_x)/df(old_x);
      
      if(!testeDDF(ddf,x,a)){
        cat("\nMudou o sentido da concavidade!");
        return(NULL);
      }
      
      rtol1 <- abs(x - old_x);
      rtol2 <- abs(f(x));
      rtol3 <- abs(x-old_x)/abs(x);
      data<-rbind(data,c(x,rtol1,rtol2,rtol3));
      k <- k + 1;
    }
    #cat("Fim K = ", k , "\n");
  }
  return (data);
}

secante<-function(f,x0,x1,tol1,tol2,tol3,data){
    repeat{
        x2<-( x0 * f(x1) - x1 * f(x0) ) / ( f(x1) - f(x0) );
        rtol1<-abs(x2 - x1);
        rtol2<-abs(f(x1));
        rtol3<-abs((x2 - x1)/x2);
        data<-rbind(data,c(x2,rtol1,rtol2,rtol3));
        if(rtol2<tol2){
            #cat(x2)
            break
        }else{
            x0<-x1;
            x1<-x2;
        }
    }

    return(data)
}


#MyData <- read.csv(file="c:/TheDataIWantToReadIn.csv", header=TRUE, sep=",")
#
automatiza<-function(f,df,ddf,a,b,x0,x1,tol1B,tol2B,tol3B,tol1N,tol2N,tol3N,tol1S,tol2S,tol3S){
        data<-matrix(c("","Metodo","bissecao",""),1,4)
        data<-rbind(data,matrix(c("X","Tolerancia 1","Tolerancia 2","Tolerancia 3"),1,4));
        timeBissecao<-proc.time();
        data<-bissecao(f,a,b,tol1B,tol2B,tol3B,data);
        timeBissecao<- proc.time() - timeBissecao;
        data<-rbind(data,matrix(c("","","",""),1,4));
        n<-nrow(data);
        data<-cbind(data,matrix("",n,1));
        data<-cbind(data,matrix("",n,1));
        tempos<-matrix(timeBissecao,5,1);
        data<-rbind(data,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        data<-rbind(data,matrix(c("Time:",tempos[1],tempos[2],tempos[3],tempos[4],tempos[5]),1,6));

        
        data1<-matrix(c("","Metodo","newton",""),1,4)
        data1<-rbind(data1,matrix(c("X","Tolerancia 1","Tolerancia 2","Tolerancia 3"),1,4));
        timeNewton<-proc.time();
        data1<-newton(f, df, ddf, x0 ,tol1N, tol2N, tol3N,data1);
        timeNewton<- proc.time() - timeNewton;
        data1<-rbind(data1,matrix(c("","","",""),1,4));
        n<-nrow(data1);
        data1<-cbind(data1,matrix("",n,1));
        data1<-cbind(data1,matrix("",n,1));
        tempos<-matrix(timeNewton,5,1);
        data1<-rbind(data1,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        data1<-rbind(data1,matrix(c("Time:",tempos[1],tempos[2],tempos[3],tempos[4],tempos[5]),1,6));

        
        
        data2<-matrix(c("","Metodo","secante",""),1,4)
        data2<-rbind(data2,matrix(c("X","Tolerancia 1","Tolerancia 2","Tolerancia 3"),1,4));
        timeSecante<-proc.time();
        data2<-secante(f, x0, x1 ,tol1S, tol2S, tol3S,data2);
        timeSecante<- proc.time() - timeSecante;
        data2<-rbind(data2,matrix(c("","","",""),1,4));
        n<-nrow(data2);
        data2<-cbind(data2,matrix("",n,1));
        data2<-cbind(data2,matrix("",n,1));
        tempos<-matrix(timeSecante,5,1);
        data2<-rbind(data2,matrix(c("","User","System","Elapsed","Cumulative","Spawned"),1,6));
        data2<-rbind(data2,matrix(c("Time:",tempos[1],tempos[2],tempos[3],tempos[4],tempos[5]),1,6));

        
        
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
        print(caminho)
        
        
}

print("Software que calcula o processamento de cada método numérico para zero de função (Bissecao/Newton/Secante) e faz a tabulação.");
print("Autores:");
print("João Gabriel Barbirato");
print("Leonardo de Oliveira Peralta");
print("Renata Sarmet Smiderle Mendes");
print("");
print("Metodo de automatização:");
print("automatiza(funcao, primeira derivada,segunda derivada, valor de a, valor de b, primeiro chute, segundo chute, 1 tolerancia bissecao, 2 tolerancia bissecao, 3 tolerancia bissecao, 1 tolerancia Newton, 2 tolerancia Newton, 3 tolerancia Newton,1 tolerancia Secante, 2 tolerancia Secante, 3 tolerancia Secante)");
print("");
