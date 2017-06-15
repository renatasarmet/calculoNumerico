bissecao_convergencia <- function(f,a,b){
  if(f(a)*f(b)<0){
    return (1);
  }else{
    return (0);
  }
}


bissecao <- function(f,a,b,tol1,tol2,tol3){
  if(bissecao_convergencia(f,a,b)==1){
    old_x <- a+b; #confirmar se o maximo do tipo 3 é 1
    k <- 0;
    repeat{
      x <- (a+b)/2;
      if(f(a)*f(x)<0){
        b = x;
      }else{
        a = x;
      }
      cat("K = ", k, "\n");
      cat("x =", x, "\n");
      rtol1 <- abs(b-a);
      rtol2 <- abs(f(x));
      rtol3 <- abs((x-old_x)/abs(x));
      cat("Tol2 = ", rtol2, "\n");
      
      old_x <- x;
      k <- k + 1;
      if((rtol1<tol1)||(rtol2<tol2)||(rtol3<tol3))  break; #decidir se quer quais tipos
    }
    return (x);
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


newton <- function(f, df, ddf, x, tol1, tol2, tol3){
  if(!f(x)==0){
    rtol1 <- tol1 + 1;
    rtol2 <- tol2 + 1;
    rtol3 <- tol3 + 1;
    a <- ddf(x);
    k <- 0;
    #No while alterar entre 'e' e 'ou' dependendo do problema
    while((rtol1>tol1)&&(rtol2>tol2)&&(rtol3>tol3)){ 
      cat("K = ", k , "\n");
      cat("x = ", x, "\n");
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
      k <- k + 1;
    }
    cat("Fim K = ", k , "\n");
  }
  return (x);
}

