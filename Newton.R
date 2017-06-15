bissecaoParaNewton <- function(a,b){
  x <- (a+b)/2;
  cat(x);
  return(x);
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
    #while((rtol1>tol1)&&(rtol2>tol2)&&(rtol3>tol3)){ 
    while(rtol2>tol2){ 
      cat("K = ", k , "\n");
      cat("x = ", x, "\n");
      old_x <- x;
      
      if(df(old_x)==0){
        cat("\n Derivada em ", old_x, "igual a 0!\n");
        return(NULL);
      }
      
      x <- old_x - f(old_x)/df(old_x);
      
      if(!testeDDF(ddf,x,a)){
        cat("\nMudou o sentido da concavidade!\n");
        return(NULL);
      }
      
      rtol1 <- abs(x - old_x);
      rtol2 <- abs(f(x));
      rtol3 <- abs(x-old_x)/abs(x);
      cat("Tol1 = ", rtol1, "\n");
      cat("Tol2 = ", rtol2, "\n");
      cat("Tol3 = ", rtol3, "\n");
      k <- k + 1;
    }
    k <- k-1;
    cat("Fim K = ", k , "\n");
  }
  return (x);
}


cat("Programa que calcula sistemas lineares \n");

time1 <- proc.time();

f <- function(x) { 8-4.5*(x-sin(x))}
df <- function(x) { -4.5 *(1-cos(x))}
ddf <- function(x) { -4.5*sin(x)}
x0 <- 15;
tol1 <- 10^-(2);
tol2 <- 10^-(2);
tol3 <- 10^-(2);

solucao <- newton(f,df,ddf,x0,tol1,tol2,tol3);
cat("Ultima aproximacao: ", solucao,"\n");

print(proc.time() - time1)