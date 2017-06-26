source("automatizacaoZeroFuncao.R");

f <- function(x) { x^50 - (10*x^30)/9 + 5/21 };
df <- function(x) { 50*x^49 - (100*x^29)/3};
ddf <- function(x) { 2450*x^48 - (2900*x^28)/3 };

#REESCREVER TUDO COM A FUNCAO NOVA

##Intervalo nao converge pois f(a) * f(b) > 0
#a <- 0.7;
#b <- 0.9;

##Teste ruim de chute pois é bem longe do resultado
#x0 <- 50;
#x1 <- 45;

##Tolerancia proxima ao zero de maquina
#tol <- 10^-16;

#automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
