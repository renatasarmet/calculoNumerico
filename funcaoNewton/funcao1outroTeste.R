source("automatizacaoZeroFuncao.R");

f <- function(x) { x^5 - (10*x^3)/9 + 5/21 };
df <- function(x) { 5*x^4 - (10*x^2)/3};
ddf <- function(x) { 20*x - 20*x/3 };

#Intervalo nao converge pois f(a) * f(b) > 0
a <- 0.7;
b <- 0.9;

#Teste ruim de chute pois é bem longe do resultado
x0 <- 50;
x1 <- 45;

#Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
