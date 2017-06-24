source("automatizacaoZeroFuncao.R");

f <- function(x) { x^5 - (10*x^3)/9 + 5/21 };
df <- function(x) { 5*x^4 - (10*x^2)/3};
ddf <- function(x) { 20*x - 20*x/3 };

#Teste ruim
a <-0.852;
b <- 0.853;
x0 <- 0.8;
x1 <- 1;
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
