source("automatizacaoZeroFuncao.R");

f <- function(x) { x^11 - 0.001 };
df <- function(x) { 11*x^10 };
ddf <- function(x) { 110*x^9 };

#VER
a <- -1;
b <- 1;

#VER
x0 <- 0;
x1 <- 0.5;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
