source("automatizacaoZeroFuncao.R");


f <- function(x) { 1/tan(x) };
df <- function(x) { -(1/sin(x))^2  };
ddf <- function(x) { 2*(1/tan(x))* (1/sin(x))^2 };

#VER
a <- 1;
b <- 3;

#VER
x0 <- 1;
x1 <- 2;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
