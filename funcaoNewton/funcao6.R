source("automatizacaoZeroFuncao.R");

f <- function(x) { exp(1)^sin(x) - 2 };
df <- function(x) { exp(1)^sin(x) * cos(x)  };
ddf <- function(x) { exp(1)^sin(x) * (cos(x)^2 - sin(x)) };

#VER
a <- -1;
b <- 2;

#VER
x0 <- 2;
x1 <- 2.1;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
