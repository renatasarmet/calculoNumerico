source("automatizacaoZeroFuncao.R");

f <- function(x) { e^sin(x) - 2 };
df <- function(x) { e^sin(x) * cos(x)  };
ddf <- function(x) { e^sin(x) * (cos(x)^2 - sin(x)) };

#VER
a <- 0;
b <- 10;

#VER
x0 <- 8;
x1 <- 10;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
