source("automatizacaoZeroFuncao.R");

f <- function(x) { e^sin(x) - 2 };
df <- function(x) { e^sin(x) * cos(x)  };
ddf <- function(x) { e^sin(x) * (cos(x)^2 - sin(x)) };

#VER
a <- -1;
b <- 1;

#VER
x0 <- 14;
x1 <- 24;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
