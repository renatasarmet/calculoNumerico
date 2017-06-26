source("automatizacaoZeroFuncao.R");

f <- function(x) { 1/tg(x) };
df <- function(x) { -csc(x)^2  };
ddf <- function(x) { 2*cot(x) * csc(x)^2 };

#VER
a <- 0;
b <- 1;

#VER
x0 <- -0.2;
x1 <- -0.3;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
