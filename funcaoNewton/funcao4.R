source("automatizacaoZeroFuncao.R");

f <- function (x) { 1/x + 1/(x^2) };
df <-function (x) { -1/(x^2) -2/(x^3) };
dff<-function (x) { 2/(x^3) + 6/(x^4) };

x0 <- 1;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);