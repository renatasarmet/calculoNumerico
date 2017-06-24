source("automatizacaoZeroFuncao.R");

f  <-function (x) { exp(x)/2 - 2*cos(x) };
df <-function (x) { exp(x)/2 + 2*sen(x) };
dff<-function (x) { exp(x)/2 + 2*cos(x) };

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);