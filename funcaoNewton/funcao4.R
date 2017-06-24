source("automatizacaoZeroFuncao.R");

f <- function (x) { 1/x + 1/(x^2) };
df <-function (x) { -1/(x^2) -2/(x^3) };
dff<-function (x) { 2/(x^3) + 6/(x^4) };

#Intervalo bom, porem demora muito principalmente quando esta proximo ao resultado
a <- -2;
b <- -0.5;

#Teste bom para secante e newton. Newton vai mais rapido mesmo o segundo chute da secante sendo em direcao ao resultado
x0 <- -0.5;
x1 <- -0.7;

#Tambem podemos perceber pelo gráfico que o chute tem que ser depois de 1, pois se nao causa indefinido (será que convem fazer um terceiro teste sobre isso?)

#Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
