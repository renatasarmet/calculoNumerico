source("automatizacaoZeroFuncao.R");

f <- function(x) { x - x*log(x) };
df <- function(x) { -log(x) - 1 };
ddf <-function(x) { -1/x - 1 };

#Não é possível determinar chute de intervalo bom. Apesar do intervalo agora ser bem maior, o numero de iteraçoes mudou pouco
a <- 0.2;
b <- 100;

#Teste ruim de chute para Newton e pior para Secante, porém a Secante anda rápido mesmo assim. Tambem percebemos que Newton teve mais ou menos o mesmo tanto de iteração tanto com teste longe quanto com teste ruim; o problema dele foi próximo ao resultado final
x0 <- 100;
x1 <- 200;

#Tambem podemos perceber pelo gráfico que o chute tem que ser depois de 1, pois se nao causa indefinido (será que convem fazer um terceiro teste sobre isso?)

#Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
