source("automatizacaoZeroFuncao.R");

f <- function(x) { x - x*log(x) };
df <- function(x) { -log(x) - 1 };
ddf <-function(x) { -1/x - 1 };

#Não é possível determinar chute de intervalo bom
a <- 2.1;
b <- 3;

#Teste bom de chute para secante porém ruim para Newton
x0 <- 1.6;
x1 <- 2;

#Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
