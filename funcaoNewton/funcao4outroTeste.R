source("automatizacaoZeroFuncao.R");

f <- function (x) { 1/x + 1/(x^2) };
df <-function (x) { -1/(x^2) -2/(x^3) };
dff<-function (x) { 2/(x^3) + 6/(x^4) };

#Intervalo longe, porem continua parecido com o pequeno quanto ao numerico de iteracoes e onde mais demora
a <- -40;
b <- -0.5;

#Teste bom para secante e newton. Newton vai mais rapido mesmo o segundo chute da secante sendo em direcao ao resultado
x0 <- -1.7; # ta indo pro outro lado temos que pegar outro
x1 <- -1.5;

# Citar dificuldade e encontrar um chute, tanto bom quanto ruim. Ha muito problema de nao convergencia

#Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
