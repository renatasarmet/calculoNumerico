source("automatizacaoZeroFuncao.R");

f <- function(x) { x^5 - (10*x^3)/9 + 5/21 }; 
df <- function(x) { 5*x^4 - (10*x^2)/3};
ddf <- function(x) { 20*x - 20*x/3 };

#REESCREVER TUDO COM A FUNCAO NOVA

##Teste ruim de intervalo, pois ele chega quase no resultado, entao fica muito tempo por ir sempre na metade
a <- 0.8;
b <- 0.9;

# Teste bom de chute pois é perto do resultado
x0 <- 0.83;
x1 <- 1;

## Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
