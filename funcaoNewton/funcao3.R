source("automatizacaoZeroFuncao.R");

f  <-function (x) { exp(x)/2 - 2*cos(x) };
df <-function (x) { exp(x)/2 + 2*sin(x) };
ddf<-function (x) { exp(x)/2 + 2*cos(x) };

# Teste ruim, provavelmente por causa do método
a <- 0.904;
b <- 0.9048;

# Teste ruim de chute pois é bem longe do resultado
x0 <- 0;
x1 <- -1;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);