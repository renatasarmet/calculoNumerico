source("automatizacaoZeroFuncao.R");

f  <-function (x) { exp(x)/2 - 2*cos(x) };
df <-function (x) { exp(x)/2 + 2*sin(x) };
ddf<-function (x) { exp(x)/2 + 2*cos(x) };

# Teste ruim, provavelmente por causa do método
# todos os testes não se diferenciaram muito entre si
# nesse teste temos um intervalo pequeno.
a <- 0.904;
b <- 0.9048;

# Testei perto da parte predominantemente exponencial 
# (pior das hipóteses) (derivada maior) e, fora os casos de
# troca de sentido da concavidade, quando não é possível 
# aplicar o método,
# os métodos de newton e secante aparentam não mudar muito.
x0 <- .8;
x1 <- 10;

# Tolerancia proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);
