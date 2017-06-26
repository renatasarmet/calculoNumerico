source("automatizacaoZeroFuncao.R");

f  <-function (x) { exp(x)/2 - 2*cos(x) };
df <-function (x) { exp(x)/2 + 2*sin(x) };
ddf<-function (x) { exp(x)/2 + 2*cos(x) };

# Teste ruim, provavelmente por causa do método
# todos os testes não se diferenciaram muito entre si

# aqui temos um intervalo relativamente grande.
# o resultado é semelhante para quando b = 10 (igual),
# b = 30 (mais 2 iterações), b = 50 (mais 4 iteações).
# para b  ~= 0.9, há diferença de aprox 10 iterações 
# (eram 44 e agora sao aprox 55)
a <- 0;
b <- 5;

# Testei perto da parte predominantemente exponencial 
# (pior das hipóteses) (derivada maior) e, fora os casos de
# troca de sentido da concavidade, quando não é possível 
# aplicar o método,
# os métodos de newton e secante aparentam não mudar muito.

# neste teste o método de newton demora, pois é um chute alto.
# contudo, o método da secante continua semelhante ao teste 
# anterior. 
x0 <- 60;
x1 <- 3;

# Toleranci proxima ao zero de maquina
tol <- 10^-16;

automatiza(f,df,ddf,a,b,x0,x1,tol,tol,tol,tol,tol,tol,tol,tol,tol);