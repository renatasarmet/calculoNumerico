funcao1 <- function(x) { x^5 - (10*x^3)/9 + 5/21 };
df1 <- function(x) { 5*x^4 - (10*x^2)/3};
ddf1 <- function(x) { 20*x - 20*x/3 };

print("xNewton: chute inicial pra newton\nxBissecao[0]: inferior\nxBissecao[1]: superior\nxSecante[0]: inferior\nxSecante[1]: superior");

xNewton <- -0.8;
xBisseccao[0] <- -0.75;
xBisseccao[1] <- -0.25;
xSecante[0] <- 0.8;
xSecante[1] <- 1;