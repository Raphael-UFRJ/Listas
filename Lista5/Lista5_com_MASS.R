install.packages("MASS")
library(MASS)

# N??mero de investimentos
n <- 10

# M??dia do retorno de cada investimento
mu <- 1e6

# Desvio padr??o do retorno de cada investimento
sigma <- 1.3e6

# N??mero de simula????es
num_simulacoes <- 100000

# Vetor com os coeficientes de correla????o
rho_valores <- c(0, 0.25, 0.5, 0.75, 0.9)

# Fun????o para calcular a probabilidade de perda para um dado coeficiente de correla????o
calc_probabilidade_perda <- function(rho) {
  # Matriz de correla????o
  R <- matrix(rho, n, n)
  diag(R) <- 1
  
  # Vetor de desvios padr??o
  s <- rep(sigma, n)
  
  # Matriz de covari??ncia
  S <- diag(s)
  CV <- S %*% R %*% S
  
  # Gerando amostras normais correlacionadas
  amostras <- mvrnorm(num_simulacoes, mu = rep(mu, n), Sigma = CV)
  
  # Calculando os retornos totais
  retornos_totais <- rowSums(amostras)
  
  # Calculando a probabilidade de perda (retorno total < 0)
  probabilidade_perda <- mean(retornos_totais < 0)
  
  return(probabilidade_perda)
}

# Calculando as probabilidades de perda para cada coeficiente de correla????o
probabilidades_perda <- sapply(rho_valores, calc_probabilidade_perda)

# Exibindo os resultados
resultados <- data.frame(rho = rho_valores, probabilidade_perda = probabilidades_perda)
print(resultados)