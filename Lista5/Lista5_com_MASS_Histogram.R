# Instalar e carregar o pacote MASS
if (!require(MASS)) install.packages("MASS")
library(MASS)

# Definir os par??metros do problema
n <- 10
mu <- 1e6
sigma <- 1.3e6
num_simulacoes <- 100000
rho_values <- c(0, 0.25, 0.5, 0.75, 0.9)

# Fun????o para calcular a probabilidade de perda e retornar os retornos totais
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
  
  return(list(probabilidade_perda = probabilidade_perda, retornos_totais = retornos_totais))
}

# Criar um dataframe para armazenar os resultados
resultados <- data.frame()

# Gerar os retornos totais e a probabilidade de perda para cada valor de correla????o
for (rho in rho_values) {
  resultado <- calc_probabilidade_perda(rho)
  retornos <- resultado$retornos_totais
  probabilidade_perda <- resultado$probabilidade_perda
  
  # Armazenar os resultados no dataframe
  resultados <- rbind(resultados, data.frame(retornos, rho = rho, probabilidade_perda = probabilidade_perda))
}

# Criar os histogramas para cada valor de correla????o
par(mfrow=c(3, 2)) # Configurar layout para m??ltiplos histogramas

for (rho in rho_values) {
  dados_filtro <- subset(resultados, rho == rho)$retornos
  
  # Criar histograma com marca????es
  hist(dados_filtro, breaks = 50, 
       xlab = "Retorno Total", 
       ylab = "Frequ??ncia", 
       main = paste("Distribui????o do Retorno Total (rho =", rho, ")"),
       col = "lightblue", border = "black")
  
  abline(v = 0, col = "red", lwd = 2)
}

# Resetar layout para um ??nico gr??fico
par(mfrow=c(1, 1))
