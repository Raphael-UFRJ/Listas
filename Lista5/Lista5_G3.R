# Carregar os pacotes
library(MASS)
library(ggplot2)

# Definir os par??metros do investimento
n <- 10  # N??mero de investimentos
mu <- 1e6  # M??dia do retorno de cada investimento
sigma <- 1.3e6  # Desvio padr??o do retorno de cada investimento
num_simulacoes <- 100000  # N??mero de simula????es
rho_valores <- c(0, 0.25, 0.5, 0.75, 0.9)  # Vetor com os coeficientes de correla????o

# Fun????o para calcular a probabilidade de perda
calc_probabilidade_perda <- function(rho) {
  R <- matrix(rho, n, n) # Matriz de correla????o
  diag(R) <- 1 # Ajustando a matriz de correla????o R para garantir que todos os elementos da diagonal principal sejam iguais a 1.
  s <- rep(sigma, n) # Vetor de desvios padr??o
  S <- diag(s) # Matriz de covari??ncia
  # C??lculo da Matriz de Covari??ncia - %*% denota a multiplica????o de matrizes
  CV <- S %*% R %*% S # Primeiro, multiplica S por R: S %*% R Depois, multiplica o resultado novamente por S: (S %*% R) %*% S
  # Gerando amostras normais correlacionadas
  amostras <- mvrnorm(num_simulacoes, mu = rep(mu, n), Sigma = CV)
  retornos_totais <- rowSums(amostras)
  probabilidade_perda <- mean(retornos_totais < 0)
  return(list(retornos_totais = retornos_totais, probabilidade_perda = probabilidade_perda))
}

# Fun????o para plotar o histograma e destacar a ??rea de perda
plot_histogram_density <- function(rho) {
  results <- calc_probabilidade_perda(rho)
  retornos_totais <- results$retornos_totais
  probabilidade_perda <- results$probabilidade_perda
  hist_data <- data.frame(retornos = retornos_totais)
  
  gg <- ggplot(hist_data, aes(x = retornos)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = '#67B7D1', alpha = 0.5) +
    geom_density(color = '#67B7D1') +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    geom_area(stat = "density", data = subset(hist_data, retornos < 0), aes(x = retornos, y = after_stat(density)), fill = 'red', alpha = 0.3) +
    labs(title = paste("Distribui????o de Retornos Totais\n(rho =", rho, ")\nProbabilidade de Perda =", round(probabilidade_perda * 100, 2), "%"),
         x = "Retorno Total",
         y = "Densidade") +
    theme_minimal() +
    scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
  
  print(gg)
  
  return(probabilidade_perda)
}

# Lista para armazenar as probabilidades de perda
probabilidades_perda <- sapply(rho_valores, function(rho) {
  probabilidade_perda <- plot_histogram_density(rho)
  return(probabilidade_perda)
})

# Exibir as probabilidades de perda
resultados <- data.frame(rho = rho_valores, probabilidade_perda = probabilidades_perda)
print(resultados)
