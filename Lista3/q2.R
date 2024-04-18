q2 <- function(n) {
  # Carros ativos por dia
  carros_ativos <- c(15, 20, 18)
  # Consumo de gasolina por carro por dia (litros)
  consumo_gasolina <- c(40, 70, 58)
  # Custo variável por litro de gasolina
  custo_gasolina <- c(5.70, 7.20, 6.00)
  
  # Inicializando uma matriz para armazenar os resultados de cada iteração
  resultados <- matrix(NA, nrow = n, ncol = 3)
  
  # Amostragem Monte Carlo e armazenamento dos resultados
  for (i in 1:n) {
    carros <- sample(carros_ativos, 1)
    consumo <- sample(consumo_gasolina, 1)
    custo_litro <- sample(custo_gasolina, 1)
    custo_combustivel_dia <- carros * consumo * custo_litro
    resultados[i, ] <- c(carros, consumo, custo_litro)
  }
  
  # Convertendo a matriz de resultados em um data frame
  resultados_df <- as.data.frame(resultados)
  colnames(resultados_df) <- c("Carros Ativos", "Consumo por Carro (litros)", "Custo por Litro (R$)")
  
  # Exibindo a tabela
  cat("Tabela de Resultados:\n")
  print(resultados_df)
  
  # Calculando métricas estatísticas
  media_custo_combustivel <- mean(resultados[, 1] * resultados[, 2] * resultados[, 3])
  desvio_padrao_custo_combustivel <- sd(resultados[, 1] * resultados[, 2] * resultados[, 3])
  primeiro_quartil_combustivel <- quantile(resultados[, 1] * resultados[, 2] * resultados[, 3], 0.25)
  terceiro_quartil_combustivel <- quantile(resultados[, 1] * resultados[, 2] * resultados[, 3], 0.75)
  
  # Exibindo métricas estatísticas
  cat("\nMédia do Custo Diário de Combustível:", media_custo_combustivel, "\n")
  cat("Desvio Padrão do Custo Diário de Combustível:", desvio_padrao_custo_combustivel, "\n")
  cat("Primeiro Quartil do Custo Diário de Combustível:", primeiro_quartil_combustivel, "\n")
  cat("Terceiro Quartil do Custo Diário de Combustível:", terceiro_quartil_combustivel, "\n")
  
  # Plotando o histograma do custo diário de combustível
  hist(resultados[, 1] * resultados[, 2] * resultados[, 3], breaks = "FD", xlab = "Custo Diário de Combustível", main = "Histograma do Custo Diário de Combustível")
}

# Chamada da função q2 com 10000 iterações
q2(10000)
