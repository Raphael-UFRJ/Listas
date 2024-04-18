# 1-Um casco de navio consiste de 562 placas met ́alicas que devem ser rebitadas.
# Estima-se que o tempo gasto por um rebitador seja dado por (3h45,5h30,4h15) por
# placa e que cada rebitador recebe ( $7.00, $8.50, $7.50) por hora trabalhada.
#   1. Crie um modelo de risco de custo de m ̃ao-obra de rebitagem usando MC
#   2. Compare a distribui ̧c ̃ao cumulativa obtida usando a abordagem MC com aquela obtida usando o TCL.

q1 <- function(n) {
  # Tempo gasto por placa por rebitador
  tempos <- c(3.75, 5.5, 4.25)
  # Custo por hora de cada rebitador
  custos <- c(7.00, 8.50, 7.50)
  
  # Amostragem Monte Carlo
  custo_total <- replicate(n, {
    tempo <- sample(tempos, 1)
    custo_hora <- sample(custos, 1)
    custo_placa <- tempo * custo_hora
    custo_placa * 562
  })
  
  # Calculando métricas estatísticas
  media_custo <- mean(custo_total)
  desvio_padrao_custo <- sd(custo_total)
  primeiro_quartil <- quantile(custo_total, 0.25)
  terceiro_quartil <- quantile(custo_total, 0.75)
  
  # Exibindo métricas estatísticas
  cat("Média do Custo Total de Mão-de-Obra:", media_custo, "\n")
  cat("Desvio Padrão do Custo Total de Mão-de-Obra:", desvio_padrao_custo, "\n")
  cat("Primeiro Quartil do Custo Total de Mão-de-Obra:", primeiro_quartil, "\n")
  cat("Terceiro Quartil do Custo Total de Mão-de-Obra:", terceiro_quartil, "\n")
  
  # Plotando o histograma do custo total de mão-de-obra
  hist(custo_total, breaks = "FD", xlab = "Custo Total de Mão-de-Obra", main = "Histograma do Custo Total de Mão-de-Obra")
}

q1(10000)
