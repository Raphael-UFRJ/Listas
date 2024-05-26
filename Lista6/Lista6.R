# Instalar e carregar as bibliotecas necessárias
install.packages("quantmod")
install.packages("tseries")
install.packages("PerformanceAnalytics")
install.packages("MASS")

library(quantmod) # Para importar dados financeiros históricos
library(tseries) # Para realizar testes estatísticos
library(PerformanceAnalytics) # Para análises de desempenho de investimentos
library(MASS) # Para gerar amostras de distribuições multivariadas

# Função para importar dados do Yahoo Finance
q1 <- function(simbolos) {
  getSymbols(simbolos, src = "yahoo", from = Sys.Date() - 365, to = Sys.Date())
  precos <- do.call(merge, lapply(simbolos, function(simbolo) Ad(get(simbolo))))
  return(precos)
}

# Função para estimar os parâmetros do modelo CER
q2 <- function(precos) {
  retornos <- na.omit(diff(log(precos)))
  medias <- colMeans(retornos)
  matriz_cov <- cov(retornos)
  return(list(medias = medias, matriz_cov = matriz_cov))
}

# Função para verificar a normalidade dos retornos
q3 <- function(retornos) {
  testes_normalidade <- apply(retornos, 2, jarque.bera.test)
  return(testes_normalidade)
}

# Função para calcular a matriz de correlação empírica
q4 <- function(retornos) {
  matriz_cor <- cor(retornos)
  return(matriz_cor)
}

# Função para comparar simulações MC do CER correlacionado com dados históricos
q5 <- function(medias, matriz_cov, retornos, num_simulacoes = 10000) {
  num_ativos <- ncol(retornos)
  num_periodos <- nrow(retornos)

  set.seed(123)
  retornos_simulados <- mvrnorm(num_simulacoes * num_periodos,
    mu = medias, Sigma = matriz_cov
  )
  retornos_simulados <- matrix(retornos_simulados,
    ncol = num_ativos, byrow = TRUE
  )

  medias_simuladas <- colMeans(retornos_simulados)
  matriz_cov_simulada <- cov(retornos_simulados)

  return(list(
    Medias_Historicas = medias,
    Medias_Simuladas = medias_simuladas,
    Cov_Historica = matriz_cov,
    Cov_Simulada = matriz_cov_simulada
  ))
}

# Função para estimar os valores dos ativos em Dezembro/2024
q6 <- function(precos, medias, matriz_cov,
               data_projecao = "2024-12-31",
               num_simulacoes = 10000) {
  periodo_proj <- as.integer(difftime(as.Date(data_projecao),
    Sys.Date(),
    units = "days"
  ))
  num_ativos <- ncol(precos)
  precos_atuais <- tail(precos, 1)

  precos_simulados <- matrix(0, nrow = num_simulacoes, ncol = num_ativos)

  for (i in 1:num_simulacoes) {
    retornos_futuros <- mvrnorm(periodo_proj,
      mu = medias / 252, Sigma = matriz_cov / 252
    )
    retornos_futuros_acumulados <- apply(retornos_futuros, 2, cumsum)
    precos_simulados[i, ] <- precos_atuais * exp(tail(retornos_futuros_acumulados, 1))
  }

  precos_futuros_estimados <- colMeans(precos_simulados)

  return(data.frame(
    Ativo = colnames(precos),
    Preco_Atual = as.numeric(precos_atuais),
    Preco_Futuro_Estimado = precos_futuros_estimados
  ))
}

# Definir os símbolos dos ativos
simbolos <- c("PETR4.SA", "VALE3.SA", "ABEV3.SA", "BBAS3.SA")

# Questão 1: Importar dados
precos <- q1(simbolos)

# Questão 2: Estimar os parâmetros do modelo CER
parametros <- q2(precos)
retornos <- na.omit(diff(log(precos)))

# Questão 3: Verificar a normalidade
testes_normalidade <- q3(retornos)

# Questão 4: Calcular a matriz de correlação empírica
matriz_cor <- q4(retornos)

# Questão 5: Comparar simulações MC do CER correlacionado com dados históricos
comparacao <- q5(parametros$medias, parametros$matriz_cov, retornos)

# Questão 6: Estimar valores futuros dos ativos em Dezembro/2024
valores_futuros <- q6(
  precos, parametros$medias,
  parametros$matriz_cov
)

# Visualizar os resultados
list(
  Testes_Normalidade = testes_normalidade,
  Matriz_Correlacao = matriz_cor,
  Comparacao_Simulacao = comparacao,
  Valores_Futuros = valores_futuros
)

# Visualizar os resultados
print("Testes de Normalidade:")
print(testes_normalidade)

print("Matriz de Correlação:")
print(matriz_cor)

print("Comparação de Simulação:")
print(comparacao)

print("Valores Futuros Estimados:")
print(valores_futuros)