# N??mero de investimentos
n <- 10

# M??dia do retorno de cada investimento
mu <- 1e6

# Desvio padr??o do retorno de cada investimento
sigma <- 1.3e6

# Vetor com os coeficientes de correla????o
rho_valores <- c(0, 0.25, 0.5, 0.75, 0.9)
# Nomeado em homenagem a Charles Spearman, ?? frequentemente denotado pela letra grega '??' (rho) e ?? usado principalmente para an??lise de dados. 
# Mede a for??a e a dire????o da associa????o entre duas vari??veis classificadas.

# Fun????o para calcular a probabilidade de perda para um dado coeficiente de correla????o
calc_probabilidade_perda <- function(rho) {
  # Retorno total esperado da carteira
  retorno_total_esperado <- n * mu
  
  # Vari??ncia da carteira
  var_carteira <- n * sigma^2 + n * (n - 1) * rho * sigma^2
  
  # Desvio padr??o da carteira
  sd_carteira <- sqrt(var_carteira)
  
  # Calculando a probabilidade de perda (retorno total < 0)
  probabilidade_perda <- pnorm(0, mean = retorno_total_esperado, sd = sd_carteira)
  
  return(probabilidade_perda)
}

# Calculando as probabilidades de perda para cada coeficiente de correla????o
probabilidades_perda <- sapply(rho_valores, calc_probabilidade_perda)

# Exibindo os resultados
resultados <- data.frame(rho = rho_valores, probabilidade_perda = probabilidades_perda)
print(resultados)