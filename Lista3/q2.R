# 2-A frota de uma empresa de taxi  ??e composta por 20 ve ????culos. Por ??em, acidentes e
# a necessidade de manuten ??c ??ao fazem com que a frota ativa tenha uma distribui ??c ??ao de
# (15, 20, 18) carros ativos num dia. Sabendo que, cada carro consome (40,70,58) litros
# de gasolina por dia a um custo vari ??avel de (5.70, 7.20, 6.00) reais/ litro. Usando MC,
# crie um modelo de risco de custo para o gasto di ??ario de combust ????vel da empresa de taxi.

q2 <- function(n) {
  # Carros ativos por dia
  carros_ativos <- c(15, 20, 18)
  # Consumo de gasolina por carro por dia (litros)
  consumo_gasolina <- c(40, 70, 58)
  # Custo vari??vel por litro de gasolina
  custo_gasolina <- c(5.70, 7.20, 6.00)
  
  # Amostragem Monte Carlo
  custo_combustivel <- replicate(n, {
    carros <- sample(carros_ativos, 1)
    consumo <- sample(consumo_gasolina, 1)
    custo_litro <- sample(custo_gasolina, 1)
    custo_combustivel_dia <- carros * consumo * custo_litro
    custo_combustivel_dia
  })
  
  hist(custo_combustivel, breaks = "FD", xlab = "Custo Di??rio de Combust??vel", main = "Histograma do Custo Di??rio de Combust??vel")
}

# Chamada da fun????o q2 com 10000 itera????es
q2(10000)