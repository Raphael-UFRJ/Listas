# 3-Um investidor deseja avaliar o retorno de um investimento em um restaurante.
#   Uma avalia ̧c ̃ao de especialistas mosstrou que:
#    1. cada cliente gasta (150, 500, 250)
#    2. receba a visita di ́aria de (40, 120, 60) clientes.
#    3. o lucrosobre o faturamento  ́e de (15% ,30%, 22%)
#  Calcule o risco do do lucro total do restaurante durante o primeiro ano de opera ̧c ̃ao,
#  assumindo que o restaurante opera 300 dias por ano.

# Definindo os dados
gastoCliente <- c(150, 500, 250)
clientesDiarios <- c(40, 120, 60)
margemLucro <- c(0.15, 0.30, 0.22)
operacaoAno <- 300

# Função para calcular o lucro diário
calcularLucroDiario <- function() {
  gastoAleatorio <- sample(gastoCliente, 1)
  clientesAleatorios <- sample(clientesDiarios, 1)
  margemAleatoria <- sample(margemLucro, 1)
  
  faturamentoDiario <- gastoAleatorio * clientesAleatorios
  lucroDiario <- faturamentoDiario * margemAleatoria
  
  return(lucroDiario)
}

# Realizando calculo do lucro anual
lucroAnual <- replicate(operacaoAno, calcularLucroDiario())

# Calculando a média e o desvio padrão dos lucros diários
mediaLucrosDiarios <- mean(lucroAnual)
desvioPadraoLucrosDiarios <- sd(lucroAnual)

# Calculando o risco do lucro total
riscoLucroTotal <- desvioPadraoLucrosDiarios * sqrt(operacaoAno)

# Exibindo o resultado
cat("O risco do lucro total do restaurante durante o primeiro ano de operação é de aproximadamente:", riscoLucroTotal, "\n")

