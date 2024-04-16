# 3-Um investidor deseja avaliar o retorno de um investimento em um restaurante.
#   Uma avalia ̧c ̃ao de especialistas mosstrou que:
#    1. cada cliente gasta (150, 500, 250)
#    2. receba a visita di ́aria de (40, 120, 60) clientes.
#    3. o lucrosobre o faturamento  ́e de (15% ,30%, 22%)
#  Calcule o risco do do lucro total do restaurante durante o primeiro ano de opera ̧c ̃ao,
#  assumindo que o restaurante opera 300 dias por ano.

#dados
install.packges("triangle")
library(triangle)
gastoClientes <- rtriangle(150, 500, 250)
clientesDiarios <- rtriangle(40,120,60)
margemLucro <- c(0.15,0.30,0.20)
dias <- 300

#calculo do faturamento e lucro
faturamentoMedioDiario <- mean(gastoClientes)*mean(clientesDiarios)
lucroAnualDiario <-mean(faturamentoMedioDiario*margemLucro)
lucroMedioAnual <- lucroAnualDiario*dias
  
desvioPadraoMargem <- sd(margemLucro)

print(paste("O risco do lucro total do restaurante durante o primeiro ano de operacao e de aproximadamente:", desvioPadraoAnual))
