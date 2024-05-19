# Simulação de Probabilidade de Perda em Carteiras de Investimentos

Este projeto simula a probabilidade de perda em uma carteira de investimentos composta por 10 ativos, considerando diferentes coeficientes de correlação entre os ativos. Utilizamos a função `mvrnorm` do pacote `MASS` em R para gerar amostras normais correlacionadas e calcular a probabilidade de que o retorno total da carteira seja negativo.

## Requisitos

- R (versão 3.6 ou superior)
- Pacote `MASS`

## Instalação

Se você ainda não possui o R instalado, faça o download e a instalação a partir do site oficial: [https://www.r-project.org/](https://www.r-project.org/).

Para instalar o pacote `MASS`, execute o seguinte comando no console R:

```R
install.packages("MASS")
```

## Uso
1. Clone este repositório ou baixe os arquivos para o seu computador.
2. Abra o R ou RStudio.
3. Carregue o código-fonte principal (por exemplo, simulacao_probabilidade_perda.R).
