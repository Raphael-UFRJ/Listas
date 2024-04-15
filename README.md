# Lista-3
Lista de exercícios para avaliação do curso de Analise de Risco da UFRJ
---
title: "Simulação Monte Carlo em R"
output: html_notebook
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

---
Questão 1: Modelo de Custo de Mão-de-Obra de Rebitagem
---

```{r}
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
  
  hist(custo_total, breaks = "FD", xlab = "Custo Total de Mão-de-Obra", main = "Histograma do Custo Total de Mão-de-Obra")
}

q1(10000)
```

---
Questão 2: Modelo de Custo Diário de Combustível para a Frota de Táxi
---

```{r}
q2 <- function(n) {
  # Carros ativos por dia
  carros_ativos <- c(15, 20, 18)
  # Consumo de gasolina por carro por dia (litros)
  consumo_gasolina <- c(40, 70, 58)
  # Custo variável por litro de gasolina
  custo_gasolina <- c(5.70, 7.20, 6.00)
  
  # Amostragem Monte Carlo
  custo_combustivel <- replicate(n, {
    carros <- sample(carros_ativos, 1)
    consumo <- sample(consumo_gasolina, 1)
    custo_litro <- sample(custo_gasolina, 1)
    custo_combustivel_dia <- carros * consumo * custo_litro
    custo_combustivel_dia
  })
  
  hist(custo_combustivel, breaks = "FD", xlab = "Custo Diário de Combustível", main = "Histograma do Custo Diário de Combustível")
}

# Chamada da função q2 com 10000 iterações
q2(10000)
```

---
Questão 3: Modelo de Lucro Total do Restaurante
---

```
```
