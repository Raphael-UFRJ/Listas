# Monte Carlo é uma técnica estatística que utiliza amostragem aleatória para estimar resultados de interesse em problemas complexos.

#Questão 1
# Crie o experimento computacional ”lancar uma moeda n vezes” repetido m vezes” e produza o histograma da do numero de sucessos em cada experimento. Repita o experimento e tire conclusoes sobre o efeito da amostragem.
q1 <- function(n, m) {
  resultados <- replicate(m, sum(sample(c(0, 1), n, replace = TRUE)))
  hist(resultados,
       breaks = seq(0, n + 1),
       xlab = "Número de Sucessos",
       main = "Histograma de Número de Sucessos")
}

#Explicação:
# sample(c(0, 1), n, replace = TRUE): Isso cria uma amostra aleatória de n lançamentos de uma moeda. O vetor c(0, 1) representa os possíveis resultados do lançamento (0 para cara e 1 para coroa), e o argumento replace = TRUE indica que os resultados são amostrados com substituição, o que significa que cada lançamento é independente dos outros.

# hist(resultados, breaks = seq(0, n+1), xlab = "Número de Sucessos", main = "Histograma de Número de Sucessos"): Isso cria um histograma dos resultados, onde resultados é o vetor de valores que queremos agrupar no histograma. O argumento breaks define os limites dos intervalos do histograma. No caso, estamos usando seq(0, n+1) para garantir que cada possível número de sucessos seja representado em uma barra do histograma.

#Conclusão:
#  A fórmula matemática básica aqui é simplesmente a contagem do número de sucessos em uma série de lançamentos de moeda, onde cada lançamento tem uma probabilidade de sucesso de 0.5. A soma desses sucessos em cada repetição do experimento é então usada para construir o histograma, que nos dá uma ideia da distribuição dos resultados possíveis.


#------------------------------------
# Questão 2: Seja X uma VA que  ́e obtida pela soma de 12 VAs uniformes (0,1):

# Questão 2a - Usando o TCL, obtenha uma aproximacao analitica para X:
# calcule a sua media, variancia e esboce um grafico dessa distribuicao.
q2a <- function() {
  media <- 12 * 0.5
  variancia <- 12 * (1 / 12) * (1 / 12)
  cat("Média:", media, "\n")
  cat("Variância:", variancia, "\n")
  curve(dnorm(x,
              mean = media,
              sd = sqrt(variancia)),
        from = 0,
        to = 12,
        xlab = "X",
        ylab = "Densidade",
        main = "Distribuição de X")
}

#Explicação:
#  media <- 12 * 0.5: A média de uma variável aleatória uniforme no intervalo (0, 1) é (a + b) / 2, onde "a" é o limite inferior (0) e "b" é o limite superior (1). Como estamos somando 12 dessas variáveis, a média da soma será 12 * média de uma única variável, que é 12 * 0.5 = 6. Isso segue do fato de que a média de uma variável uniforme em (0, 1) é 0.5.

#  variancia <- 12 * (1/12) * (1/12): A variância de uma variável aleatória uniforme no intervalo (0, 1) é (b - a)^2 / 12, onde a e "b" são os limites inferior e superior, respectivamente. Novamente, como estamos somando 12 dessas variáveis independentes, a variância da soma será 12 * variância de uma única variável, que é 12 * (1/12) * (1/12) = 1/12. Isso segue do fato de que a variância de uma variável uniforme em (0, 1) é 1/12.

# curve(dnorm(x, mean = media, sd = sqrt(variancia)), from = 0, to = 12, xlab = "X", ylab = "Densidade", main = "Distribuição de X"): Aqui, estamos usando a função curve para traçar a curva da distribuição normal com média media e desvio padrão sqrt(variancia) (raiz quadrada da variância) no intervalo de 0 a 12. Isso é feito usando dnorm, que é uma função em R que calcula os valores de densidade de probabilidade para uma distribuição normal.

#Conclusão:
#  A função calcula a média e a variância da soma de 12 variáveis aleatórias uniformemente distribuídas (0, 1) e, em seguida, esboça o gráfico da distribuição normal que modela essa soma de acordo com o Teorema Central do Limite (TCL). O TCL afirma que, para um grande número de variáveis aleatórias independentes e idênticas, sua soma se aproxima de uma distribuição normal, independentemente da distribuição original das variáveis.)

#------------------------------------

# Questão 2b
#idem ao 2a usando MC. O método Monte Carlo é usado para obter uma aproximação empírica da distribuição da soma de 12 variáveis aleatórias uniformemente distribuídas.
q2b <- function() {
  n <- 12000
  x <- matrix(runif(n * 12), ncol = 12)
  soma_x <- rowSums(x)
  media <- mean(soma_x)
  variancia <- var(soma_x)
  cat("Média:", media, "\n")
  cat("Variância:", variancia, "\n")
  hist(soma_x,
       breaks = "Freedman-Diaconis",
       xlab = "X",
       main = "Histograma da Soma de 12 VAs Uniformes")
}
#Explicação:
#  x <- matrix(runif(n*12), ncol = 12): Aqui, estamos gerando uma matriz x de dimensões n (que foi setada em 12000) por 12, onde cada elemento é uma amostra aleatória de uma variável uniforme no intervalo (0, 1). Isso é feito usando a função runif, que gera números aleatórios uniformemente distribuídos.

#  soma_x <- rowSums(x): Esta linha calcula a soma de cada linha da matriz x, ou seja, a soma das 12 variáveis aleatórias para cada uma das n amostras.

#  media <- mean(soma_x): Aqui, calculamos a média dos resultados da soma das 12 variáveis aleatórias. Isso nos dá uma estimativa da média da distribuição da soma.

#  variancia <- var(soma_x): Esta linha calcula a variância dos resultados da soma das 12 variáveis aleatórias. Isso nos dá uma estimativa da variância da distribuição da soma.

#  cat("Média:", media, "\n") e cat("Variância:", variancia, "\n"): Estas linhas imprimem na tela a média e a variância calculadas.

#  hist(soma_x, breaks = "Freedman-Diaconis", xlab = "X", main = "Histograma da Soma de 12 VAs Uniformes"): Aqui, estamos plotando um histograma dos resultados da soma das 12 variáveis aleatórias. O argumento breaks = "Freedman-Diaconis" indica que queremos utilizar o método de Freedman-Diaconis ou FD para determinar o número de intervalos no histograma. Os rótulos dos eixos x e do título do histograma são definidos pelos argumentos xlab e main, respectivamente.

#Conclusão:
#  A  função usa o método Monte Carlo para simular a soma de 12 variáveis aleatórias uniformemente distribuídas e estima a média e a variância dessa soma. Em seguida, gera um histograma para visualizar a distribuição dos resultados. Isso nos permite entender melhor o comportamento da soma dessas variáveis aleatórias e suas propriedades estatísticas.

#------------------------------------

# Questão 3
#Usando MC, obtenha uma aproximacao empirica para a VA que  ́e obtida pela soma de 5,10,15 distribuicoes triangulares Xi distribuidas com parametros (mini = 10, maxi = 30, mprovi = 20.
#    - usando TCL estime: media, variancia e plote um grafico de sua funcao de probabilidade.
#    - compare com os resultados obtidos por MC
#  O método Monte Carlo é usado para obter uma aproximação empírica da distribuição da soma de 5, 10 ou 15 distribuições triangulares.
q3 <- function(n, m) {
  if (n < 5 || n > 30 || m < 5 || m > 30) {
    stop("Os parâmetros n e m devem estar entre 5 e 30.")
  }
  x <- matrix(runif(n * m, 10, 30), ncol = m)
  soma_x <- rowSums(x)
  media <- mean(soma_x)
  variancia <- var(soma_x)
  cat("Média:", media, "\n")
  cat("Variância:", variancia, "\n")
  hist(soma_x,
       breaks = "FD",
       xlab = "X",
       main = "Histograma da Soma de Distribuições Triangulares")
}
#Explicação:
#  x <- matrix(runif(n*m, 10, 30), ncol = m): Aqui, estamos gerando uma matriz x de dimensões n por m, onde cada elemento é uma amostra aleatória de uma distribuição uniforme no intervalo entre 10 e 30. Isso é feito usando a função runif, que gera números aleatórios uniformemente distribuídos dentro do intervalo especificado.

#  hist(soma_x, breaks = "FD", xlab = "X", main = "Histograma da Soma de Distribuições Triangulares"): Aqui, estamos plotando um histograma dos resultados da soma das variáveis aleatórias. O argumento breaks = "FD" indica que queremos utilizar o método de Freedman-Diaconis para determinar o número de intervalos no histograma. Os rótulos dos eixos x e do título do histograma são definidos pelos argumentos xlab e main, respectivamente.

#Conclusão:
#  A função usa o método Monte Carlo para simular a soma de m variáveis aleatórias que seguem uma distribuição triangular com parâmetros específicos e estima a média e a variância dessa soma. Em seguida, gera um histograma para visualizar a distribuição dos resultados. Isso nos permite entender melhor o comportamento da soma dessas variáveis aleatórias e suas propriedades estatísticas.

#------------------------------------
# Questão 4: Usando MC obtenha uma aproximacao empırica para a funcao de probabilidade

# Questão 4a
#produto de duas VAs Z = X * Y, X, Y ~ N(0, 1).
#o método Monte Carlo é usado para obter uma aproximação empírica das funções de probabilidade especificadas para diferentes variáveis aleatórias.
q4a <- function(n) {
  x <- rnorm(n)
  y <- rnorm(n)
  z <- x * y
  hist(z,
       breaks = "FD",
       xlab = "Z",
       main = "Histograma do Produto de Duas VAs Normais")
}
#Explicação:
#  x <- rnorm(n): Aqui, estamos gerando n amostras aleatórias de uma distribuição normal padrão (média 0 e desvio padrão 1) e armazenando-as na variável x, e em y <- rnorm(n), o mesmo só que armazenado em y.

#  z <- x * y: Aqui, estamos multiplicando as amostras correspondentes de x e y, obtendo assim o produto de duas variáveis aleatórias normais.

#  hist(z, breaks = "FD", xlab = "Z", main = "Histograma do Produto de Duas VAs Normais"): Esta linha cria um histograma dos valores resultantes do produto, armazenados em z.
#Conclusão:
#   Eesta função utiliza o método Monte Carlo para simular o produto de duas variáveis aleatórias normais padrão e gera um histograma para visualizar a distribuição dos resultados. Isso nos permite entender melhor o comportamento do produto dessas variáveis aleatórias e suas propriedades estatísticas.

#----------------------------------
# Questão 4b - quociente Z = X/Y .
q4b <- function(n) {
  x <- rnorm(n)
  y <- rnorm(n)
  z <- x / y
  hist(z,
       breaks = "FD",
       xlab = "Z",
       main = "Histograma do Quociente de Duas VAs Normais")
}
#Explicação:
#  quociente Z = X/Y 
#  hist(z, breaks = "FD", xlab = "Z", main = "Histograma do Quociente de Duas VAs Normais"): Esta linha cria um histograma dos valores resultantes da divisão, armazenados em z
#Conclusão:
#  Eesta função utiliza o método Monte Carlo para simular o quociente de duas variáveis aleatórias normais padrão e gera um histograma para visualizar a distribuição dos resultados. Isso nos permite entender melhor o comportamento do quociente dessas variáveis aleatórias e suas propriedades estatísticas.

#----------------------------------

# Questão 4c - Maximo(Xi)(i = 2, 5, 10) que representa a funcao de probabilidade do maximo dentre i VAs cada uma delas seguindo uma Normal(0, 1).
q4c <- function(n, i) {
  maximos <- apply(matrix(rnorm(n * i), ncol = i), 1, max)
  hist(maximos,
       breaks = "FD",
       xlab = "Máximo",
       main = paste("Histograma do Máximo de", i, "VAs Normais"))
}
#Explicação:
#  matrix(rnorm(n*i), ncol = i): Aqui, estamos gerando uma matriz com n linhas e i colunas, onde cada elemento é uma amostra aleatória de uma distribuição normal padrão (média 0 e desvio padrão 1). A função rnorm(n*i) gera n*i amostras aleatórias e a função matrix organiza essas amostras em uma matriz com i colunas.

#  apply(..., 1, max): A função apply é usada para aplicar a função max a cada linha da matriz. Isso retorna o máximo de cada linha, ou seja, o máximo de i variáveis aleatórias normais independentes.

#  maximos <- apply(...): Armazena os valores máximos de cada linha em um vetor chamado maximos.
#Conclusão:
#  A função utiliza o método Monte Carlo para simular o máximo de i variáveis aleatórias normais padrão e gera um histograma para visualizar a distribuição dos resultados. Isso nos permite entender melhor o comportamento do máximo dessas variáveis aleatórias e suas propriedades estatísticas.

#----------------------------------

# Questão 4d - χ2(n) = Pn1 X2i onde Xi ~ Normal(0, 1)(i = 1, 5, 10, 30).
q4d <- function(n, m) {
  x_quadrado <- replicate(m, sum(rnorm(n)^2))
  hist(x_quadrado,
       breaks = "FD",
       xlab = "χ^2",
       main = "Histograma da Distribuição χ^2")
}
#Explicação:
#  rnorm(n)^2: Aqui, estamos gerando n amostras aleatórias de uma distribuição normal padrão (média 0 e desvio padrão 1) e elevando cada uma ao quadrado. Isso nos dá n variáveis aleatórias que seguem uma distribuição χ² com 1 grau de liberdade, porque cada amostra ao quadrado segue uma distribuição de chi-quadrado com 1 grau de liberdade.

#  sum(rnorm(n)^2): Somamos todas as n amostras ao quadrado para obter uma única variável aleatória que segue uma distribuição χ² com 1 grau de liberdade.

#  replicate(m, ...): Replicamos o processo sum(rnorm(n)^2) m vezes para obter m valores da variável aleatória χ².

#  x_quadrado <- replicate(...): Armazenamos os valores da variável aleatória χ² em um vetor chamado x_quadrado.

#  hist(x_quadrado, breaks = "FD", xlab = "χ^2", main = "Histograma da Distribuição χ^2"): Esta linha cria um histograma dos valores da variável aleatória χ², armazenados em x_quadrado.
#Conclusão:
#  A função utiliza o método Monte Carlo para simular a distribuição de uma variável aleatória χ² com m graus de liberdade e gera um histograma para visualizar a distribuição dos resultados. Isso nos permite entender melhor o comportamento da variável aleatória χ² e suas propriedades estatísticas.

#----------------------------------

# Questão 4e - 'Z = eN(0,1)
q4e <- function(n) {
  z <- exp(rnorm(n))
  hist(z,
       breaks = "FD",
       xlab = "Z",
       main = "Histograma da Distribuição Z = e^N(0,1)")
}
#Explicação:
#  rnorm(n): Aqui, estamos gerando n amostras aleatórias de uma distribuição normal padrão (média 0 e desvio padrão 1).

#  exp(rnorm(n)): Aplicamos a função exponencial a cada uma das n amostras aleatórias geradas. Isso significa que estamos calculando eX , onde X é uma variável aleatória normal padrão.

#  z <- exp(rnorm(n)): Armazenamos os resultados das operações anteriores em um vetor chamado z, que representará nossa variável aleatória Z.

#  hist(z, breaks = "FD", xlab = "Z", main = "Histograma da Distribuição Z = e^N(0,1)"): Esta linha cria um histograma dos valores da variável aleatória Z.

#Conclusão:
#  A função utiliza o método Monte Carlo para simular a distribuição de uma variável aleatória Z, onde Z é a exponencial do resultado de uma variável aleatória normal padrão. 