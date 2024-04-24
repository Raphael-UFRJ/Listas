# Instalar e carregar os pacotes necess??rios
install.packages(c("igraph", "flextable"))
library(igraph)
library(flextable)

# Fun????o para criar o grafo de preced??ncia
criar_grafo_precedencia <- function(dados) {
  # Criar grafo de preced??ncia
  grafo <- graph_from_data_frame(dados[, c("Pred","DMin", "Atividade")], directed = TRUE)
  
  # Plotar o grafo
  plot(grafo, main = "Grafo de Preced??ncia do Projeto", vertex.label = paste(dados$Atividade, dados$Descri????o),
       edge.label = dados$Descri????o)
  
  return(grafo)
}

# Fun????o para identificar as atividades iniciais e finais
identificar_atividades_iniciais_finais <- function(grafo) {
  # Atividades iniciais (sem predecessores)
  atividades_iniciais <- V(grafo)[degree(grafo, mode = "in") == 0]
  
  # Atividades finais (sem sucessores)
  atividades_finais <- V(grafo)[degree(grafo, mode = "out") == 0 & degree(grafo, mode = "in") != 0]
  
  return(list(atividades_iniciais = atividades_iniciais, atividades_finais = atividades_finais))
}

# Carregar os dados
dados <- read.csv(text = "Atividade,Descri????o,Pred,DMin,DMp,DMax
1,Remove refractory material,7.15,4,5,10
2,Repair inner air casing,1,1,5,14
3,Repair outer air casing,2,1,5,10
4,Repair under boiler,,14,27,35
5,Rebrick,2.17,5,6,14
6,Chemically ckean,11.17,4,6,8
7,Remove air registers,,0,0.5,3
8,Install plastic refractory,10.19,0.5,0.5,1
9,Repair air register assemblies,7,10,10,20
10,Install air registers,5.9,0.5,1,3
11,Rag for chemical cleaning,15,5,6,7
12,Remove drum internals,15,0.5,0.5,3
13,Repair drum internals,12,5,12,18
14,Install drum internals,13.18,1,1.5,3
15,Initial hydrostatic test,,1,2,5
16,Exploratory block,1.12,7,8,18
17,Retube and poll,16,4,6,12
18,Preliminary hydrostatic test,6,0,0,14
19,final hydrostatic test,14,0,0,3")

# Passo 1: Criar o grafo de preced??ncia
grafo_precedencia <- criar_grafo_precedencia(dados)

# Passo 2: Identificar as atividades iniciais e finais
atividades <- identificar_atividades_iniciais_finais(grafo_precedencia)

# Imprimir as atividades iniciais e finais
cat("Atividades iniciais:\n")
for (v in atividades$atividades_iniciais) {
  cat(dados$Atividade[v], "\n")
}
cat("\nAtividades finais:\n")
for (v in atividades$atividades_finais) {
  cat(dados$Atividade[v],"\n")
}