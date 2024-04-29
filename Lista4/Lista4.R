# Tabela de dados corrigida
projeto <- data.frame(
  Atividade = 1:19,
  Descrição = c("Remove refractory material", "Repair inner air casing", "Repair outer air casing",
                "Repair under boiler", "Rebrick", "Chemically clean", "Remove air registers",
                "Install plastic refractory", "Repair air register assemblies", "Install air registers",
                "Rag for chemical cleaning", "Remove drum internals", "Repair drum internals",
                "Install drum internals", "Initial hydrostatic test", "Exploratory block",
                "Retube and poll", "Preliminary hydrostatic test", "Final hydrostatic test"),
  Pred = c("7", "1", "2", "-", "2", "11", "-", "10", "7", "5", "15", "15", "12", "13", "-", "1", "16", "6", "14"),
  DMin = c(4, 1, 1, 14, 5, 4, 0, 0.5, 10, 0.5, 5, 0.5, 5, 1, 1, 7, 4, 0, 0),
  DMp = c(5, 5, 5, 27, 6, 6, 0.5, 0.5, 10, 1, 6, 0.5, 12, 1.5, 2, 8, 6, 0, 0),
  DMax = c(10, 14, 10, 35, 14, 8, 3, 1, 20, 3, 7, 3, 18, 3, 5, 18, 12, 14, 3)
)

# Carregar pacote igraph
library(igraph)

# Função para desenhar o grafo de precedência do projeto
q1 <- function() {
  # Criar o grafo
  graf <- graph_from_data_frame(projeto[, c("Pred", "Atividade")], directed = TRUE)
  
  # Adicionar nó inicial "I" e conectar às atividades iniciais com predecessores "-"
  graf <- add_vertices(graf, 1, name = "I")
  atividades_iniciais <- projeto$Atividade[projeto$Pred == "-"]
  for (atividade in atividades_iniciais) {
    graf <- add_edges(graf, c("I", as.character(atividade)))
  }
  
  # Remover os nós iniciais redundantes
  graf <- delete_vertices(graf, which(V(graf)$name == "-"))
  
  # Plotar o grafo com ajustes para evitar sobreposição de nós
  plot(graf, main = "Grafo de Precedência do Projeto", vertex.label = V(graf)$name, vertex.label.cex = 0.8,
       layout = layout_with_dh, vertex.size = 15, edge.arrow.size = 0.5, margin = 0.1)
  # o layout layout_with_dh que posiciona os nós de forma mais espaçada horizontalmente. 
  # Também mudei o tamanho dos nós (vertex.size) e o tamanho das setas das arestas (edge.arrow.size) para melhorar a visualização.
  # E defini uma margem (margin) para deixar um espaço em branco ao redor do grafo.
}


# Função para identificar as atividades iniciais e finais
q2 <- function() {
  # Identificar atividades iniciais
  atividades_iniciais <- projeto$Atividade[projeto$Pred == "-"]
  
  # Identificar atividades finais
  atividades_finais <- projeto$Atividade[!(projeto$Atividade %in% projeto$Pred)]
  
  cat("Atividades Iniciais:", atividades_iniciais, "\n")
  cat("Atividades Finais:", atividades_finais, "\n")
}

# Chamadas de função para testar cada parte do exercício
q1()
q2()


# Função para gerar um novo grafo de precedência com uma atividade inicial e uma final
q3 <- function() {
  
}


# Função para calcular aproximações empíricas para o risco de prazo da obra
q4 <- function() {
}

# Função para estimar as probabilidades das atividades pertencerem ao caminho crítico
q5 <- function() {
}

# Função para calcular a distribuição de probabilidade da data de início mais cedo para todas as atividades
q6 <- function() {
}

# Função para gerar um cronograma com probabilidade de 85% de ser cumprido
q7 <- function() {
}

# Função para desenhar um diagrama de Gantt para o agendamento
q8 <- function() {
}

# Chamadas de função para testar cada parte do exercício
q3()
q4()
q5()
q6()
q7()
q8()
