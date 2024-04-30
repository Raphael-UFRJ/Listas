# Carregar pacote igraph
library(igraph)

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

# Função para desenhar o grafo de precedência do projeto
q1 <- function() {
  # Definir os elos corretamente
  elos <- c(7, 1, 1, 15, 7, 1, 15, 1, 2, 1, 2, 5, 2, 4, 8, 2, 9, 5, 17, 5, 11, 6, 17, 6, 10, 8, 19, 8, 9, 10, 9, 10, 13, 14, 18, 14, 1, 16, 12, 13, 12, 16, 4, 4, 3, 8, 8, 8)
  
  # Criar o grafo com os elos
  g <- make_graph(edges = elos, directed = TRUE)
  
  # Plotar o grafo usando tkplot
  tkplot(g, main = "Grafo de Precedência do Projeto")
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
