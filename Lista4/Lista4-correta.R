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
  Pred = c("7,15", "1", "2", "-", "2,17", "11,17", "-", "10,19", "7", "5,9", "15", "15", "12", "13,18", "-", "1,12", "16", "6", "14"),
  DMin = c(4, 1, 1, 14, 5, 4, 0, 0.5, 10, 0.5, 5, 0.5, 5, 1, 1, 7, 4, 0, 0),
  DMp = c(5, 5, 5, 27, 6, 6, 0.5, 0.5, 10, 1, 6, 0.5, 12, 1.5, 2, 8, 6, 0, 0),
  DMax = c(10, 14, 10, 35, 14, 8, 3, 1, 20, 3, 7, 3, 18, 3, 5, 18, 12, 14, 3)
)

# Função para desenhar o grafo de precedência do projeto
q1 <- function(projeto) {
  # Converter os predecessores em uma lista de elos
  elos <- c()
  for (i in 1:nrow(projeto)) {
    preds <- unlist(strsplit(as.character(projeto$Pred[i]), ","))
    for (pred in preds) {
      if (pred != "-") {
        elos <- rbind(elos, c(as.numeric(pred), projeto$Atividade[i]))
      }
    }
  }
  
  # Criar o grafo com os elos
  g <- graph_from_edgelist(as.matrix(elos), directed = TRUE)
  
  # Identificar atividades iniciais e finais
  atividades_iniciais <- projeto$Atividade[projeto$Pred == "-"]
  preds <- unlist(strsplit(paste(projeto$Pred[projeto$Pred != "-"], collapse = ","), ","))
  atividades_finais <- projeto$Atividade[!projeto$Atividade %in% preds]
  
  # Definir cores para os vértices - NÃO FOI NEM COM REZA BRABA
  # vertex_colors <- rep("white", vcount(g))
  # names(vertex_colors) <- V(g)$name
  # vertex_colors[names(vertex_colors) %in% atividades_iniciais] <- "green"
  # vertex_colors[names(vertex_colors) %in% atividades_finais] <- "red"
  
  
  # Plotar o grafo
  tkplot(g, main = "Grafo de Precedência do Projeto", vertex.color = "white")
}

# Função para identificar as atividades iniciais e finais
q2 <- function(projeto) {
  # Identificar atividades iniciais (sem predecessores)
  atividades_iniciais <- projeto$Atividade[projeto$Pred == "-"]
  
  # Identificar atividades finais (não aparecem como predecessores)
  preds <- unlist(strsplit(paste(projeto$Pred[projeto$Pred != "-"], collapse = ","), ","))
  atividades_finais <- projeto$Atividade[!projeto$Atividade %in% preds]
  
  cat("Atividades Iniciais:", atividades_iniciais, "\n")
  cat("Atividades Finais:", atividades_finais, "\n")
}

# Chamada de função para testar
q1(projeto)
q2(projeto)
