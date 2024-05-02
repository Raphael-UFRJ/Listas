funcCpm <- function(n = 9,
                    d = c(0, 2, 6, 4, 3, 5, 4, 2, 0),
                    Suc = list(c(2, 3, 4), c(5, 7), 8, 6, 8, 8, 9, 9, 0),
                    Pre = list(0, 1, 1, 1, 2, 4, 2, c(3, 5, 6), c(7, 8))) {
  # Inicializar vetores
  est <- rep(0, n)
  eft <- rep(0, n)
  lst <- rep(0, n)
  lft <- rep(0, n)
  
  # Forward pass
  for (i in 1:n) {
    eft[i] <- est[i] + d[i]
    if (length(Suc[[i]]) > 0) {
      for (j in Suc[[i]]) {
        est[j] <- max(est[j], eft[i])
      }
    }
  }
  
  # Backward pass
  #lft[n] <- eft[n]
  #for (i in n:1) {
    #lst[i] <- lft[i] - d[i]
    #if (length(Pre[[i]]) > 0) {
      #for (j in Pre[[i]]) {
     #   lft[j] <- min(lft[j], lst[i])
    #  }
   # }
  #}
  
  # Backward pass
  lft[n] <- eft[n]
  for (i in (n - 1):1) { # Correção na iteração do loop
    lst[i] <- lft[i] - d[i]
    if (length(Suc[[i]]) > 0) {
      lft[i] <- min(lft[Suc[[i]]])
    } else {
      lft[i] <- eft[i]
    }
  }
  
  
  
  # Calculate slack times
  slack <- lft - eft
  
  # Return results
  list(est = est, eft = eft, lst = lst, lft = lft, slack = slack)
}

