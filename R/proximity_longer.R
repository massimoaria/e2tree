

idImpurity2 <- function(y,index,S){
  y <- y[index,index]
  lab <- row.names(y)
  S <- S[index,]
  #Matrix::diag(dis) <- -1

  y[(lower.tri(y, diag=TRUE))] <- -1
  D <- y %>% as.matrix() %>%
    as.data.frame() %>%
    mutate(obs_row=colnames(.)) %>%
    pivot_longer(cols = 1:(ncol(.)-1), names_to = "obs_col") %>%
    dplyr::filter(.data$value >-1)

  ind <- seq_len(ncol(S))

  n <- length(index)
  tab <- colSums(S)
  ind <- ind[(tab>1 & tab < (n-1))]
  imp=rep(Inf,ncol(S))

  for (i in ind){
    #print(i)
    s2 <- S[,i]
    s <- rep(-1,nrow(D))
    s[D$obs_row %in% lab[s2==1] & D$obs_col %in% lab[s2==1]] <- 1
    s[D$obs_row %in% lab[s2==0] & D$obs_col %in% lab[s2==0]] <- 0
    #n1 <- sum(s2)
    #n0 <- n-n1
    #items <- c(n0*(n0-1),n1*(n1-1))/2
    # Tasso ben classificati in tutti i nodi terminali dell'i-esimo albero
    R <- cbind(D,s=s) %>%
      filter(.data$s>-1) %>%
      group_by(s) %>%
      summarize(m=mean(.data$value))
    imp[i] <- sum(R$m*c(n-sum(s2),sum(s2)))/n

  }

  names(imp)=colnames(S)
  return(imp)
}
