calc_auroc2 <- function(nR_S1, nR_S2, Nratings) {
  flipped_nR_S1 <- rev(nR_S1)
  flipped_nR_S2 <- rev(nR_S2)
  
  S1_H2 <- numeric(Nratings)
  S2_H2 <- numeric(Nratings)
  S1_FA2 <- numeric(Nratings)
  S2_FA2 <- numeric(Nratings)
  
  for (c in 1:Nratings) {
    S1_H2[c] <- nR_S1[c] + 0.5
    S2_H2[c] <- flipped_nR_S2[c] + 0.5
    S1_FA2[c] <- flipped_nR_S1[c] + 0.5
    S2_FA2[c] <- nR_S2[c] + 0.5
  }
  
  H2 <- S1_H2 + S2_H2
  FA2 <- S1_FA2 + S2_FA2
  
  H2 <- H2 / sum(H2)
  FA2 <- FA2 / sum(FA2)
  
  cum_H2 <- c(0, cumsum(H2))
  cum_FA2 <- c(0, cumsum(FA2))
  
  k <- numeric(Nratings)
  
  for (c in 1:Nratings) {
    k[c] <- (cum_H2[c + 1] - cum_FA2[c])^2 - (cum_H2[c] - cum_FA2[c + 1])^2
  }
  
  auroc2 <- 0.5 + 0.25 * sum(k)
  
  return(auroc2)
}
