tt_moran <- function(z, type, W, nsim) {
  zz <- c(crossprod(z))
  z_tr <- z[type=="train"]
  z_te <- z[type=="test"]
  W_trtr <- W_tt[type=="train", type=="train"]
  W_trte <- W_tt[type=="train", type=="test"]
  W_tetr <- W_tt[type=="test", type=="train"]
  W_tete <- W_tt[type=="test", type=="test"]
  r4 <- function(z_tr, z_te, W_trtr, W_trte, W_tetr, W_tete, zz) {
    res <- numeric(4)
    res[1] <- as.vector(crossprod(z_tr, W_trtr %*% z_tr))/zz
    res[2] <- as.vector(crossprod(z_tr, W_trte %*% z_te))/zz
    res[3] <- as.vector(crossprod(z_te, W_tetr %*% z_tr))/zz
    res[4] <- as.vector(crossprod(z_te, W_tete %*% z_te))/zz
    names(res) <- c("trtr", "trte", "tetr", "tete")
    res
  }
  moran_tt <- r4(z_tr, z_te, W_trtr, W_trte, W_tetr, W_tete, zz)
  if (nsim > 0) {
    sims <- matrix(NA, ncol=4, nrow=nsim)
    for (i in 1:nsim) {
      z <- sample(z)
      z_tr <- z[type=="train"]
      z_te <- z[type=="test"]
      sims[i,] <- r4(z_tr, z_te, W_trtr, W_trte, W_tetr, W_tete, zz)
    }
    sims1 <- rbind(sims, moran_tt)
    xrank <- integer(4)
    pval <- numeric(4)
    for (i in 1:4) {
      rankres <- rank(sims1[,i])
      xrank[i] <- as.integer(rankres[nrow(sims1)])
      diff <- nsim - xrank[i]
      diff <- ifelse(diff > 0, diff, 0)
      pval[i] <- punif(abs(xrank[i] - (nsim+1)/2)/(nsim + 1), 0, 0.5,
       lower.tail=FALSE)
    }
    attr(moran_tt, "sims") <- sims
    attr(moran_tt, "xrank") <- xrank
    attr(moran_tt, "pval") <- pval
  }
  moran_tt
}
