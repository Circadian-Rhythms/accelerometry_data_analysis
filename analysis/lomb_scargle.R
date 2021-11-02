library(Rmpfr)

ls_periodograms <- function(x, y, freqs, centered = T, normalize = T, precision = T){
  # R.H.D. Townsend, “Fast calculation of the Lomb-Scargle periodogram using 
  # graphics processing units.”, The Astrophysical Journal Supplement Series, 
  # vol 191, pp. 247-253, 2010 - Scipy
  m_x <- matrix(rep(x, length(freqs) * length(x), times = length(freqs)), ncol = length(freqs))
  m_freqs <- diag(freqs)
  m <- m_x %*% m_freqs

  if (precision) {
    y <- mpfr(y, 128)
    c <- mpfr(apply(m, MARGIN = 2, cos), 128)
    s <- mpfr(apply(m, MARGIN = 2, sin), 128)
  } else{
    c <- apply(m, MARGIN = 2, cos)
    s <- apply(m, MARGIN = 2, sin)
  }
  
  y_raw <- y
  if (centered) {
    y <- y - mean(y)
  }
  
  xc <- colSums(y * c)
  xs <- colSums(y * s)
  cc <- colSums(c^2)
  ss <- colSums(s^2)
  cs <- colSums(c * s)
  
  tau <- 2 * cs / (cc - ss)
  
  c_tau <- cos(freqs * tau)
  s_tau <- sin(freqs * tau)
  
  pgram <- 0.5 * (
    (c_tau * xc + s_tau * xs)^2 / (c_tau^2 * cc + 2 * c_tau * s_tau * cs + s_tau^2 * ss) +
      (c_tau * xs - s_tau * xc)^2 / (c_tau^2 * ss - 2 * c_tau * s_tau * cs + s_tau^2 * cc))
  
  if (normalize) {
    pgram <- 2 * pgram / sum(y_raw^2)
  }
  
  if (precision) {
    pgram <- as.double(pgram)
  }
  return(pgram)
}
