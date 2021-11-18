fit_cosinor <- function(acc) {
  Y <- acc$acceleration
  N <- length(Y)
  x <- sapply(acc$date_time, 
              function(x) {cos(as.integer(x)*2*pi/(60*60*24))})
  z <- sapply(acc$date_time, 
              function(z) {sin(as.integer(z)*2*pi/(60*60*24))})
  
  S = matrix(c(N, sum(x), sum(z),
               sum(x), sum(x ** 2), sum(x * z),
               sum(z), sum(x * z), sum(z ** 2)), 
             nrow = 3, byrow = TRUE)
  
  d <- matrix(c(sum(Y), sum(Y * x), sum(Y * z)), nrow = 3)
  u <- solve(S) %*% d
  
  A <- ((u[2]**2) + (u[3]**2)) ** (1/2)
  phi <- atan(-u[3]/u[2])

  c(u[1],A,phi)

  Y_hat <- list(u[1]+u[2]*x+u[3]*z)
  fitted_values = Y_hat
  
  
  c(u[1],A,phi, fitted_values)

}

test_cosinor_fit <- function(acc, Y_hat, sample_rate) {
  RSS <- sum((acc$acceleration - Y_hat) ** 2)
  
  SSPE <- acc %>%
    group_by(hour = hour(date_time), minute = minute(date_time), second = second(date_time)) %>%
    summarise(SSPE_component = get_SSPE_component(acceleration))
  SSPE <- sum(SSPE$SSPE_component)
  
  SSLOF <- RSS - SSPE
  N <- length(acc$acceleration)
  m <- (60*60*24)/sample_rate

  F_stat <- (SSLOF/(m-3))/(SSPE/(N-m))
  print(pf(F_stat, m-3, N-m))
  browser()
}

get_SSPE_component <- function(xs) {
  sum((xs-mean(xs)) ** 2)
}