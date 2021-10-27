cosinor <- function(acc) {
  Y <- acc$data$acceleration
  N <- length(Y)
  x <- sapply(acc$data$date_time, 
              function(x) {cos(as.integer(x)*2*pi/(60*60*24))})
  z <- sapply(acc$data$date_time, 
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
}