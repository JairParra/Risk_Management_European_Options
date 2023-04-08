f_normal_copula_pdf <- function(u, mu, Sigma) {
  # Compute the PDF of the copula of the normal distribution at the 
  # generic point u in the unit hypercube
  # INPUTS
  #  u     : [vector] (J x 1) grade
  #  Mu    : [vector] (N x 1) mean
  #  Sigma : [matrix] (N x N) covariance matrix
  # OUTPUTS
  #  F_U   : [vector] (J x 1) PDF values
  
  N <- length(u) 
  m <- length(mu)     # number of assets
  s <- sqrt(diag(Sigma)) 
  
  x <- qnorm(p = u, mean = mu, sd = s)
  
  Numerator <- (2 * pi)^(-N/2) * ((det(Sigma))^(-0.5)) * exp(-0.5 * (x - mu) %*% pracma::mldivide(A = Sigma, B = (x - mu)))
                                                              
  fs <- dnorm(x, mu, s)
  Denominator <- prod(fs)
  
  F_U <- pracma::mrdivide(Numerator, Denominator)
  
  F_U
}
