f_student_copula_pdf <- function (u, mu, Sigma, nu) {
  # Pdf of the copula of the Student t distribution at the generic point u in the unit hypercube
  # INPUTS
  #   u     : [vector] (J x 1) grade
  #   Mu    : [vector] (N x 1) mean
  #   Sigma : [matrix] (N x N) scatter
  #   nu    : [scalar] degrees of freedom
  # OUTPUTS
  #   F_U   : [vector] (J x 1) PDF
  
  N <- length(u)
  s <- sqrt(diag(Sigma))
  
  x <- mu + s * qt(p = u, df =  nu)
  
  z2 <- (x - mu) %*% pracma::mldivide(Sigma, (x - mu))
  K  <- (nu * pi)^(-N/2) * gamma((nu + N) / 2) / gamma(nu / 2) * ((det(Sigma))^(-.5))
  Numerator <- K * (1 + z2 / nu)^(-(nu + N) / 2)
  
  fs <- dt((x - mu)/s, nu) / s
  Denominator <- prod(fs)
  
  F_U <- Numerator/Denominator
  
  F_U
}


