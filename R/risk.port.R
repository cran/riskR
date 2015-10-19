risk.port <-
function(x, alpha = c(0.05), beta = 1)
{
 m = dim(x)[2]
 P = array(rep(0, 15 * length(alpha) * dim(x)[2]), dim = c(15, length(alpha), dim(x)[2]))
  for (j in 1 : 15)
 {
  for (k in 1 : length(alpha))
  { 
   f <- function(w = rep(1 / m, m))
   {
    risk(as.vector(abs(w)%*%t(x)), alpha, beta)[j, k]
   }
   W = optim(rep(1 / m, m), f) $ par
   P[j,k,] = abs(W) / sum(abs(W)) 
  }
 }
 colnames(P) <- paste(round(100*alpha, 2), "%", sep="")
 rownames(P) <- c("EL", "VAR", "StD", "AbD", "SeD", "E_StD", "E_AbD", "E_SeD", "VaR", "ES", "EVaR", "ENT", "SD", "SDR", "ML")
 return(P)
}
