risk.roll <-
function(x,  N = length(x) - 1, alpha = c(0.05), beta = 1)
{
 F = length(x) - N
 R = array(rep(0, 15 * length(alpha) * F), dim = c(15, length(alpha), F))
 for (j in 1 : F)
 {
  R[,,j] = risk(x[j : (N + j - 1)], alpha, beta)   
 }
 colnames(R) <- paste(round(100*alpha, 2), "%", sep="")
 rownames(R) <- c("EL", "VAR", "StD", "AbD", "SeD", "E_StD", "E_AbD", "E_SeD", "VaR", "ES", "EVaR", "ENT", "SD", "SDR", "ML")
 return(R)
}
