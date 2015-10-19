risk.hedge <-
function(x, y, alpha = c(0.05), beta = 1)
{
 H = matrix(rep(0, 15 * length(alpha)), ncol = length(alpha))
 for (j in 1 : 15)
 {
  for (k in 1 : length(alpha))
  { 
   f <- function (h)
   {
    risk(x - h * y, alpha, beta)[j, k]
   }
   H[j,k] = optimize(f, c(-20, 20)) $ minimum
  }
 }
 colnames(H) <- paste(round(100*alpha, 2), "%", sep="")
 rownames(H) <- c("EL", "VAR", "StD", "AbD", "SeD", "E_StD", "E_AbD", "E_SeD", "VaR", "ES", "EVaR", "ENT", "SD", "SDR", "ML")
 return(H)
}
