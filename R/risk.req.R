risk.req <-
function(x, M = 10^6, T = 1, alpha = c(0.05), beta = 1)
{
 R <- risk(x, alpha, beta) * M * sqrt(T)
 colnames(R) <- paste(round(100*alpha, 2), "%", sep="")
 rownames(R) <- c("EL", "VAR", "StD", "AbD", "SeD", "E_StD", "E_AbD", "E_SeD", "VaR", "ES", "EVaR", "ENT", "SD", "SDR", "ML")
 return(R)
}
