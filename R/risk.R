risk <-
function(x, alpha = c(0.05), beta = 1)
{
 EL = VAR = StD = AbD = SeD = E_StD = E_AbD = E_SeD = VaR = ES = EVaR = ENT = SD = SDR = ML = rep(0, length(alpha))
 expect <- function(x, alpha)
 {
  int <- function(e)
  {
   ind = ifelse(x < e, 1, 0)
   sum(abs(alpha - ind) * ((x - e) ^ 2))
  }
  optimize(int, c(-20, 20)) $ minimum
 }
 for (i in 1 : length(alpha))
  {
   EL[i] = -mean(x)
   VAR[i] = var(x)
   StD[i] = sd(x)
   AbD[i] = mean(abs(x - mean(x)))
   SeD[i] = sqrt(mean(ifelse(x < mean(x), (x - mean(x))^2, 0)))
   E_StD[i] = EL[i] + beta * StD[i]
   E_AbD[i] = EL[i] + beta * AbD[i]
   E_SeD[i] = EL[i] + beta * SeD[i]
   VaR[i] = -quantile(x, alpha[i])
   ES[i] = -mean(x[x < -VaR[i]])
   EVaR[i] = -expect(x, alpha[i])
   ENT[i] = (1 / beta) * log(mean(exp(-beta * x)))
   SD[i] = sqrt(mean(ifelse(x < -ES[i], (x - (-ES[i]))^2, 0)))
   SDR[i] = ES[i] + beta * SD[i]
   ML [i] = -min(x)
  }
 ans <- rbind(EL, VAR, StD, AbD, SeD, E_StD, E_AbD, E_SeD, VaR, ES, EVaR, ENT, SD, SDR, ML)
 colnames(ans) <- paste(round(100*alpha, 2), "%", sep="")
 return(ans)
}
