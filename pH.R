# calculate pHCorr according to the selected method
# playground

# monod

pH <- seq(1,12,0.1)

pH_const_A.monod <- 6 # this is meant as minimum value under which the process does not happen
pH_const_B.monod <- 10
KN_pH <- 25

pHCorr.monod <- pH_const_B.monod * (pH-pH_const_A.monod)/((KN_pH - pH_const_A.monod)+(pH - pH_const_A.monod))

plot(pH, pHCorr.monod, type = "l", col = "blue")
abline(v = 8.1, col = "red")
abline(h = 1, col = "red")
abline(h = 0)

# non-linear NOTE: ph_const_A and pH_const_B do not seem to mean the same across all methods

pH_const_A.nl <- 1.0
pH_const_B.nl <- 1.18
pH_correction <- 15
opt_pH <- 8.1

step1 <- log10(2) * pH_const_A.nl * pH_const_B.nl^pH 
step2 <- exp(-1.0 * (abs(pH - opt_pH)^3.0) / pH_correction)
pHCorr.nl = step1 * step2

plot(pH, pHCorr.nl, type = "l", col = "blue")
abline(v = 8.1, col = "red")
abline(h = 1, col = "red")
abline(h = 0)

# linear

pH_const_A.lin <- 0
pH_const_B.lin <- 0.1234568 # 1/8.1

pHCorr.lin <- pH_const_A.lin + pH_const_B.lin * pH

plot(pH, pHCorr.lin, type = "l", col = "blue")
abline(v = 8.1, col = "red")
abline(h = 1, col = "red")
abline(h = 0)

# piece-wise (need to check this as the equation seems off)

pH_const_A.pw <- 0.2
pH_const_B.pw <- 1
pHmin <- 6
pHmax <- 8.1

pHCorr.pw <- rep(NA, length(pH))

for (i in 1:length(pH)) {
  if (pH[i] < pHmin) {
    pHCorr.pw[i] <- pH_const_A.pw
  } else if (pH[i] > pHmax) {
    pHCorr.pw[i] <- pH_const_B.pw
  } else {
    pHCorr.pw[i] <- pH_const_A.pw + ((pH[i]-pHmin)/(pHmax-pHmin)*(pH_const_B.pw - pH_const_A.pw))
  }
}

plot(pH, pHCorr.pw, type = "l", col = "blue")
abline(v = 8.1, col = "red")
abline(h = 1, col = "red")
abline(h = 0)

