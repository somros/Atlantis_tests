# Script to test thermal sensitivities of Atlantis GBR groups

# * Equation from Gary G - Tdep_di=log(2)*0.851*(1.066.^T_i).*exp(-((abs(T_i-T_opt_nitzschia)).^3)./1000)

# Tcorr = ln(2)*phiA*ConsB^(T)*exp(-phiC*(|T-Topt|^Cons)/phiCorr))

# see User Guide I pag. 195 for details

# making biological sense of these will be hard, need to check G Griffith's thesis for details.
# keep in mind that translating any study to these will be very difficult, i.e. this will be hard to parameterise
# in general, Tcorr needs to be able of modifying variables the right way

library(ggplot2)

phiA <- 0.551 # this is species-specific (temp_coefftA_XXX) 
ConsB <- 1.035 # this is global(temp_coefftB) # these 2 together adjust the magnitude of Tcorr
phiC <- 1.5 # this is global (temp_coefftC) # this adjusts the steepness of the decline 
Topt <- 28.5 # this is species-specific (q10_optimal_temp_XXX)
Cons <- 3 # this is global (temp_exp) this decides the position and the width of the hump, the higher the narrower the thermal window
phiCorr <- 2000 # this is species-specific (q10_correction_XXX) andd it is the width of the hump, the higher the broader

Temp <- 0:40

Tcorr <- log(2) * phiA * ConsB^(Temp) * exp(-phiC*(((abs(Temp-Topt))^Cons)/phiCorr))

plot(Temp, Tcorr, "l")
abline(v = 28, col = "red")
abline(h = 1, col = "blue")

# comes out slightly different from the manual, Tcorr is higher consistently here, not sure why as the 
# method seems the same. seems like log10 was used for that plot in the manual

