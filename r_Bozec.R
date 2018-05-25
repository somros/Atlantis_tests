# script to test the effect of rugosity

# from atcoral.c routine Calculate_Rugosity(), method Bozec

Diam <- 1.2 # m
KH <- 0.5 # constant to obtain coral height
Ch <- Diam * KH

pa_reef <- 0 # see notes

sa_reef <- 8*pi/3 * (((Diam^2) / 16 * Ch)^2) * (((((16*Ch)/(Diam^2))+1)^0.66)-1)

step1 <- Diam^2 / (16.0 * Ch)
step2 <- ((16.0 * Ch^2 / Diam^2) + 1.0)^0.66 - 1
step3 <- (8.0 * 3.141592654 / 3.0) * step1 * step1 * step2


########### keep going from here