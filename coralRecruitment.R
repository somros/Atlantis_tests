# recruitment, as modelled for corals

KDENR <- 0 # external recruitment

CrecA <- 1 # max recruits attainable (with B)
CrecB <- 1 # max recruits attainable (with A), and min recruits for SB = 0 
CrecC <- 0.0001 # slope

SB <- 0:10000 # spawning biomass

Rec <- KDENR + CrecA*(CrecB - exp(-CrecC*SB)) # case 16

plot(SB, Rec)

# coral growth and transition to next age class

sp_AgeClassSize <- 12 # number of years in a cohort. I think. It's between 12-30 for Gladstone Harbour and GBR, I wonder how it could be 1 for Guam with this routine
max_accel_transition <- 0.005 # note that these are species-specific
accel_Trans_A <- 2
accel_Trans_B <- 1.5

trans_prob <- rep(NA, sp_AgeClassSize-2)

for (i in (sp_AgeClassSize-2): 1) { # why ageClassSize - 2? 
  trans_prob[i] <- max_accel_transition/ (1 + exp(-accel_Trans_A * (i - accel_Trans_B)))
}

df <- data.frame(((sp_AgeClassSize-2): 1), trans_prob)

plot(((sp_AgeClassSize-2): 1), trans_prob)

