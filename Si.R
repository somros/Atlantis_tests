# some tests for deltaSi scalar for heterotrophic growth

DSi <- 0:800 # (muMol * l-1)
Vmax <- 0.135 # maximum velocity, (muMol * h-1)
Km <- 74.5 # half-saturation (muMol)
U <- DSi * Vmax/(Km + DSi) # uptake: muMol Si * h-1 * ml(sponge)-1


# environmental concentrations of Si

# values of DSi for GBR have wide range. pick value
# of 100 mug/L as per http://www.gbrmpa.gov.au/__data/assets/pdf_file/0011/16778/Inshore-Water-Quality-Monitoring-Report_2011.pdf

# molar mass of Si(OH)4

Molar.mass <- 28+(17*4)

# molar concentration

CDSi <- 100/Molar.mass

abline(v = CDSi, col = "blue") # at present concentrations uptake is very limited

## introduce a new scalar to mum for Si-dependent benthic groups, delta_Si

# it has to be 1 for "normal" concentrations, decline according to Uptake

# requires 2 new parameters: Vmax_SiUptake_XXX, Km_SiUptake_XXX (it may be a model parameter as we do not have species-specific info)

# same kinetics, but 1 for "normal silica", say 75 muM here, 0 for 0 silica

Vmax_delta <- 2 # this achieves a delta = 1 for the chosen maximum concentration
Km_delta <- 75 # This is the chosen maximum concentration
delta_Si <- (DSi * Vmax_delta) / (Km_delta + DSi) # the risk here is that DSi can become very small very quickly


par(mfrow = c(1,2))
plot(DSi, U)
abline(v = Km, col = "red")
plot(DSi, delta_Si)
abline(v = Km_delta, col = "red")

# may use a piece-wise, where we pick a best concentration (relatively low) and we have 1 for anything above it.














