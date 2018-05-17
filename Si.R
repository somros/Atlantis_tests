# some tests for deltaSi scalar for heterotrophic growth

DSi <- 0:800 # in muM
Vmax <- 0.135
Km <- 74.5
U <- DSi * Vmax/(Km + DSi)
plot(DSi, U)
abline(v = Km, col = "red")

# environmental concentrations of Si

# values of DSi for GBR have wide range. pick value
# of 100 mug/L as per http://www.gbrmpa.gov.au/__data/assets/pdf_file/0011/16778/Inshore-Water-Quality-Monitoring-Report_2011.pdf

# molar mass of Si(OH)4

Mm <- 28+(17*4)

# molar concentration

CDSi <- 100/Mm

abline(v = CDSi, col = "blue")


