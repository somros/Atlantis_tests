# sedimentation: calculation of S

Ksmother_coefft <- 1.031 # this is as in GBR .prm file. Asymptote to the curve
Ksmother_const <-  0.0603 # the complement to 1 of this is the first point of the curve basically
sed_level <- 0:1000 # not sure of the units over here, if a tracer probably still mg N m-3

# CAREFUL: CURRENT PARAMETRISATION IMPOSES A VERY STRONG SEDIMENT LIMITATION!

# code as in Coral_Consumer_Activities()

step1 = -1.0 * Ksmother_coefft * log(sed_level) - Ksmother_const

smother_effect = 1.0 + step1

plot(sed_level, smother_effect)

# requires careful parametrisation
