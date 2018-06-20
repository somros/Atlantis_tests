# sedimentation: calculation of S

Ksmother_coefft <- 0.054 # this is as in Guam .prm file. Even 0 sed will cut growth
Ksmother_const <- 0.4622
sed_level <- 0:1000 # not sure of the units over here, if a tracer probably still mg N m-3

# code as in Coral_Consumer_Activities()

step1 = -1.0 * Ksmother_coefft * log(sed_level) - Ksmother_const

smother_effect = 1.0 + step1

plot(sed_level, smother_effect)

# requires careful parametrisation
