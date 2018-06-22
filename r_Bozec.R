# script to test the effect of rugosity
# from atcoral.c routine Calculate_Rugosity(), method Bozec


# this would basically be looped over all coral species. All coral species contribute to both pa_reef and sa_reef. And all sponges too now, in different
# ways as parameterized

# one loop iteration, i.e. one species, looks like such 

colony_diam <- c(15, 30, 60) # cm, cohort-specific average diameter of a colony of a species
Max_diam <- max(colony_diam) # m, max diameter for a colony of a species. This is calculated in atbiolsetup.c and it corresponds to the largest cohort-specific diameter for a given species
small_num <- 0.000001 # to prevent dividing by 0 later on
cover <- 0.9 # proportional cover of this particular species in the current box
KH <- 0.1 # constant to obtain coral height, species-specific for coral groups IT DOES NOT MAKE ANY SENSE
rugosity_const <- 0.88 # 0.88 in Bozec et al. (2015)

# get areas 

pa_reef <- cover * Max_diam # planar area reef (flat)
sa_reef_vector <- rep(NA, length(colony_diam)) # surface area reef (relief)

for (i in 1:length(colony_diam)) { # this equates the number of cohorts
  
  colony_height <- colony_diam[i] * KH
  
  cdiam <- colony_diam[i]
  
  if (cdiam > Max_diam * cover) { # this is where the cover of the group limits the paraboloid's area. I'm not convinced. Why not scale that by the cover?
    cdiam <- Max_diam * cover
  }
  
  # sa_reef_vector[i] <- 8*pi/3 * (((colony_diam[i]^2) / 16 * colony_height)^2) * (((((16*colony_height*colony_height)/(colony_diam^2))+1)^0.66)-1) # this is Bozec's equation
  
  step1 <- cdiam^2 / (16.0 * colony_height)
  step2 <- ((16.0 * colony_height^2 / cdiam^2) + 1.0)^0.66 - 1
  step3 <- (8.0 * 3.141592654 / 3.0) * step1 * step1 * step2
  sa_reef_vector[i] <- step3
  
}

sa_reef <- sum(sa_reef_vector)

# Get reef index (surface-area-index)

reef_index <- sa_reef / (pa_reef + small_num)

# Final rugosity relationship from reef_index - note will be directly setting the rugosity not handling it as a flux

ans <- rugosity_const * reef_index

ans # this tends to be huge, probably because of mishandling of pa. Needs more thought I think

# rugosity has to be 1 > ans > 5









# refuge bit, to understand what happens here

RugCover_Coefft <- 1.4613
RugCover_Const <- 0.0475
RugCover_Cap <- 4
RugCover_scalar <- 0.8 # this is species-specific FOR FISH
LocalRugosity <- ans # bound between 1 (flat) and 5, highest for branching coral. this is parameters
step1 <- RugCover_Coefft * log(LocalRugosity) + RugCover_Const
hab_scalar = min(RugCover_Cap, RugCover_scalar / step1) # or Rscalar * step1?

# test for rugosity between 1 and 5

rug_constrained <- seq(1,5,.1)
hab_scalar_constrained <- rep(NA, length(rug_constrained))

for (i in 1:length(rug_constrained)) {
  
  step1 <- RugCover_Coefft * log(rug_constrained[i]) + RugCover_Const
  hab_scalar_constrained[i] = min(RugCover_Cap, RugCover_scalar / step1) # or Rscalar * step1?
  
}

df <- data.frame(rug_constrained, hab_scalar_constrained)
plot(rug_constrained, hab_scalar_constrained)

# hab_scalar is probably constrained to 0-1 too




