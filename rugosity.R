# script to test the effect of rugosity
# from atcoral.c routine Calculate_Rugosity(), method Bozec


# this would basically be looped over all coral species. All coral species contribute to both pa_reef and sa_reef. 
# And all sponges too now, in different ways as parameterized

# one loop iteration, i.e. one species, looks like such 

colony_diam <- c(10, 20, 40) # cm, cohort-specific average diameter of a colony of a species
max_diam <- max(colony_diam) # m, max diameter for a colony of a species. This is calculated in atbiolsetup.c and it corresponds to the largest cohort-specific diameter for a given species
mean_diam <- mean(colony_diam)
small_num <- 0.000001 # to prevent dividing by 0 later on
cover <- 1 # proportional cover of this particular species in the current box
KH <- 0.1 # constant to obtain coral height, species-specific for coral groups IT DOES NOT MAKE ANY SENSE
rugosity_const <- 1 # 0.88 in Bozec et al. (2015)

# get areas 

# pa_reef <- cover * max_diam ## this is the original Atlantis code. As this sh9uld be a planar area occupied by the species in the 
# box, not sure about this notation. Conceptually it works, the bigger and the more the better, but as of scaling the paraboloid 
# surface areas later on it does not seem to do a great job

pa_reef <- cover * pi * (mean_diam/2)^2 # planar area reef (flat) THIS IS ONE OF THE ISSUES: this is no area this way, 
# and turning into the area of the circle has its own host of issues:
# we have to assume one diameter fits all, and this then has to go and divide surface areas of paraboloids of different diameter
# It might though be one step forward, but not good either as it does not account how much of each cohort we have actually in the box, 
# which is ultimately what makes the difference in terms of refuge space. 
# being able to do this calculation for each cohort and scale it to their cover may help ## check ##

sa_reef_vector <- rep(NA, length(colony_diam)) # surface area reef (relief)

# for diagnostics

s1v <- rep(NA, length(colony_diam))
s2v <- rep(NA, length(colony_diam))
s3v <- rep(NA, length(colony_diam))

for (i in 1:length(colony_diam)) { # this equates the number of cohorts
  
  colony_height <- colony_diam[i] * KH
  
  cdiam <- colony_diam[i]
  
  if (cdiam > max_diam * cover) { # this is where the cover of the group limits the paraboloid's area. I'm not convinced. Why not scale that by the cover?
    cdiam <- max_diam * cover
  }
  
  # sa_reef_vector[i] <- 8*pi/3 * (((colony_diam[i]^2) / 16 * colony_height)^2) * (((((16*colony_height*colony_height)/(colony_diam^2))+1)^0.66)-1) # this is Bozec's equation
  
  step1 <- cdiam^2 / (16.0 * colony_height)
  step2 <- (((16.0 * colony_height^2 / cdiam^2) + 1.0)^0.66) - 1
  step3 <- (8.0 * 3.141592654 / 3.0) * step1 * step1 * step2
  sa_reef_vector[i] <- step3 # elements of this vector are paraboloid surface areas
  
  # diagnostics
  s1v[i] <- step1
  s2v[i] <- step2
  s3v[i] <- step3
  
}

sa_reef <- sum(sa_reef_vector) # sum of all the paraboloid areas

# Get reef index (surface-area-index)

reef_index <- sa_reef / ((pa_reef + small_num) * length(colony_diam)) # this is equivalent of finding reef index for each and then taking mean

# Final rugosity relationship from reef_index - note will be directly setting the rugosity not handling it as a flux

ans <- rugosity_const * reef_index

ans 

# rugosity has to be 1 > ans > 5

# update 25/06/2018. Overall this model does not make any distinction between coral morphologies.
# We may evaluate if this really has any advantage over simpler models that treat 3D structures implicitly, as one
# thing I have learned from the 3D work is that small scale does matter, unfortunately


################################################################
################################################################
# Blackwood's model

# parameters

rugosity_inc <-  0.03 # this is a parameter in .prm, it is species-specific and it will depend on the time step. the more rugose, the higher
max_rugosity <- 3
min_rugosity <- 1
cover <- 0.56 # this is box-specific, species-specific and cohort-specific if I understand the function right
rug_erode <- 0.01
unbleached <- 0.95

# start from non-sheltering, i.e. massive. Needs some iterations to get to regime

time_steps <- 1:1000
local_rugosity <- rep(1, length(time_steps)) # this starts from 0, let's run some simulations


for (ts in 1:(length(time_steps)-1)) { # this must be looped across species. In theory the fact that it is weighted by the cover should guarantee it does not overshoot
  
  rug_growth <- rugosity_inc * (cover * unbleached) * (max_rugosity - local_rugosity[ts]) # this is modification as code seems wrong
  # this means that at each time step only live corals 
  
  # original would be: rug_growth <- rugosity_inc * unbleached * (max_rugosity - LocalRugosity) # but this fails to account for coral cover to my understanding
  
  rug_erosion <- rug_erode * (1.0 - cover * unbleached) * (local_rugosity[ts] - min_rugosity) # 
  
  # orignial here would have been: CRN_rug_erode <- CRN_rug_erode * (1.0 - bleached) * (LocalRugosity - min_rugosity) 
  # but this agains does not account for cover, and in fact has the opposite effect of scaling the erosion by the amount of live corals
  # Basically the issue in the code comes down to the proportions of bleached and unbleached corals not pointing to the amount of corals
  # note that the term (1 - C * unbleached) is meant to account for the coral cover, in that the more corals the less the effects of bioerosion
  # on rugosity will be, whereas for a very low cover we will have troubles with bioerosion
  
  # expand with sponges
  # rug_erosion <- rug_erode * (1.0 - cover * unbleached) * (local_rugosity[ts] - min_rugosity)
  
  delta_rug <- rug_growth - rug_erosion # this is the rugosity increase
  
  local_rugosity[ts+1] <- delta_rug + local_rugosity[ts] # this will need to be looped across all species as they all should contribute to rugosity!! 
  # Drawback o this method is that it does not really account for cohorts and their different contributions
  # Also how to calibrate sponges? rug_increase makes a difference for the max attainable growth too.
}


plot(time_steps, local_rugosity)

# this needs to be expanded with sponge bioerosion and it needs testing (follow rugosity tracer) 

# if we expand with dynamic tie to coral cover and to boring sponge abundance we may have a decent model but still takes testing





# refuge bit, to understand what happens here

RugCover_Coefft <- 1.4613
RugCover_Const <- 0.0475
RugCover_Cap <- 1 # watch these parameters, we may want to cap it to 1 for 0 cover, i.e. no refuge at all.
RugCover_scalar <- 0.8 # this is species-specific FOR FISH and it depends on how much a species DISLIKES rugosity as a refuge

step1 <- RugCover_Coefft * log(local_rugosity) + RugCover_Const

hab_scalar <- rep(NA, length(local_rugosity)) # this is called hab_scalar in atdemography.c and refuge_status in atvertprocesses

for (i in 1:length(local_rugosity)) {
  hab_scalar[i] = min(RugCover_Cap, RugCover_scalar / step1[i])
}

plot(local_rugosity, hab_scalar)

# test for rugosity between 1 and 5





#######################################################################################################
# Blackwood's model. Expanded with bit on bioerosion, from sponges and grazers as main borers. Is this accurate?
# also what are the grazers? Fish and urchins, how to tease apart? May be too specific but let us try.

# parameters

rugosity_inc <-  0.003 # this is a parameter in .prm, it is species-specific and it will depend on the time step. the more rugose, the higher
max_rugosity <- 5
min_rugosity <- 1
cover <- 0.56 # this is box-specific, species-specific and cohort-specific if I understand the function right
rug_erode <- 0.000063	
unbleached <- 0.95

# erosion parameters
rug_erode_sponge <- 0.001 
rug_erode_grazer <- 0.002 # "mainly scarid fish" (Tribollet 2005)
cover_sponge <- 0.01
grazer_abundance <- 1000 # this would be in mg N m-2 in the current box, i.e. biomass of grazer per unit area BUT: this way not comparable!

# start from non-sheltering, i.e. massive. Needs some iterations to get to regime

time_steps <- 1:10000
local_rugosity <- rep(0, length(time_steps)) # this starts from 0, let's run some simulations


for (ts in 1:(length(time_steps)-1)) { # this must be looped across species. In theory the fact that it is weighted by the cover should guarantee it does not overshoot
  
  rug_growth <- rugosity_inc * (cover * unbleached) * (max_rugosity - local_rugosity[ts]) # this is modification as code seems wrong
  # this means that at each time step only live corals 
  
  # original would be: rug_growth <- rugosity_inc * unbleached * (max_rugosity - LocalRugosity) # but this fails to account for coral cover to my understanding
  
  rug_erosion_sponge <- rug_erode_sponge * cover_sponge * (1.0 - cover * unbleached) * (local_rugosity[ts] - min_rugosity) # 
  rug_erosion_graze <- rug_erode_grazer * grazer_abundance * (1.0 - cover * unbleached) * (local_rugosity[ts] - min_rugosity)
  rug_erosion <- rug_erosion_sponge + rug_erosion_graze
  
  delta_rug <- rug_growth - rug_erosion # this is the rugosity increase
  
  local_rugosity[ts+1] <- delta_rug + local_rugosity[ts] # this will need to be looped across all species as they all should contribute to rugosity!! 
  # Drawback o this method is that it does not really account for cohorts and their different contributions
  # Also how to calibrate sponges? rug_increase makes a difference for the max attainable growth too.
}


plot(time_steps, local_rugosity)


