
library(foreach)
library(doParallel)
library(EnvStats)

## Load functions
source('/lustre1/scratch/326/vsc32693/Measles/simFunctions.R')
source('/lustre1/scratch/326/vsc32693/Measles/measlesSimulator_v2.R')

scenario <- Sys.getenv("scenario")

## COMMAND LINE ARGUMENTS ----
################################## #

prop.lambda.w = 1
rho.ch = 1 # prob of symptomatic child
rho.ad = 1 # prob of symptomatic adult
school.size = 420

R.s = 12 # R0; 
# R.ctcs, # ?
alpha.as = 1
prop.immune.ch1 = 0.89 # prop vaccinated/immune school year 1-3 -> status.matrix$infected = -2
prop.immune.ch2 = 0.95 # school year 4-6
prop.immune.ad = 1 # prop vaccinated/immune
#suscep.ch: child susceptibility, relative to adult susceptibility
suscep.ch = 1
#pdiagn.ch: probability to TEST a child, when symptomatic
pdiagn.ch = 0.5
#pdiagn.ad: probability to TEST a adult, when symptomatic
pdiagn.ad = 0.9
test.sens.as = 0.85
test.sens.s = 0.95
test.delay.s = 0
test.delay.as = 10000

#t.detection: time it takes before an indvidual becomes detectable -> 4 days before rash onset, i.e. 8 days (latent period) after infection
t.detection = 3
detectionSymp = 'dist' # 'symp.onset' or 'dist' or 'default'
# PEV will never be effective if this is the time of rash onset (b/c exposure time is t_rash - 4)
median.incubation = 12.5
fixed.incubation = FALSE

#t.isolation: duration of the isolation -> t_rash + 4 days
t.isolation = 4
#t.isolation.q: duration of isolation after exposure
# t.isolation.q = 21
#t.stop: duration of the simulation
t.stop = 1000

#start of isolation for classmates: day X
delay.isolation = 3
# delay.isolation = 0
isolate.once = FALSE

const.infect = TRUE # constant infectiousness profile or peaked

weekend = TRUE # account for weekends (no contacts)

## Post-exposure vaccination
PEV = FALSE
pev.eff = 0.85
pev.accept = 0.3
pev.duration.q = 0
delay.PEV = 0

# between-contact rate
lambda.b.tot <- 2.5 # contact frequency between classes in a non-pandemic situation

# scenario <- 'Baseline'
# args <- commandArgs(trailingOnly = TRUE)
# scenario = as.character(args[1])

child.seed.arg = 1
#Repetitive seeding
reseeding.ch <- 0
reseeding.ad <- 0
seeding.time <- Inf # no additional seeding


k.overd = 1000

# scenario <- 'Baseline'
# args <- commandArgs(trailingOnly = TRUE)
# scenario = as.character(args[1])
# 
# print(scenario)

if(scenario == 'Baseline'){
  t.isolation.q = 0 # PEV only included if this is > 0
  overdispersion = FALSE
}else if(scenario == 'Scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
}else if(scenario == 'Scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
}else if(scenario == 'Scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
}else if(scenario == 'Scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  #### Scenarios superspreading
}else if(scenario == 'Scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
}else if(scenario == 'Scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
}else if(scenario == 'Scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
}else if(scenario == 'Scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
}else if(scenario == 'Scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  ### Sensitivity analyses
}else if(scenario == 'Sens1_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  R.s = 18
}else if(scenario == 'Sens1_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  R.s = 18
}else if(scenario == 'Sens1_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  R.s = 18
}else if(scenario == 'Sens1_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  R.s = 18
}else if(scenario == 'Sens1_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  R.s = 18
  #### Scenarios superspreading
}else if(scenario == 'Sens1_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  R.s = 18
}else if(scenario == 'Sens1_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  R.s = 18
}else if(scenario == 'Sens1_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  R.s = 18
}else if(scenario == 'Sens1_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  R.s = 18
}else if(scenario == 'Sens1_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  R.s = 18
}else if(scenario == 'Sens2_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  #### Scenarios superspreading
}else if(scenario == 'Sens2_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens2_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
}else if(scenario == 'Sens3_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
  #### Scenarios superspreading
}else if(scenario == 'Sens3_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens3_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.2
}else if(scenario == 'Sens4_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  const.infect = FALSE
}else if(scenario == 'Sens4_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  const.infect = FALSE
}else if(scenario == 'Sens4_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  const.infect = FALSE
}else if(scenario == 'Sens4_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  const.infect = FALSE
}else if(scenario == 'Sens4_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  const.infect = FALSE
  #### Scenarios superspreading
}else if(scenario == 'Sens4_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  const.infect = FALSE
}else if(scenario == 'Sens4_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  const.infect = FALSE
}else if(scenario == 'Sens4_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  const.infect = FALSE
}else if(scenario == 'Sens4_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  const.infect = FALSE
}else if(scenario == 'Sens4_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  const.infect = FALSE
}else if(scenario == 'Sens5_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  detectionSymp = 'symp.onset'
  #### Scenarios superspreading
}else if(scenario == 'Sens5_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens5_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'symp.onset'
}else if(scenario == 'Sens6_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  detectionSymp = 'dist2'
  #### Scenarios superspreading
}else if(scenario == 'Sens6_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'dist2'
}else if(scenario == 'Sens6_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  detectionSymp = 'dist2'
}else if(scenario == 'Sens7_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  lambda.b.tot = 6
  #### Scenarios superspreading
}else if(scenario == 'Sens7_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  lambda.b.tot = 6
}else if(scenario == 'Sens7_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  lambda.b.tot = 6
}else if(scenario == 'Sens8_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  isolate.once = TRUE
  #### Scenarios superspreading
}else if(scenario == 'Sens8_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  isolate.once = TRUE
}else if(scenario == 'Sens8_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  isolate.once = TRUE
}else if(scenario == 'Sens9_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  median.incubation = 14
}else if(scenario == 'Sens9_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  median.incubation = 14
}else if(scenario == 'Sens9_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  median.incubation = 14
}else if(scenario == 'Sens9_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  median.incubation = 14
}else if(scenario == 'Sens9_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  median.incubation = 14
  #### Scenarios superspreading
}else if(scenario == 'Sens9_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  median.incubation = 14
}else if(scenario == 'Sens9_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  median.incubation = 14
}else if(scenario == 'Sens9_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  median.incubation = 14
}else if(scenario == 'Sens9_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  median.incubation = 14
}else if(scenario == 'Sens9_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  median.incubation = 14
  ## Multiple introductions (random from entire school)
}else if(scenario == 'Sens10_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
  #### Scenarios superspreading
}else if(scenario == 'Sens10_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
}else if(scenario == 'Sens10_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  child.seed.arg = 3
  ## Multiple introductions (random from entire school) at multiple times
  ## This introduces a new seed every 'seeding.time' days ...
}else if(scenario == 'Sens11_baseline'){
  t.isolation.q = 0
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen1'){
  t.isolation.q = 21
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen2'){
  t.isolation.q = 18
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen3'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen4'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = FALSE
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
  #### Scenarios superspreading
}else if(scenario == 'Sens11_scen5'){
  t.isolation.q = 0
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen6'){
  t.isolation.q = 21
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen7'){
  t.isolation.q = 18
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen8'){
  t.isolation.q = 21
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}else if(scenario == 'Sens11_scen9'){
  t.isolation.q = 18
  delay.isolation = 9
  overdispersion = TRUE
  k.overd = 0.3
  prop.immune.ch1 <- prop.immune.ch2 <- 0.7
  reseeding.ch <- 1
  seeding.time <- 10
}



## DEFAULT PARAMETERS           ----
################################## #

#pmf inferred from data
#data from: https://www.agodi.be/nieuwe-omkadering-basisonderwijs
#starts at class size 1
#!!! primary class sizes, best proxy for secondary class sizes, I'm afraid
zero = .Machine$double.eps
class_size_pmf = c(zero, 0.000452284034373587, zero, 0.000452284034373587, zero, zero, zero, zero, 0.000904568068747173, 0.00135685210312076, 0.00180913613749435, 
                   0.00180913613749435, 0.00814111261872456, 0.0235187697874265, 
                   0.0398009950248756, 0.052464947987336, 0.0687471732247852, 0.106286748077793, 
                   0.180009045680687, 0.26232473993668, 0.251922207146088) 

#n.teachers: number of teachers
#compute as proportion of the pupils, from:
#https://www.vlaanderen.be/publicaties/vlaams-onderwijs-in-cijfers
teacher.pupil.ratio <- 1/9

seeds.ad = 0
seeds.ch = child.seed.arg 


total.seeds<-(seeds.ch+seeds.ad)+floor(t.stop/seeding.time)*(reseeding.ad+reseeding.ch)


## PRE-PROCESSING    ----
################################## #

# set RNG stream
rnSeed = 191124
set.seed(rnSeed)

# sample population
classes.comp = c()
pupils = 0
while(pupils < school.size) {
  class = sample(x=1:length(class_size_pmf), prob=class_size_pmf, size=1)
  classes.comp = c(classes.comp, class)
  pupils = sum(classes.comp)
}

#n.teachers: number of teachers
n.teachers <- round(pupils*teacher.pupil.ratio, 0)

#total.considered.population
n<-pupils+n.teachers

#within-class contact rate
# lambda.w = rep(classes.comp-1, classes.comp) # + teachers ??

#lambda.b: contact frequency between classes
#data from: SOCRATES - Survey Belgium 2010 - contacts for children [6,12] years old (ALL CONTACTS EXCLUDED HOLIDAYS) - within: more than 1 hr - between total contacts - within
# lambda.w = 6.62 # contact frequency within classes
# lambda.w <- min(classes.comp) - 1 + 2 # 16
lambda.b.tot <- lambda.b.tot # contact frequency between classes in a non-pandemic situation
lambda.b = lambda.b.tot * prop.lambda.w #prop.lamba is the proportion of between contacts respect to the non-pandemic scenario

#
# R.ctcs<-12.16 #(all cts excluded holidays) 
# R.ctcs <- lambda.w + lambda.b

## RUN SIMULATION                ----
################################## #

# bool.log.contacts = FALSE


cl <- makeCluster(95)
registerDoParallel(cl)

nSim <- 10000

epi.outbreak <- list()

epi.outbreak<-foreach(i_sim = 1:nSim, 
                      .packages=c("EnvStats")) %dopar% {
                        #print(i_sim)
                        epi.outbreak[[i_sim]]<-measles_simulator(t.stop = t.stop, seeding.time = seeding.time,
                                                                 n.teachers = n.teachers, classes.comp = classes.comp,
                                                                 seeds.ch = seeds.ch, seeds.ad = seeds.ad, reseeding.ch = reseeding.ch, reseeding.ad = reseeding.ad,
                                                                 overdispersion = overdispersion, k.overd = k.overd,
                                                                 R.s = R.s, # R0; https://www.sciencedirect.com/science/article/pii/S1473309917303079#cesec60 
                                                                 # R.ctcs = R.ctcs, # ?
                                                                 alpha.as = alpha.as,
                                                                 prop.immune.ch1 = prop.immune.ch1, # prop vaccinated/immune -> status.matrix$infected = -2
                                                                 prop.immune.ch2 = prop.immune.ch2,
                                                                 prop.immune.ad = prop.immune.ad, # prop vaccinated/immune
                                                                 # lambda.w = lambda.w, 
                                                                 lambda.b = lambda.b,
                                                                 suscep.ch = suscep.ch,
                                                                 rho.ch = rho.ch, rho.ad = rho.ad,
                                                                 pdiagn.ch = pdiagn.ch, pdiagn.ad = pdiagn.ad,
                                                                 t.detection = t.detection, # time it takes before an individual becomes detectable
                                                                 detectionSymp = 'dist', # indicator whether everyone is detected at first symptoms ('symp.onset') or depending on rash onset ('dist') 
                                                                 median.incubation = median.incubation,
                                                                 fixed.incubation = fixed.incubation,
                                                                 const.infect = const.infect,
                                                                 delay.isolation = delay.isolation,
                                                                 isolate.once = isolate.once,
                                                                 test.sens.as = test.sens.as, test.sens.s = test.sens.s, 
                                                                 test.delay.s = test.delay.s, test.delay.as = test.delay.as, 
                                                                 t.isolation = t.isolation, # isolation for 4 days after rash onset
                                                                 t.isolation.q = t.isolation.q, # isolation after exposure)
                                                                 daycare = FALSE, prop.infants = prop.infants, 
                                                                 days.present = days.present.init, children.present = children.present.init,
                                                                 weekend = TRUE,                                                                  
                                                                 PEV = FALSE, pev.eff = pev.eff, pev.accept = pev.accept, pev.duration.q = pev.duration.q, delay.PEV = delay.PEV,
                                                                 bool.log.contacts = FALSE
                        )
                      }

stopCluster(cl)


## SIMULATION OUTPUT            ----
################################## #

setwd('/lustre1/scratch/326/vsc32693/Measles/out')

##----------------------------------------------------------------------------------
## Contact networks

print('creating contact network....')

contact.network <- list()
for (i in 1:nSim) {
  ctc.betw<-rep(NA,n)
  temp.ctc.betw<-epi.outbreak[[i]]$between.classes.contacts
  for (j in 1:length(temp.ctc.betw)){
    if (length(temp.ctc.betw[[j]][-1])>0){
      ctc.betw[j]<-paste(temp.ctc.betw[[j]][-1],collapse = ",")
    }
  }
  temp.status.matrix<-epi.outbreak[[i]]$status.matrix
  contact.network[[i]] <- data.frame(ID=1:n,
                                     StudentClass=temp.status.matrix$Category,
                                     TeacherClass=temp.status.matrix$TeacherAssign,
                                     IsIndex=temp.status.matrix$IndexCase,
                                     InfectionTime=temp.status.matrix$time.of.infection,
                                     InfectorID=temp.status.matrix$infector,
                                     SymptOnsDate=temp.status.matrix$TimeSymptomOnset,
                                     RashOnsDate=temp.status.matrix$TimeRashOnset,
                                     DayPosTest=temp.status.matrix$TimePosTest,
                                     # Compliance=temp.status.matrix$Compliance,
                                     isImmune=ifelse(temp.status.matrix$infected!=0, 1, 0),
                                     BetweenClassesCtc=ctc.betw)
  # write.csv(dfn, paste0(out,"InfNetw_Sim",i,".csv"))
}
saveRDS(contact.network, file = paste0('contact_networks_', scenario, '.rds'))

###-----------------------------------------------------------------------------------
### PROBABILITY OF ONWARD TRANSMISSION

print('obtaining probability of onward transmission....')

propOnward <- NULL
propOnwardNoSeed <- NULL
propOnwardAfterIsol <- NULL
pInfectiousAfterIsol <- NULL
R_index <- NULL
propOnwardAfterPEV <- NULL
propEffectivePEV <- NULL
propReceivedPEV <- NULL
propSuscReceivedPEV <- NULL
propSusceptibleStart <- NULL
propSusceptibleEnd <- NULL

for(i_sim in 1:nSim){
  
  total.seeds <- length(which(epi.outbreak[[i_sim]]$status.matrix$IndexCase == 1))
  n <- dim(epi.outbreak[[i_sim]]$status.matrix)[1]
  # Simulation events
  events.sim <- epi.outbreak[[i_sim]]$time.events[-1, ]
  events.sim <- data.frame(time = as.numeric(events.sim[,1]),
                           event = events.sim[,2],
                           id = as.numeric(events.sim[,3]))
  events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
  events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
  
  # Contact network
  ctc.betw<-rep(NA,n)
  temp.ctc.betw<-epi.outbreak[[i_sim]]$between.classes.contacts
  for (j in 1:length(temp.ctc.betw)){
    if (length(temp.ctc.betw[[j]][-1])>0){
      ctc.betw[j]<-paste(temp.ctc.betw[[j]][-1],collapse = ",")
    }
  }
  temp.status.matrix<-epi.outbreak[[i_sim]]$status.matrix
  dfn<-data.frame(ID=1:n,
                  StudentClass=temp.status.matrix$Category,
                  TeacherClass=temp.status.matrix$TeacherAssign,
                  IsIndex=temp.status.matrix$IndexCase,
                  InfectionTime=temp.status.matrix$time.of.infection,
                  InfectorID=temp.status.matrix$infector,
                  SymptOnsDate=temp.status.matrix$TimeSymptomOnset,
                  RashOnsDate=temp.status.matrix$TimeRashOnset,
                  DayPosTest=temp.status.matrix$TimePosTest,
                  # Compliance=temp.status.matrix$Compliance,
                  isImmune=ifelse(temp.status.matrix$infected!=0, 1, 0),
                  BetweenClassesCtc=ctc.betw)
  
  # Proportion of infected cases that cause secondary case, incl seeds
  propOnward[i_sim] <- length(unique(events.sim$id[events.sim$event=='Infector'])) / sum(!is.na(dfn$InfectionTime))
  
  # Proportion of infected cases that cause secondary case, excl seeds
  id.index <- dfn$ID[dfn$IsIndex == 1]
  propOnwardNoSeed[i_sim] <- length(unique(events.sim$id[events.sim$event=='Infector' & !(events.sim$id %in% id.index)])) /
    (sum(!is.na(dfn$InfectionTime)) - total.seeds)
  
  # Proportion of infected cases where PEV was effective
  ids.inf <- unique(events.sim$id[events.sim$event == 'Infection'])
  ids.inf <- ids.inf[!(ids.inf %in% id.index)]
  ids.inf.pev <- ids.inf[ids.inf %in% events.sim$id[events.sim$event == 'ContactPEV']]
  ids.inf.pev.eff <- ids.inf.pev[ids.inf.pev %in% events.sim$id[events.sim$event == 'EffectivePEV']]
  propEffectivePEV[i_sim] <- ifelse(length(ids.inf.pev.eff) > 0,
                                    length(unique(ids.inf.pev.eff)) / length(unique(ids.inf.pev)),
                                    0)
  # proportion of infected individuals that received PEV
  propReceivedPEV[i_sim] <- ifelse(length(ids.inf.pev) > 0,
                                   length(ids.inf.pev) / (sum(!is.na(dfn$InfectionTime)) - total.seeds),
                                   0)
  
  # Proportion of susceptibles that received PEV
  n.immune <- sum(epi.outbreak[[i_sim]]$status.matrix$infected == (-2) & epi.outbreak[[i_sim]]$status.matrix$PEV.status == 0) # those that were immune at the start
  n.susc.init <- n - n.immune
  # ids.pev <- unique(events.sim$id[events.sim$event == 'ContactPEV'])
  ids.pev <- sum(epi.outbreak[[i_sim]]$status.matrix$PEV.status == 1)
  propSuscReceivedPEV[i_sim] <- ifelse(length(ids.pev) > 0 & ids.pev != 0,
                                       length(ids.pev) / n.susc.init,
                                       0)
  
  # Proportion of individuals that are susceptible at the end of outbreak
  propSusceptibleStart[i_sim] <- (n - n.immune)/n
  propSusceptibleEnd[i_sim] <- sum(epi.outbreak[[i_sim]]$status.matrix$infected == 0)/n
  
  # secondary cases infected by index
  cases.index <- epi.outbreak[[i_sim]]$status.matrix$id[epi.outbreak[[i_sim]]$status.matrix$infector %in% id.index]
  cases.index <- cases.index[!is.na(cases.index)]
  R_index[i_sim] <- length(unique(cases.index))/length(id.index)
  
  # Proportion of isolated infected cases that cause secondary case AFTER their initial isolation
  # Detected cases are never infectious after isolation, so this only applies to undiagnosed isolated contacts (quarantined)
  ids.isol <- unique(events.sim$id[events.sim$event=='BackFromIso'])
  ids.isol <- ids.isol[which(!(ids.isol %in% id.index))]
  ids.inf.isol <- unique(ids.isol[ids.isol %in% events.sim$id[events.sim$event == 'Infector']])
  inf.after.isol <- rep(0, length(ids.inf.isol))
  if(length(ids.inf.isol) > 0){
    for(i in 1:length(ids.inf.isol)){
      inf.times <- events.sim$time[events.sim$event=='Infector' & events.sim$id == ids.inf.isol[i]]
      infection.time <- epi.outbreak[[i_sim]]$status.matrix$time.of.infection[epi.outbreak[[i_sim]]$status.matrix$id == ids.inf.isol[i]]
      backfromiso.times <- events.sim$time[events.sim$event=='BackFromIso' & events.sim$id == ids.inf.isol[i]]
      # first time back from isolation AFTER INFECTION
      backfromiso.times <- min(backfromiso.times[backfromiso.times > infection.time], na.rm = T)
      timediff <- inf.times - backfromiso.times
      if(any((timediff[!is.na(timediff)]) > 0)){
        inf.after.isol[i] <- 1
      }
    }
  }
  propOnwardAfterIsol[i_sim] <- ifelse(length(ids.inf.isol) > 0,
                                       sum(inf.after.isol) / (sum(!is.na(dfn$InfectionTime)) - total.seeds),
                                       0)
  
  ## Check how many exposed contacts got infected and returned from isolation while still infectious -> potential for transmission
  ## Detected cases are never infectious after coming back from isolation
  ids.infected <- events.sim$id[events.sim$event == 'Infection']
  ids.infected.rash <- NULL
  ids.infected.backfromiso <- list()
  ids.infectious.after.iso <- c()
  for(i in 1:length(ids.infected)){
    ids.infected.rash[i] <- epi.outbreak[[i_sim]]$status.matrix$TimeRashOnset[epi.outbreak[[i_sim]]$status.matrix$id == ids.infected[i]]
    back.iso <- events.sim$time[events.sim$event=='BackFromIso' & events.sim$id == ids.infected[i]]
    if(length(back.iso) > 0){
      ids.infected.backfromiso[[i]] <- back.iso
      # back from isolation during infectious period?
      if(any(back.iso >= (ids.infected.rash[i] - 4) & back.iso <= (ids.infected.rash[i] + 4))){
        ids.infectious.after.iso <- c(ids.infectious.after.iso, ids.infected[i])
      }
    }
  }
  pInfectiousAfterIsol[i_sim] <- length(unique(ids.infectious.after.iso)) / length(unique(ids.infected))
  
  ## Proportion of onward transmission from cases who received PEV
  # ANY onward transmission from infectious individuals who received PEV after they were infected
  ids.pev <- unique(events.sim$id[events.sim$event == 'ContactPEV'])
  ids.inf.pev <- unique(ids.pev[ids.pev %in% events.sim$id[events.sim$event == 'Infector']])
  inf.after.pev <- rep(0, length(ids.inf.pev))
  if(length(ids.inf.pev) > 0){
    for( i in 1:length(ids.inf.pev)){
      inf.times <- events.sim$time[events.sim$event == 'Infector' & events.sim$id == ids.inf.pev[i]]
      infection.time <- epi.outbreak[[i_sim]]$status.matrix$time.of.infection[epi.outbreak[[i_sim]]$status.matrix$id == ids.inf.pev[i]]
      pev.time <- events.sim$time[events.sim$event == 'ContactPEV' & events.sim$id == ids.inf.pev[i]]
      if(infection.time < pev.time){
        timediff <- inf.times - pev.time
        if(any((timediff[!is.na(timediff)]) > 0)){
          inf.after.pev[i] <- 1
        }
      }
    }
  }
  propOnwardAfterPEV[i_sim] <- ifelse(length(ids.inf.pev) > 0,
                                      sum(inf.after.pev) / (sum(!is.na(dfn$InfectionTime)) - total.seeds),
                                      0)
  
}

df.trans <- data.frame(
  simID = 1:nSim,
  propOnward = propOnward,
  propOnwardNoSeed = propOnwardNoSeed,
  propOnwardAfterIsol = propOnwardAfterIsol,
  pInfectiousAfterIsol = pInfectiousAfterIsol,
  R_index = R_index,
  propOnwardAfterPEV = propOnwardAfterPEV,
  propEffectivePEV = propEffectivePEV,
  propReceivedPEV = propReceivedPEV,
  propSuscReceivedPEV = propSuscReceivedPEV,
  propSusceptibleStart = propSusceptibleStart,
  propSusceptibleEnd = propSusceptibleEnd
)
# n.classclosure = n.classclosure,
# n.schoolclosure = n.schoolclosure)
# df
write.csv(df.trans, paste0(scenario,"_onward_transmission.csv"))

##--------------------------------------------------------------------------------------
## Outbreak characteristics

print('obtaining outbreak characteristics....')


finalSize <- NULL
finalSizeProp <- NULL
n.detectedpositive <- NULL
n.immune <- NULL
n.susceptible <- NULL
propDetected <- NULL
propSuscIsolated <- NULL

for(i_sim in 1:nSim){
  total.seeds <- length(which(epi.outbreak[[i_sim]]$status.matrix$IndexCase == 1))
  finalSize[i_sim] <- length(c(which(epi.outbreak[[i_sim]]$status.matrix$infected == (-1)), # recovered
                               which(epi.outbreak[[i_sim]]$status.matrix$infected == 1) # infected
  ))
  finalSizeProp[i_sim] <- (finalSize[i_sim] - total.seeds) / n
  n.detectedpositive <- c(n.detectedpositive, epi.outbreak[[i_sim]]$n.detectedpositive)
  propDetected <- c(propDetected, n.detectedpositive[i_sim]/finalSize[i_sim])
  n.immune <- c(n.immune, sum(epi.outbreak[[i_sim]]$status.matrix$infected == (-2) & epi.outbreak[[i_sim]]$status.matrix$PEV.status == 0)) # number immune at start of outbreak
  n.susceptible <- c(n.susceptible, sum(epi.outbreak[[i_sim]]$status.matrix$infected == 0)) # number susceptible at end of outbreak
  n.susc.init <- n - n.immune[i_sim]
  events.sim <- epi.outbreak[[i_sim]]$time.events[-1, ]
  events.sim <- data.frame(time = as.numeric(events.sim[,1]),
                           event = events.sim[,2],
                           id = as.numeric(events.sim[,3]))
  events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
  events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
  n.susc.isol <- length(unique(events.sim$id[events.sim$event=='IsolationContact']))
  propSuscIsolated <- c(propSuscIsolated, n.susc.isol/n.susc.init)  
  # ## Peak number of cases
  # epi.curve<-0
  # for (t in 1:length(events.sim[,1]) ){
  #   # number of infected at each step
  #   epi.curve[t]<-length(which(events.sim[1:t,2]=='Infection'))
  # }
  # casesPeak[i_sim] <- max(epi.curve)
}
FinSize<-finalSize
FinalSizeProp<-finalSizeProp
# n.tests<-n.tests
df <- data.frame(
  simID = 1:nSim,
  FinSize   = FinSize,
  FinSizeProp = FinalSizeProp,
  
  propSuscIsolated = propSuscIsolated,
  propDetected = propDetected,
  # n.tests   = n.tests,
  n.immune  = n.immune, 
  n.susceptible = n.susceptible,
  # casesPeak = casesPeak,
  # n.severe1 = n.severe1,
  # n.severe2 = n.severe2,
  # n.schooldayslost= n.schooldayslost,
  n.detectedpositive = n.detectedpositive)
# n.classclosure = n.classclosure,
# n.schoolclosure = n.schoolclosure)
# df
write.csv(df, paste0(scenario,"_outbreak_characteristics.csv"))

## save everything
saveRDS(epi.outbreak, file = paste0('epi_outbreak_', scenario, '.rds'))



