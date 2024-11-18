
library(foreach)

## Load functions
source('/lustre1/scratch/326/vsc32693/Measles/simFunctions.R')
source('/lustre1/scratch/326/vsc32693/Measles/measlesSimulator.R')

## COMMAND LINE ARGUMENTS ----
################################## #

args <- commandArgs(trailingOnly = TRUE)

seeding.time = strtoi(args[1]) # no additional seeding
child.seed.arg = strtoi(args[2])
prop.lambda.w = 1
rho.ch = as.numeric(args[3]) # prob of symptomatic child
rho.ad = as.numeric(args[4]) # prob of symptomatic adult
school.size = as.numeric(args[5])

R.s = as.numeric(args[6]) # R0; https://www.sciencedirect.com/science/article/pii/S1473309917303079#cesec60 
# R.ctcs, # ?
alpha.as = as.numeric(args[7])
prop.immune.ch = as.numeric(args[8]) # prop vaccinated/immune -> status.matrix$infected = -2
prop.immune.ad = as.numeric(args[9]) # prop vaccinated/immune
#suscep.ch: child susceptibility, relative to adult susceptibility
suscep.ch = as.numeric(args[10])
#pdiagn.ch: probability to TEST a child, when symptomatic
pdiagn.ch = 1
#pdiagn.ad: probability to TEST a adult, when symptomatic
pdiagn.ad = 1
test.sens.as = 0.8 
test.sens.s = 0.8
test.delay.s = 0
test.delay.as = 10000

#t.detection: time it takes before an indvidual becomes detectable -> 4 days before rash onset, i.e. 8 days (latent period) after infection
t.detection = 8
#t.isolation: duration of the isolation -> t_rash + 5 days
t.isolation = 4
#t.isolation.q: duration of isolation after exposure
t.isolation.q = 21
#t.stop: duration of the simulation
t.stop <- 100



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

#Repetitive seeding
reseeding.ch <- 0
reseeding.ad <- 0

total.seeds<-(seeds.ch+seeds.ad)+floor(t.stop/seeding.time)*(reseeding.ad+reseeding.ch)


#lambda.b: contact frequency between classes
#data from: SOCRATES - Survey Belgium 2010 - contacts for children [6,12] years old (ALL CONTACTS EXCLUDED HOLIDAYS) - within: more than 1 hr - between total contacts - within
lambda.w = 6.62 # contact frequency within classes
lambda.b.tot <- 2.5 # contact frequency between classes in a non-pandemic situation
lambda.b = lambda.b.tot * prop.lambda.w #prop.lamba is the proportion of between contacts respect to the non-pandemic scenario

#
R.ctcs<-12.16 #(all cts excluded holidays) 


## PRE-PROCESSING    ----
################################## #

# set RNG stream
rnSeed = 14022020
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

## RUN SIMULATION                ----
################################## #

bool.log.contacts = FALSE

nSim <- 1

epi.outbreak <- list()

for(i_sim in 1:nSim) {
  #print(i_sim)
  epi.outbreak[[i_sim]]<-measles_simulator(t.stop = t.stop, seeding.time = seeding.time,
                                           n.teachers = n.teachers, classes.comp = classes.comp,
                                           seeds.ch = seeds.ch, seeds.ad = seeds.ad, reseeding.ch = reseeding.ch, reseeding.ad = reseeding.ad,
                                           R.s = R.s, # R0; https://www.sciencedirect.com/science/article/pii/S1473309917303079#cesec60 
                                           R.ctcs = R.ctcs, # ?
                                           alpha.as = alpha.as,
                                           prop.immune.ch = prop.immune.ch, # prop vaccinated/immune -> status.matrix$infected = -2
                                           prop.immune.ad = prop.immune.ad, # prop vaccinated/immune
                                           lambda.w = lambda.w, lambda.b = lambda.b,
                                           suscep.ch = suscep.ch,
                                           rho.ch = rho.ch, rho.ad = rho.ad,
                                           pdiagn.ch = pdiagn.ch, pdiagn.ad = pdiagn.ad,
                                           t.detection = t.detection, # time it takes before an individual becomes detectable
                                           test.sens.as = test.sens.as, test.sens.s = test.sens.s, 
                                           test.delay.s = test.delay.s, test.delay.as = test.delay.as, 
                                           t.isolation = t.isolation, # isolation for 4 days after rash onset
                                           t.isolation.q = t.isolation.q, # isolation after exposure)
                                           bool.log.contacts = FALSE
  )
}

