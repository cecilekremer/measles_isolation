
library(foreach)
library(doParallel)

## Load functions
source('/lustre1/scratch/326/vsc32693/Measles/simFunctions.R')
source('/lustre1/scratch/326/vsc32693/Measles/measlesSimulator.R')


# args <- commandArgs(trailingOnly = TRUE)
# scenario = args[1]
scenario <- Sys.getenv("scenario")
# print(scenario)

## COMMAND LINE ARGUMENTS ----
################################## #

seeding.time = Inf # no additional seeding
prop.lambda.w = 1
rho.ch = 1 # prob of symptomatic child
rho.ad = 1 # prob of symptomatic adult
school.size = 420

# R.s = 9.9 # R0; 
# R.ctcs, # ?
alpha.as = 1
prop.immune.ch = 0.8 # prop vaccinated/immune -> status.matrix$infected = -2
prop.immune.ad = 1 # prop vaccinated/immune
#suscep.ch: child susceptibility, relative to adult susceptibility
suscep.ch = 1
#pdiagn.ch: probability to TEST a child, when symptomatic
pdiagn.ch = 0.9
#pdiagn.ad: probability to TEST a adult, when symptomatic
pdiagn.ad = 0.9
test.sens.as = 0.85
test.sens.s = 0.85
test.delay.s = 0
test.delay.as = 10000

#t.detection: time it takes before an indvidual becomes detectable -> 4 days before rash onset, i.e. 8 days (latent period) after infection
t.detection = 8
#t.isolation: duration of the isolation -> t_rash + 4 days
t.isolation = 4
#t.isolation.q: duration of isolation after exposure
# t.isolation.q = 21
#t.stop: duration of the simulation
t.stop = 100

#start of isolation for classmates: day X
delay.isolation = 0

# scenario <- 'Baseline'
# 
# print(scenario)
cat("scenario=",scenario)

if(scenario == 'Baseline'){
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 12
  overdispersion = FALSE
}else if(scenario == 'Scen1'){ # expect more transmission
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 18
  overdispersion = FALSE
}else if(scenario == 'Scen2'){ # expect fewer but bigger outbreaks
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 12
  overdispersion = TRUE
  k.overd = 0.1
}else if(scenario == 'Scen3'){ # expect fewer but bigger outbreaks compared to Baseline, but less than Scen2
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 12
  overdispersion = TRUE
  k.overd = 5
}else if(scenario == 'Scen4'){ # expect less transmission
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 12
  overdispersion = FALSE
  t.detection = 0
}else if(scenario == 'Scen5'){ # expect less transmission
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 12
  overdispersion = FALSE
  prop.immune.ch = 0.9
}else if(scenario == 'Scen6'){ # expect more transmission
  child.seed.arg = 1
  t.isolation.q = 0
  R.s = 12
  overdispersion = FALSE
  prop.immune.ch = 0.5
}else if(scenario == 'Scen7'){
  child.seed.arg = 1
  t.isolation.q = 0
  t.isolation = 0
  R.s = 12
  overdispersion = FALSE
  prop.immune.ch = 0
  pdiagn.ch = 1
  test.sens.s = 1
}else if(scenario == 'Scen8'){ # expect fewer but bigger outbreaks
  child.seed.arg = 1
  t.isolation.q = 0
  t.isolation = 0
  R.s = 12
  prop.immune.ch = 0
  overdispersion = TRUE
  k.overd = 0.3
  pdiagn.ch = 1
  test.sens.s = 1
}else if(scenario == 'Scen9'){ # expect fewer but bigger outbreaks compared to Baseline, but less than Scen2
  child.seed.arg = 1
  t.isolation.q = 0
  t.isolation = 0
  R.s = 12
  prop.immune.ch = 0
  overdispersion = TRUE
  k.overd = 5
  pdiagn.ch = 1
  test.sens.s = 1
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

#Repetitive seeding
reseeding.ch <- 0
reseeding.ad <- 0

total.seeds<-(seeds.ch+seeds.ad)+floor(t.stop/seeding.time)*(reseeding.ad+reseeding.ch)


## PRE-PROCESSING    ----
################################## #

# set RNG stream
rnSeed = 211124
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

#lambda.b: contact frequency between classes
#data from: SOCRATES - Survey Belgium 2010 - contacts for children [6,12] years old (ALL CONTACTS EXCLUDED HOLIDAYS) - within: more than 1 hr - between total contacts - within
lambda.w = min(classes.comp) - 1 + 2 #techars; #6.62 # contact frequency within classes
lambda.b.tot <- 2.5 # contact frequency between classes in a non-pandemic situation
lambda.b = lambda.b.tot * prop.lambda.w #prop.lamba is the proportion of between contacts respect to the non-pandemic scenario

#
R.ctcs<-lambda.w + lambda.b #12.16 #(all cts excluded holidays) 

## RUN SIMULATION                ----
################################## #

# bool.log.contacts = FALSE


cl <- makeCluster(71)
registerDoParallel(cl)

nSim <- 100

epi.outbreak <- list()

epi.outbreak<-foreach(i_sim = 1:nSim) %dopar% {
  #print(i_sim)
  epi.outbreak[[i_sim]]<-measles_simulator(t.stop = t.stop, seeding.time = seeding.time,
                                           n.teachers = n.teachers, classes.comp = classes.comp,
                                           seeds.ch = seeds.ch, seeds.ad = seeds.ad, reseeding.ch = reseeding.ch, reseeding.ad = reseeding.ad,
                                           overdispersion = overdispersion, k.overd = k.overd,
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
                                           delay.isolation = delay.isolation,
                                           test.sens.as = test.sens.as, test.sens.s = test.sens.s, 
                                           test.delay.s = test.delay.s, test.delay.as = test.delay.as, 
                                           t.isolation = t.isolation, # isolation for 4 days after rash onset
                                           t.isolation.q = t.isolation.q, # isolation after exposure)
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
for(i_sim in 1:nSim){
  
  total.seeds <- length(which(epi.outbreak[[i_sim]]$status.matrix$IndexCase == 1))
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
  propOnwardNoSeed[i_sim] <- length(unique(events.sim$id[events.sim$event=='Infector' & events.sim$id != id.index])) /
    (sum(!is.na(dfn$InfectionTime)) - total.seeds)
  
  # Proportion of isolated infected cases that cause secondary case AFTER their initial isolation
  ids.isol <- unique(events.sim$id[events.sim$event=='BackFromIso'])
  ids.isol <- ids.isol[which(!(ids.isol %in% id.index))]
  ids.inf.isol <- unique(ids.isol[ids.isol %in% events.sim$id[events.sim$event == 'Infector']])
  inf.after.isol <- rep(0, length(ids.inf.isol))
  for(i in 1:length(ids.inf.isol)){
    inf.times <- events.sim$time[events.sim$event=='Infector' & events.sim$id == ids.inf.isol[i]]
    backfromiso.times <- events.sim$time[events.sim$event=='BackFromIso' & events.sim$id == ids.inf.isol[i]][1] # first time back from isolation
    timediff <- inf.times - backfromiso.times
    if(any((timediff[!is.na(timediff)]) > 0)){
      inf.after.isol[i] <- 1
    }
  }
  propOnwardAfterIsol[i_sim] <- sum(inf.after.isol) / (sum(!is.na(dfn$InfectionTime)) - total.seeds)
  
}
# print(paste0('ONWARD1: ', paste(propOnward, collapse = ', '))); 
# print(paste0('ONWARD2: ', paste(propOnwardNoSeed, collapse = ', '))); 
# print(paste0('ONWARD3: ', paste(propOnwardAfterIsol, collapse = ', ')))

# length(propOnward)
# length(propOnwardNoSeed)
# length(propOnwardAfterIsol)
# class(propOnward)
# class(propOnwardNoSeed)
# class(propOnwardAfterIsol)

df.trans <- data.frame(
  simID = 1:nSim,
  propOnward = propOnward,
  propOnwardNoSeed = propOnwardNoSeed,
  propOnwardAfterIsol = propOnwardAfterIsol
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
casesPeak <- NULL
n.susceptible <- NULL
for(i_sim in 1:nSim){
  total.seeds <- length(which(epi.outbreak[[i_sim]]$status.matrix$IndexCase == 1))
  finalSize[i_sim] <- length(c(which(epi.outbreak[[i_sim]]$status.matrix$infected == (-1)), # recovered
                               which(epi.outbreak[[i_sim]]$status.matrix$infected == 1) # infected
  ))
  finalSizeProp[i_sim] <- (finalSize[i_sim] - total.seeds) / n
  n.detectedpositive <- c(n.detectedpositive, epi.outbreak[[i_sim]]$n.detectedpositive)
  n.immune <- rbind(n.immune, sum(epi.outbreak[[i_sim]]$status.matrix$infected == (-2)))
  n.susceptible <- rbind(n.susceptible, sum(epi.outbreak[[i_sim]]$status.matrix$infected == 0))
  ## Peak number of cases
  events.sim <- epi.outbreak[[i_sim]]$time.events[-1, ]
  events.sim <- data.frame(time = as.numeric(events.sim[,1]),
                           event = events.sim[,2],
                           id = as.numeric(events.sim[,3]))
  events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
  events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
  epi.curve<-0
  for (t in 1:length(events.sim[,1]) ){
    # number of infected at each step
    epi.curve[t]<-length(which(events.sim[1:t,2]=='Infection'))
  }
  casesPeak[i_sim] <- max(epi.curve)
}
FinSize<-finalSize
FinalSizeProp<-finalSizeProp
# n.tests<-n.tests
df <- data.frame(
  simID = 1:nSim,
  FinSize   = FinSize,
  FinSizeProp = FinalSizeProp,
  # n.tests   = n.tests,
  n.immune  = n.immune, 
  n.susceptible = n.susceptible,
  casesPeak = casesPeak,
  # n.severe1 = n.severe1,
  # n.severe2 = n.severe2,
  # n.schooldayslost= n.schooldayslost,
  n.detectedpositive = n.detectedpositive)
# n.classclosure = n.classclosure,
# n.schoolclosure = n.schoolclosure)
# df
write.csv(df, paste0(scenario,"_outbreak_characteristics.csv"))

## save everything
# saveRDS(epi.outbreak, file = paste0('epi_outbreak_', scenario, '.rds'))



