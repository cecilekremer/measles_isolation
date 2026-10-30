
library(EnvStats)

## Load functions
source('~/GitHub/measles_isolation/simulation_code/simFunctions.R')
source('~/GitHub/measles_isolation/simulation_code/measlesSimulator.R')

seeding.time = Inf # no additional seeding
prop.lambda.w = 1
rho.ch = 1 # prob of symptomatic child
rho.ad = 1 # prob of symptomatic adult
school.size = 100

R.s = 15 # R0; 
# R.ctcs, # ?
alpha.as = 1
prop.immune.ch1 = 0.75 # prop vaccinated/immune school year 1-3 -> status.matrix$infected = -2
prop.immune.ch2 = 0.75 # school year 4-6
prop.immune.ad = 1 # prop vaccinated/immune
#suscep.ch: child susceptibility, relative to adult susceptibility
suscep.ch = 1
#pdiagn.ch: probability to TEST a child, when symptomatic
pdiagn.ch = 0.9
#pdiagn.ad: probability to TEST a adult, when symptomatic
pdiagn.ad = 0.9
test.sens.as = 0.85
test.sens.s = 0.95
test.delay.s = 0
test.delay.as = 10000

#t.detection: time it takes before an indvidual becomes detectable -> 4 days before rash onset, i.e. 8 days (latent period) after infection
t.detection = 3
#t.isolation: duration of the isolation -> t_rash + 4 days
t.isolation = 4
#t.isolation.q: duration of isolation after exposure
# t.isolation.q = 21
#t.stop: duration of the simulation
t.stop = 100

#start of isolation for classmates: day X
delay.isolation = 0

child.seed.arg = 1
t.isolation.q = 15
overdispersion = TRUE

## Post-exposure vaccination
PEV = TRUE
pev.eff = 0.85
pev.accept = 0.3
pev.duration.q = 0

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
lambda.b.tot <- 10 # contact frequency between classes in a non-pandemic situation
lambda.b = lambda.b.tot * prop.lambda.w #prop.lamba is the proportion of between contacts respect to the non-pandemic scenario


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
                                           overdispersion = FALSE, k.overd = 1000,
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
                                           PEV = FALSE, pev.eff = pev.eff, pev.accept = pev.accept, pev.duration.q = pev.duration.q,
                                           bool.log.contacts = FALSE
  )
}

# head(epi.outbreak[[1]]$status.matrix[epi.outbreak[[1]]$status.matrix$infected == -1, ])
table(epi.outbreak[[1]]$status.matrix$infected)
table(epi.outbreak[[1]]$status.matrix$already.isolated)
table(epi.outbreak[[1]]$status.matrix$Category, epi.outbreak[[1]]$status.matrix$VaccStatus)
# table(epi.outbreak[[1]]$status.matrix$SchoolYear, epi.outbreak[[1]]$status.matrix$infected)
# table(epi.outbreak[[1]]$status.matrix$Category, epi.outbreak[[1]]$status.matrix$infected)
# summary(epi.outbreak[[1]]$status.matrix$q); hist(epi.outbreak[[1]]$status.matrix$q)
# summary(epi.outbreak[[1]]$status.matrix$R.ctcs)

events.sim <- epi.outbreak[[1]]$time.events[-1,]
events.sim <- data.frame(time = as.numeric(events.sim[,1]),
                         event = events.sim[,2],
                         id = as.numeric(events.sim[,3]))
events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
table(events.sim$event)

length(unique(events.sim$id[events.sim$event=='IsolationContact']))

length(unique(events.sim$id[events.sim$event=='Infector']))

events.sim[events.sim$event == 'IsolationContact', ]
ids.inf <- events.sim$id[which(events.sim$event == 'Infection')]
table(events.sim$event[events.sim$id %in% ids.inf])
ids.inf.pev <- ids.inf[which(ids.inf %in% events.sim$id[events.sim$event=='ContactPEV'])]
ids.inf.pev[which(ids.inf.pev %in% events.sim$id[events.sim$event=='Infector'])]
# unique(events.sim$id[events.sim$event=='Infector'])
events.sim[events.sim$id == 49, ]

## If received PEV they should never be isolated as contact
ids.pev <- events.sim$id[events.sim$event=='ContactPEV']
ids.pev[ids.pev %in% events.sim$id[events.sim$event=='IsolationContact']]

## Proportion of cases where PEV was effective?
id.index <-  which(epi.outbreak[[1]]$status.matrix$IndexCase==1)
ids.inf <- unique(events.sim$id[events.sim$event == 'Infection'])
ids.inf <- ids.inf[ids.inf != id.index]
ids.inf.pev.eff <- ids.inf[ids.inf %in% events.sim$id[events.sim$event == 'EffectivePEV']]
propEffectivePEV <- ifelse(length(ids.inf.pev.eff) > 0,
                           length(unique(ids.inf.pev.eff)) / length(unique(ids.inf)),
                           0)

## Onward transmission after PEV?
# ANY onward transmission from infectious individuals who received PEV after they were infected
ids.pev <- unique(events.sim$id[events.sim$event == 'ContactPEV'])
ids.inf.pev <- unique(ids.pev[ids.pev %in% events.sim$id[events.sim$event == 'Infector']])
inf.after.pev <- rep(0, length(ids.inf.pev))
if(length(ids.inf.pev) > 0){
  for( i in 1:length(ids.inf.pev)){
    inf.times <- events.sim$time[events.sim$event == 'Infector' & events.sim$id == ids.inf.pev[i]]
    infection.time <- epi.outbreak[[1]]$status.matrix$time.of.infection[epi.outbreak[[1]]$status.matrix$id == ids.inf.pev[i]]
    pev.time <- events.sim$time[events.sim$event == 'ContactPEV' & events.sim$id == ids.inf.pev[i]]
    if(infection.time < pev.time){
      timediff <- inf.times - pev.time
      if(any((timediff[!is.na(timediff)]) > 0)){
        inf.after.pev[i] <- 1
      }
    }
  }
}
propOnwardAfterPEV <- ifelse(length(ids.inf.pev) > 0,
                             sum(inf.after.pev) / (sum(!is.na(epi.outbreak[[1]]$status.matrix$time.of.infection)) - 1),
                             0)

epi.outbreak[[1]]$status.matrix$TimeRashOnset[epi.outbreak[[1]]$status.matrix$id==169]
epi.outbreak[[1]]$status.matrix$TimeSymptomOnset[epi.outbreak[[1]]$status.matrix$id==159]

# Check that no one is isolated as a confirmed case twice
library(dplyr)
check.out <- events.sim %>% 
  group_by(id, event) %>%
  count()
summary(check.out$n[check.out$event=='Isolation'])
summary(check.out$n[check.out$event=='Detection'])
summary(check.out$n[check.out$event=='Infection'])

ids.infected <- events.sim$id[events.sim$event == 'Infection']
ids.isolated <- events.sim$id[events.sim$event == 'IsolationContact']
ids.isolated[which((ids.isolated %in% ids.infected))]
# events.sim[events.sim$id %in% ids.isolated[which((ids.isolated %in% ids.infected))], ]
events.sim[events.sim$id == 43, ]
epi.outbreak[[1]]$status.matrix$TimeRashOnset[epi.outbreak[[1]]$status.matrix$id==195]

#-------------------------------------------------------------------------------------------------------------------------------
# Check how many exposed contacts got infected and returned from isolation while still infectious -> potential for transmission
# Detected cases are never infectious after coming back from isolation
ids.infected <- events.sim$id[events.sim$event == 'Infection']
ids.infected.rash <- NULL
ids.infected.backfromiso <- list()
ids.infectious.after.iso <- c()
for(i in 1:length(ids.infected)){
  ids.infected.rash[i] <- epi.outbreak[[1]]$status.matrix$TimeRashOnset[epi.outbreak[[1]]$status.matrix$id == ids.infected[i]]
  back.iso <- events.sim$time[events.sim$event=='BackFromIso' & events.sim$id == ids.infected[i]]
  if(length(back.iso) > 0){
    ids.infected.backfromiso[[i]] <- back.iso
    
    if(any(back.iso >= (ids.infected.rash[i] - 4) & back.iso <= (ids.infected.rash[i] + 4))){
      ids.infectious.after.iso <- c(ids.infectious.after.iso, ids.infected[i])
    }
  }
}
# ids.infectious.after.iso
# ids.infected
# ids.infected.backfromiso
# ids.infected.rash

pInfectious <- length(unique(ids.infectious.after.iso)) / length(unique(ids.infected))
pInfectious

events.sim[events.sim$id==147,]  
epi.outbreak[[1]]$status.matrix$TimeRashOnset[epi.outbreak[[1]]$status.matrix$id==147]

##------------------------------------------------------------------------------------------------------------------------
## Effective reproduction number Rt: number of secondary cases generated by a case that was infected at time t
time_range <- seq(min(epi.outbreak[[1]]$status.matrix$time.of.infection), max(epi.outbreak[[1]]$status.matrix$time.of.infection))
Rt_res <- data.frame(time = time_range) %>%
  rowwise() %>%
  mutate(
    # count infectors
    infectors = sum(floor(epi.outbreak[[1]]$status.matrix$time.of.infection) == time, na.rm = T),
    Rt_forw = 0,
    Rt_backw = 0
  )
head(Rt_res)
for(i in 1:dim(Rt_res)[1]){
  
  t <- Rt_res$time[i]
  
  ## Forward Rt: secondary cases caused by individuals infected at time t
  n.infectors <- sum(floor(epi.outbreak[[1]]$status.matrix$time.of.infection[
    !is.na(epi.outbreak[[1]]$status.matrix$time.of.infection)]) == t)
  infectors <- which(floor(epi.outbreak[[1]]$status.matrix$time.of.infection) == t)
  # infectees
  n.infectees <- sum(epi.outbreak[[1]]$status.matrix$infector %in% infectors, na.rm = T)
  if(n.infectors > 0){
    Rt_res$Rt_forw[i] <- n.infectees / n.infectors
  }
  
  ## Backward Rt: secondary cases caused by individuals who were infectious at time t
  # possible infectors
  infectors <- which((epi.outbreak[[1]]$status.matrix$TimeRashOnset - 4) <= t &
                       (epi.outbreak[[1]]$status.matrix$TimeRashOnset + 4) >= t)
  n.infectors <- length(infectors)
  # infectees
  n.infectees <- sum(epi.outbreak[[1]]$status.matrix$infector %in% infectors, na.rm = T)
  # n.infectees <- sum(floor(epi.outbreak[[1]]$status.matrix$time.of.infection[
  #   !is.na(epi.outbreak[[1]]$status.matrix$time.of.infection)]) == t)
  if(n.infectors > 0){
    Rt_res$Rt_backw[i] <- n.infectees / n.infectors
  }
}
# Plot Rt over time, w/o index
ggplot(Rt_res[-1,], aes(x = time, y = Rt_forw)) +
  # geom_line(color = 'blue', size = 1) +
  # geom_line(aes(x = time, y = Rt_backw), color = 'green', size = 1) +
  geom_smooth(color = 'blue', size = 1, se = F) +
  geom_smooth(aes(x = time, y = Rt_backw), color = 'green', size = 1, se = F) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  labs(
    title = 'Time-varying effective reproduction number (Rt)',
    x = 'Time',
    y = 'Rt'
  ) +
  theme_minimal()
mean(Rt_res$Rt_forw)
mean(Rt_res$Rt_backw)

# Rt <- NULL
# # Order the infection time in a variable and for each infection time reported (i.e., people infected at that moment), 
# # compute the Rt
# inf.indiv <- epi.outbreak[[1]]$status.matrix$id[!is.na(epi.outbreak[[1]]$status.matrix$time.of.infection)]
# inf.times <- epi.outbreak[[1]]$status.matrix$time.of.infection[inf.indiv]
# # inf.time <- inf.time[order(inf.time)]
# inf.indiv <- inf.indiv[order(inf.times)]
# for(i in inf.indiv){
#   infectees <- which(epi.outbreak[[1]]$status.matrix$infector == i) # secondary cases for each infected individual
#   Rt.temp <- 0
#   if(length(infectees) > 0){
#     for(i1 in 1:length(infectees)){
#       if(epi.outbreak[[1]]$status.matrix$time.of.infection[infectees[i1]] >
#          epi.outbreak[[1]]$status.matrix$time.of.infection[i]){
#         Rt.temp <- Rt.temp + 1
#       }
#     }
#   }
#   # Number of infectious individuals at time t
#   t <- epi.outbreak[[1]]$status.matrix$time.of.infection[i]
#   infs <- which(((epi.outbreak[[1]]$status.matrix$TimeRashOnset - 4) <= t &
#                    (epi.outbreak[[1]]$status.matrix$TimeRashOnset + 4) >= t) & 
#                   !is.infinite(epi.outbreak[[1]]$status.matrix$TimeRashOnset))
#   if(length(infs) > 0){
#     Rt.temp <- Rt.temp / length(infs)
#   }
#   Rt <- rbind(Rt, c(epi.outbreak[[1]]$status.matrix$time.of.infection[i], Rt.temp))
# }
# Rt
# mean(Rt[,2])
# #plot w/o index case
# # Plot Rt over time
# Rt <- as.data.frame(Rt[-1, ])
# ggplot(Rt, aes(x = V1, y = V2)) +
#   geom_line(color = 'blue', size = 1) +
#   geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
#   labs(
#     title = 'Time-varying effective reproduction number (Rt)',
#     x = 'Time',
#     y = 'Rt'
#   ) +
#   theme_minimal()

# comp.RT<-function(status.matrix,individual,Rt){
#   infectees<-which(status.matrix$infector==individual) #people infected by individual
#   Rt.temp<-0
#   if (length(infectees)>0){
#     for (i1 in 1:length(infectees)){
#       if (status.matrix$time.of.infection[infectees[i1]]>status.matrix$time.of.infection[individual]){
#         Rt.temp<-Rt.temp+1
#       }
#     }
#   }
#   Rt<-rbind(Rt,c(status.matrix$time.of.infection[individual],Rt.temp))
#   return(Rt)
# }


# Superspreading?
events.sim[events.sim$event=='Infector',]
epi.outbreak[[1]]$status.matrix$q[epi.outbreak[[1]]$status.matrix$id==347]

ids.infected <- events.sim$id[events.sim$event == 'Infection']
ids.isolated <- events.sim$id[events.sim$event == 'IsolationContact']
# Isolated due to exposure, not infected
events.sim[events.sim$id %in% ids.isolated[which(!(ids.isolated %in% ids.infected))], ]
# events.sim[events.sim$id %in% ids.isolated[which((ids.isolated %in% ids.infected))], ]
max(events.sim$time)
epi.outbreak[[1]]$status.matrix[ids.isolated[which(!(ids.isolated %in% ids.infected))], 1] # only '0' will actually be isolated after exposure

## Contact network
# if (netw==1){
for (i in 1:nSim) {
  ctc.betw<-rep(NA,n)
  temp.ctc.betw<-epi.outbreak[[i]]$between.classes.contacts
  for (j in 1:length(temp.ctc.betw)){
    if (length(temp.ctc.betw[[j]][-1])>0){
      ctc.betw[j]<-paste(temp.ctc.betw[[j]][-1],collapse = ",")
    }
  }
  temp.status.matrix<-epi.outbreak[[i]]$status.matrix
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
  # write.csv(dfn, paste0(out,"InfNetw_Sim",i,".csv"))
}

# }

## Onward transmission from indivs who are isolated after exposure?
# Selecting those that were isolated as well as infected
ids.isol <- unique(events.sim$id[events.sim$event == 'Isolation'])
ids.isol.ct <- unique(events.sim$id[events.sim$event == 'IsolationContact'])
ids.inf <- unique(events.sim$id[events.sim$event == 'Infection'])
isolation.duration <- c()
isolation.duration.ct <- c()
isolation.duration.ct.inf <- c()
ids <- c(); ids.ct <- c(); ids.ct.inf <- c()

for(id in unique(events.sim$id)){
  # Isolation duration of infected individuals
  if((id %in% ids.isol) & (id %in% ids.inf)){
    if(id %in% unique(events.sim$id[events.sim$event == 'BackFromIso'])){
      ids <- c(ids, id)
      isolation.duration <- c(isolation.duration, 
                              events.sim$time[events.sim$id == id & events.sim$event == 'BackFromIso'] - 
                                events.sim$time[events.sim$id == id & events.sim$event == 'Isolation']
      )
    }
  }
  # Isolation duration of infected contacts
  if((id %in% ids.isol.ct) & (id %in% ids.inf)){
    if(id %in% unique(events.sim$id[events.sim$event == 'BackFromIso'])){
      ids.ct.inf <- c(ids.ct.inf, id)
      isolation.duration.ct.inf <- c(isolation.duration.ct,
                                     events.sim$time[events.sim$id == id & events.sim$event == 'BackFromIso'] - 
                                       events.sim$time[events.sim$id == id & events.sim$event == 'IsolationContact']
      )
    }
  }
  # Isolation duration of non-infected contacts
  if((id %in% ids.isol.ct) & (!(id %in% ids.inf))){
    if(id %in% unique(events.sim$id[events.sim$event == 'BackFromIso'])){
      ids.ct <- c(ids.ct, id)
      isolation.duration.ct <- c(isolation.duration.ct,
                                 events.sim$time[events.sim$id == id & events.sim$event == 'BackFromIso'] - 
                                   events.sim$time[events.sim$id == id & events.sim$event == 'IsolationContact']
      )
    }
  }
}
table(isolation.duration) # should be 5
table(isolation.duration.ct) # should be t.isolation.q - delay.isolation
table(isolation.duration.ct.inf) # should vary



ids.inf.isol <- ids[which(isolation.duration > 5.0001)]
# table(events.sim[events.sim$event == 'Infector' & events.sim$id == ids.inf.isol])
table(events.sim$event[events.sim$id %in% ids.inf.isol]) # if this includes 'Infector', there has been onward transmission

events.sim$id[events.sim$id %in% ids.inf.isol & events.sim$event == 'Infector']
events.sim[events.sim$id == 4, ]

###---------------------------------------------------------------------------------
### PROBABILITY OF ONWARD TRANSMISSION

# Proportion of infected cases that cause secondary case, incl seeds
prop.onward <- length(unique(events.sim$id[events.sim$event=='Infector'])) / sum(!is.na(dfn$InfectionTime))

# Proportion of infected cases that cause secondary case, excl seeds
table(dfn$IsIndex)
id.index <- dfn$ID[dfn$IsIndex == 1]
prop.onward.noSeed <- length(unique(events.sim$id[events.sim$event=='Infector' & events.sim$id != id.index])) / (sum(!is.na(dfn$InfectionTime))-1)

# Proportion of isolated infected cases that cause secondary case AFTER their initial isolation
ids.isol <- unique(events.sim$id[events.sim$event == 'BackFromIso'])
ids.inf.isol <- unique(ids.isol[ids.isol %in% events.sim$id[events.sim$event == 'Infector']])

inf.after.isol <- rep(0, length(ids.inf.isol))
for(i in 1:length(ids.inf.isol)){
  inf.times <- events.sim$time[events.sim$event=='Infector' & events.sim$id == ids.inf.isol[i]]
  backfromiso.times <- events.sim$time[events.sim$event=='BackFromIso' & events.sim$id == ids.inf.isol[i]][1]
  timediff <- inf.times - backfromiso.times
  if(any((timediff[!is.na(timediff)]) > 0)){
    inf.after.isol[i] <- 1
  }
  
}
inf.after.isol

prop.onward.after.isolation <- sum(inf.after.isol) / (sum(!is.na(dfn$InfectionTime))-1)


prop.onward; prop.onward.noSeed; prop.onward.after.isolation

#----------------------------------------------------------------------------------

ids[which(isolation.duration > 4.000001)]
events.sim[events.sim$id == 351, ]
dfn[dfn$ID == 208, ]

# Everyone isolated at t = 14
table(events.sim$time[events.sim$event == 'Isolation'])
summary(epi.outbreak[[1]]$status.matrix$time.of.infection)
events.sim[events.sim$time == 14 & events.sim$event == 'Isolation', ]
dfn[dfn$ID %in% events.sim$id[events.sim$time == 14 & events.sim$event == 'Isolation'], c(1:10)]

events.sim[events.sim$time != 14 & events.sim$event == 'Isolation', ]
dd <- dfn[dfn$ID %in% events.sim$id[events.sim$time != 14 & events.sim$event == 'Isolation'], c(1:10)]
dd
summary(dd$DayPosTest - dd$InfectionTime)
events.sim[events.sim$id == 137, ]

summary(events.sim$time[events.sim$event == 'Isolation'])
## Only those exposed to seed cases are isolated??
ddd <- dfn[dfn$ID %in% events.sim$id[events.sim$event == 'Isolation'] & is.na(dfn$InfectionTime), c(1:10)]
ids <- ddd$ID
events.sim[events.sim$id %in% ids, ]
events.sim[events.sim$id %in% ddd$ID[ddd$isImmune==0], ]

## OUTPUT

finalSize <- NULL
finalSizeProp <- NULL
n.detectedpositive <- NULL
n.immune <- NULL
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
  # ## Peak number of cases
  # events.sim <- epi.outbreak[[i_sim]]$time.events[-1, ]
  # events.sim <- data.frame(time = as.numeric(events.sim[,1]),
  #                          event = events.sim[,2],
  #                          id = as.numeric(events.sim[,3]))
  # events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
  # events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
  # epi.curve<-0
  # for (t in 1:length(events.sim[,1]) ){
  #   # number of infected at each step
  #   epi.curve[t]<-length(which(events.sim[1:t,2]=='Infection'))
  # }
  # casesPeak[i_sim] <- max(epi.curve)
}



