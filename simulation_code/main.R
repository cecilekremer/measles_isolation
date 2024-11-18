

## Load functions
source('simulation_code/simFunctions.R')
source('simulation_code/measlesSimulator.R')

seeding.time = Inf # no additional seeding
child.seed.arg = 5
prop.lambda.w = 1
rho.ch = 1 # prob of symptomatic child
rho.ad = 1 # prob of symptomatic adult
school.size = 420

R.s = 9.9 # R0; https://www.sciencedirect.com/science/article/pii/S1473309917303079#cesec60 
# R.ctcs, # ?
alpha.as = 1
prop.immune.ch = 0.5 # prop vaccinated/immune -> status.matrix$infected = -2
prop.immune.ad = 1 # prop vaccinated/immune
#suscep.ch: child susceptibility, relative to adult susceptibility
suscep.ch = 1
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
                                           test.sens.as = 1, test.sens.s = 1, 
                                           test.delay.s = test.delay.s, test.delay.as = test.delay.as, 
                                           t.isolation = t.isolation, # isolation for 4 days after rash onset
                                           t.isolation.q = 10, # isolation after exposure)
                                           bool.log.contacts = TRUE
  )
}

# head(epi.outbreak[[1]]$status.matrix[epi.outbreak[[1]]$status.matrix$infected == -1, ])
table(epi.outbreak[[1]]$status.matrix$infected)

events.sim <- epi.outbreak[[1]]$time.events[-1,]
events.sim <- data.frame(time = as.numeric(events.sim[,1]),
                         event = events.sim[,2],
                         id = as.numeric(events.sim[,3]))
events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
table(events.sim$event)

ids.infected <- events.sim$id[events.sim$event == 'Infection']
ids.isolated <- events.sim$id[events.sim$event == 'Isolation']
events.sim[events.sim$id %in% ids.isolated[which(!(ids.isolated %in% ids.infected))], ]
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
ids.isol <- unique(events.sim$id[events.sim$event == 'Isolation'])
ids.inf <- unique(events.sim$id[events.sim$event == 'Infection'])
isolation.duration <- c()
ids <- c()
for(id in unique(events.sim$id)){
  if((id %in% ids.isol) & (id %in% ids.inf)){
    if(id %in% unique(events.sim$id[events.sim$event == 'BackFromIso'])){
      ids <- c(ids, id)
      isolation.duration <- c(isolation.duration, 
                              events.sim$time[events.sim$id == id & events.sim$event == 'BackFromIso'] - events.sim$time[events.sim$id == id & events.sim$event == 'Isolation']
      )
    }
  }
}
table(isolation.duration)
ids.inf.isol <- ids[which(isolation.duration == 10)]
# table(events.sim[events.sim$event == 'Infector' & events.sim$id == ids.inf.isol])
table(events.sim$event[events.sim$id == ids.inf.isol]) # if this includes 'Infector'

ids[which(isolation.duration > 10)]
events.sim[events.sim$id == 1, ]
dfn[dfn$ID == 1, ]

# Everyone isolated at t = 14
table(events.sim$time[events.sim$event == 'Isolation'])
events.sim[events.sim$time == 14 & events.sim$event == 'Isolation', ]
dfn[dfn$ID %in% events.sim$id[events.sim$time == 14 & events.sim$event == 'Isolation'], c(1:10)]

events.sim[events.sim$time != 14 & events.sim$event == 'Isolation', ]
dd <- dfn[dfn$ID %in% events.sim$id[events.sim$time != 14 & events.sim$event == 'Isolation'], c(1:10)]
summary(dd$DayPosTest - dd$InfectionTime)
events.sim[events.sim$id == 137, ]

summary(events.sim$time[events.sim$event == 'Isolation'])
## Only those exposed to seed cases are isolated??
ddd <- dfn[dfn$ID %in% events.sim$id[events.sim$event == 'Isolation'] & is.na(dfn$InfectionTime), c(1:10)]
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
}



