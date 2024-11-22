
measles_simulator <- function(t.stop, seeding.time,
                              n.teachers, classes.comp,
                              seeds.ch, seeds.ad = 0, reseeding.ch, reseeding.ad = 0,
                              R.s = 9.9, # R0; https://www.sciencedirect.com/science/article/pii/S1473309917303079#cesec60 
                              overdispersion = FALSE, k.overd, # overdispersion parameter for the contact rate
                              R.ctcs, # total contacts
                              alpha.as = 1,
                              prop.immune.ch, # prop vaccinated/immune -> status.matrix$infected = -2
                              prop.immune.ad = 1, # prop vaccinated/immune
                              lambda.w, lambda.b,
                              suscep.ch = 1,
                              rho.ch = 1, rho.ad = 1,
                              pdiagn.ch, pdiagn.ad,
                              t.detection = 8, # time it takes before an individual becomes detectable
                              test.sens.as, test.sens.s, test.delay.s, test.delay.as = 10000, 
                              delay.isolation = 0,
                              t.isolation = 4, # isolation for 4 days after rash onset
                              t.isolation.q = 21, # isolation after exposure
                              bool.log.contacts = FALSE
){
  
  
  ##-------------------------
  ## SETUP POPULATION
  
  n.class <- length(classes.comp)
  n <- n.teachers + sum(classes.comp)
  
  # Matrix containing information about the state of the individuals
  status.matrix <- data.frame(infected          = rep(0, n), #1 # -2 = immune, -1 = recovered, 0 = susceptible, 1 = infectious
                              time.of.infection = NA,        #2
                              infector          = NA,        #3
                              TimeSymptomOnset  = Inf,       #4
                              TimeRashOnset     = Inf,       #5
                              TimeToDetection   = NA,        #6
                              Detected          = 0,         #7
                              Category          = 0,         #8
                              TeacherAssign     = 0,         #9
                              IndexCase         = 0,         #10
                              TimePosTest       = Inf,       #11
                              VaccStatus        = 0          #12
  )
  
  group.label <- 1:n.class
  class_id <- NULL
  for(i_id in 1:length(classes.comp)){
    class_id <- c(class_id, rep(i_id, classes.comp[i_id]))
  }
  
  status.matrix$Category[1:sum(classes.comp)] <- class_id # enumerate indivs, those w/ the same number belong to the same class
  status.matrix$Category[(sum(classes.comp)+1):n] <- 0 # teachers have Category 0
  status.matrix <- assignTeacherToClasses(status.matrix = status.matrix,
                                          classes.comp = classes.comp,
                                          n.teachers = n.teachers)
  
  # ## Within-class contact rate
  # lambda.w <- NULL
  # for(i_id in 1:n){
  #   lambda.w[1:sum(classes.comp)] <- 
  # }
  
  ## Proportion immune
  status.matrix$infected[sample(1:sum(classes.comp), floor(prop.immune.ch * sum(classes.comp)))] <- (-2) # children that are immune
  status.matrix$infected[sample((sum(classes.comp)+1):n, floor(prop.immune.ad * n.teachers))] <- (-2) # adults that are immune
  # Set vaccination status
  status.matrix$VaccStatus[status.matrix$infected == (-2)] <- 1
  
  
  ## Other population features
  status.matrix$recovery.vector       <- rep(NA, n) # recovery times
  status.matrix$detection.day         <- rep(Inf, n) # diagnosis times
  status.matrix$infectives            <- rep(0, n) # indicator for who is infectious at current time
  status.matrix$isolation             <- rep(0, n) # indicator for who is isolated at current time
  status.matrix$index.contact.within  <- rep(0, n) # indivs that have to propose a new contact
  status.matrix$index.contact.between <- rep(0, n) # indivs that have to propose a new contact
  status.matrix$positive.per.class    <- rep(0, n) # vector of size n.class (warning!)
  status.matrix$testing.day           <- rep(Inf, n)
  status.matrix$isolation.day         <- rep(Inf, n)
  status.matrix$back.from.isolation   <- rep(Inf, n)
  # status.matrix$det.timewindow
  # status.matrix$school.days.losts    
  
  ## Log contacts
  status.matrix$log.cnt.within.ch     <- ''
  status.matrix$log.cnt.within.ad     <- ''
  status.matrix$log.cnt.between.ch    <- ''
  
  ## Transmission parameters, per individual
  status.matrix$id                    <- 1:n
  status.matrix$q                     <- NA
  status.matrix$total_infectionPeriod <- NA
  status.matrix$contact_rate_within   <- lambda.w #fun.ctRate(R.s, k.overd, lambda.w, 'NB')  #lambda.w
  status.matrix$contact_rate_between  <- lambda.b #fun.ctRate(R.s, k.overd, lambda.b, 'NB')  #lambda.b
  status.matrix$susceptibility        <- 1
  
  # Age-specific attributes
  status.matrix$susceptibility[status.matrix$Category!=0] <- suscep.ch
  status.matrix$rho                                       <- rho.ad # prob of symptomatic adult
  status.matrix$rho[status.matrix$Category!=0]            <- rho.ch # prob of symptomatic child
  status.matrix$pdiagn                                    <- pdiagn.ad # prob of testing symptomatic adult
  status.matrix$pdiagn[status.matrix$Category!=0]         <- pdiagn.ch # prob of testing symptomatic child    
  
  # Adjust contact rate for adults (no between-class contacts)
  status.matrix$contact_rate_between[status.matrix$Category==0] <- 0
  
  ## Contacts: info on proposed time of next contact and the contacted individual
  status.matrix$contact.time.within.pr.ctc       <- NA
  status.matrix$contact.time.within.pr.infectee  <- NA
  status.matrix$contact.time.between.pr.ctc      <- NA
  status.matrix$contact.time.between.pr.infectee <- NA
  
  # Between-classes contact
  between.classes.contacts <- list()
  for(i in 1:n){
    between.classes.contacts[[i]] <- (-1)
  }
  is.between <- FALSE
  
  ##------------------------------------------------------
  ## SETUP EVENTS: structure to keep track of events
  
  current.time      <- 0
  time.events       <<- matrix(NA, 1, 3)
  
  events <- data.frame(NextCtc      = Inf,
                       Detection    = Inf,
                       Isolation    = Inf,
                       BackFromIso  = Inf,
                       Recovery     = Inf,
                       # ThresholdCheck = Inf,
                       NewSeeding   = Inf,
                       TestingDay   = Inf
                       # ScheduleTest = Inf
  )
  
  ## Initialize events
  # events$NewSeeding      <- 0 # seed infected to start simulation
  
  # Initial seeding event
  nSeeds.child <- ifelse(current.time == 0, seeds.ch, reseeding.ch)
  nSeeds.adult <- ifelse(current.time == 0, seeds.ad, reseeding.ad)
  
  temp.suscept.ch <- which(status.matrix$Category != 0 & status.matrix$infected == 0)
  temp.seeds.ch <- sample_cases(id.potential = temp.suscept.ch, num.infections = nSeeds.child)
  
  temp.suscept.ad <- which(status.matrix$Category == 0 & status.matrix$infected == 0)
  temp.seeds.ad <- sample_cases(id.potential = temp.suscept.ad, num.infections = nSeeds.adult)
  
  status.matrix <- register_infected_cases(id.vector = c(temp.seeds.ch, temp.seeds.ad),
                                           current.time,
                                           status.matrix,
                                           id.infector = NA, # index case imported
                                           overdispersion = overdispersion,
                                           k.overd = k.overd
  )
  status.matrix$IndexCase[temp.seeds.ch] <- 1
  status.matrix$IndexCase[temp.seeds.ad] <- 1
  
  # Schedule further seeding events
  events$NewSeeding <- current.time + seeding.time
  # events$ThresholdCheck  <- timewindow.closure # first threshold check for class/school closure
  
  ##------------------------------------------------------
  ## RUN SIMULATION
  
  # log variables
  err                <- 0
  # amountOfTest       <- 0
  n.detectedpositive <- 0
  # n.schoolclosure    <- 0
  # n.classclosure     <- 0
  
  while(sum(status.matrix$infectives, na.rm = T) > 0){ # while there are still infectives
    # while(current.time < t.stop){
    
    ##----------------------------
    ## Set events
    
    ## Individuals who can, propose a new social contact(s)
    # Within class
    for(i in which(status.matrix$index.contact.within == 1)){
      status.matrix$contact.time.within.pr.ctc[i] <- rexp(1, status.matrix$contact_rate_within[i]) + current.time # generate the next interarrival time for indiv i
      status.matrix$index.contact.within[i] <- 0
    }
    # Between class
    for(i in which(status.matrix$index.contact.between == 1)){
      status.matrix$contact.time.between.pr.ctc[i] <- rexp(1, status.matrix$contact_rate_between[i]) + current.time
      status.matrix$index.contact.between[i] <- 0
    }
    
    ## Aggregate contact times
    contact.time.overall <- c(status.matrix$contact.time.within.pr.ctc, status.matrix$contact.time.between.pr.ctc)
    
    ## Identify event times
    events$NextCtc      <- suppressWarnings(min(contact.time.overall, na.rm = T))
    events$Detection    <- min(status.matrix$detection.day)
    events$Isolation    <- min(status.matrix$isolation.day)
    events$BackFromIso  <- min(status.matrix$back.from.isolation)
    events$Recovery     <- suppressWarnings(min(status.matrix$recovery.vector, na.rm = T))
    events$TestingDay   <- min(status.matrix$testing.day)
    
    # Select the next event
    next.evts <- colnames(events)[min(events) == events]
    if(length(next.evts) > 1){
      next.evts <- sample(next.evts, 1)
    }
    
    print(next.evts)
    print(paste0('current time is ', current.time, '; there are ', sum(status.matrix$infected == 1, na.rm = T), ' infected individuals'))
    
    ##--------------------------------------------
    ## event: newSeeding
    
    if(next.evts == 'NewSeeding'){
      
      current.time <- events$NewSeeding
      # Schedule new seeding event
      events$NewSeeding <- current.time + seeding.time
      
      nSeeds.child <- ifelse(current.time == 0, seeds.ch, reseeding.ch)
      nSeeds.adult <- ifelse(current.time == 0, seeds.ad, reseeding.ad)
      
      temp.suscept.ch <- which(status.matrix$Category != 0 & status.matrix$infected == 0)
      temp.seeds.ch <- sample_cases(id.potential = temp.suscept.ch, num.infections = nSeeds.child)
      
      temp.suscept.ad <- which(status.matrix$Category == 0 & status.matrix$infected == 0)
      temp.seeds.ad <- sample_cases(id.potential = temp.suscept.ad, num.infections = nSeeds.adult)
      
      status.matrix <- register_infected_cases(id.vector = c(temp.seeds.ch, temp.seeds.ad),
                                               current.time,
                                               status.matrix,
                                               id.infector = NA, # index case imported
                                               overdispersion = overdispersion,
                                               k.overd = k.overd
      )
      status.matrix$IndexCase[temp.seeds.ch] <- 1
      status.matrix$IndexCase[temp.seeds.ad] <- 1
      
    }
    
    ##----------------------------------------
    ## event: TestingDay -> NOT NEEDED??
    
    
    ##----------------------------------------
    ## event: ScheduleTest -> NOT NEEDED??
    
    
    ##----------------------------------------
    ## event: Detection 
    ## individuals are 'detected' at their date of rash onset
    
    if(next.evts == 'Detection'){
      
      # if(current.time > 14) {stop('check isolated classmates')}
      
      current.time        <- events$Detection
      person.detected.all <- which(events$Detection == status.matrix$detection.day)
      status.matrix$detection.day[person.detected.all] <- Inf 
      
      for(person.detected in person.detected.all){
        
        # If not yet isolated AND not yet diagnosed
        if(status.matrix$isolation[person.detected] == 0 & status.matrix$Detected[person.detected] == 0){
          
          # amountOfTest <- amountOfTest + 1
          status.matrix$Detected[person.detected] <- 1
          
          # If PCR test positive
          if(is.positive(individual = person.detected,
                         sensitivity.as = test.sens.as,
                         sensitivity.s = test.sens.s,
                         t.detection = t.detection,
                         current.time = current.time,
                         status.matrix = status.matrix)){
            
            time.events    <<- rbind(time.events,
                                     c(current.time, 'Detection', person.detected))
            n.detectedpositive <- n.detectedpositive + 1
            status.matrix$TimePosTest[person.detected] <- current.time
            
            # If symptomatic (rash) when taking the test
            if(status.matrix$TimeRashOnset[person.detected] <= current.time){
              
              status.matrix$isolation.day[person.detected] <- current.time + test.delay.s
              # test.delay.classmates <- test.delay.s
              test.delay.classmates <- delay.isolation
              
              
            }else{ # if not symptomatic (rash) when taking the test
              
              status.matrix$isolation.day[person.detected] <- current.time + test.delay.as
              # test.delay.classmates <- test.delay.as
              test.delay.classmates <- delay.isolation
              
            }
            
            ## Isolate classmates, but not for baseline scenario (t.isolation.q = 0)
            if(t.isolation.q > 0){
              # Children: isolate unvaccinated/not immune classmates
              if(status.matrix$Category[person.detected] > 0){
                class.mates <- c(which(status.matrix$Category[person.detected] == status.matrix$Category))
                # isolate those who are not vaccinated (i.e. not immune/recovered/infected)
                class.mates <- class.mates[which(status.matrix$VaccStatus[class.mates] == 0)]
                class.mates <- class.mates[which(status.matrix$infected[class.mates]!=(-1))] # already recovered
                class.mates <- setdiff(class.mates, person.detected)
                status.matrix$isolation.day[class.mates] <- current.time + test.delay.classmates
              }else{
                temp.cls <- status.matrix$TeacherAssign[person.detected]
                class.mates <- c(which(status.matrix$Category == temp.cls), which(status.matrix$TeacherAssign == temp.cls))
                # isolate those who are not vaccinated (i.e. not immune(-2)/recovered(-1)/infected(1))
                class.mates <- class.mates[which(status.matrix$VaccStatus[class.mates] == 0)]
                class.mates <- class.mates[which(status.matrix$infected[class.mates]!=(-1))] # already recovered
                class.mates <- setdiff(class.mates, person.detected)
                status.matrix$isolation.day[class.mates] <- current.time + test.delay.classmates
              }
              
            }
          }
          
        }else if(status.matrix$isolation[person.detected] == 1 & status.matrix$Detected[person.detected] == 0){
          ## If already isolated because of exposure
          
          # amountOfTest <- amountOfTest + 1
          status.matrix$Detected[person.detected] <- 1
          
          # If PCR test positive
          if(is.positive(individual = person.detected,
                         sensitivity.as = test.sens.as,
                         sensitivity.s = test.sens.s,
                         t.detection = t.detection,
                         current.time = current.time,
                         status.matrix = status.matrix)){
            
            time.events    <<- rbind(time.events,
                                     c(current.time, 'Detection', person.detected))
            n.detectedpositive <- n.detectedpositive + 1
            status.matrix$TimePosTest[person.detected] <- current.time
            
            # If symptomatic (rash) when taking the test
            if(status.matrix$TimeRashOnset[person.detected] <= current.time){
              # Set 'new' isolation day to update isolation duration
              status.matrix$isolation.day[person.detected] <- current.time + test.delay.s
              
            }else{ # if not symptomatic (rash) when taking the test
              
              status.matrix$isolation.day[person.detected] <- current.time + test.delay.as
              
            }
          }
          # No further isolation needed (no contacts)
          
        }
        
      }
    }
    
    ##----------------------------------------------------------------
    ## event: Isolation
    
    if(next.evts == 'Isolation'){
      
      # if(current.time > 14) {stop('check isolated classmates')}
      
      current.time <- events$Isolation
      if(t.isolation.q > 0){
        isolated.individuals <- which(status.matrix$isolation.day == events$Isolation)
      }else if(t.isolation.q == 0){
        isolated.individuals <- which(status.matrix$isolation.day == events$Isolation)
        status.matrix$isolation.day[isolated.individuals[which(status.matrix$Detected[isolated.individuals]==0)]] <- Inf
        # isolated.individuals <- which(status.matrix$isolation.day == events$Isolation & status.matrix$infected == 1)
        isolated.individuals <- which(status.matrix$isolation.day == events$Isolation)
      }
      status.matrix$isolation.day[isolated.individuals] <- Inf
      
      for(k in isolated.individuals){
        
        if(status.matrix$isolation[k] == 0){
          
          status.matrix$isolation[k] <- 1
          
          if(status.matrix$infected[k] == 1 & status.matrix$Detected[k] == 1){
            # Isolated because of infection --> should be already detected
            status.matrix$back.from.isolation[k] <- status.matrix$TimeRashOnset[k] + t.isolation + 1 # return the 5th day after rash onset
            # status.matrix$school.days.losts[k] <- status.matrix$school.days.losts[k] + t.isolation
            time.events <<- rbind(time.events, c(current.time, 'Isolation', k))
            
            # }else if(status.matrix$infected[k] == 0){
          }else{
            # Isolated because of exposure, doesn't matter if infected or not (not detected yet)
            status.matrix$back.from.isolation[k] <- current.time + (t.isolation.q - delay.isolation) # return on day 22
            # status.matrix$school.days.losts[k] <- status.matrix$school.days.losts[k] + t.isolation.q
            time.events <<- rbind(time.events, c(current.time, 'IsolationContact', k))
            
          }
          # else{
          #   # Immune individuals: shouldn't be isolated in the first place??
          #   status.matrix$back.from.isolation[k] <- current.time
          # }
          
          status.matrix$contact.time.between.pr.ctc[k] <- NA
          status.matrix$contact.time.within.pr.ctc[k] <- NA
          status.matrix$index.contact.between[k] <- 0
          status.matrix$index.contact.within[k] <- 0
          
          # time.events <<- rbind(time.events, c(current.time, 'Isolation', k))
          
        }else if(status.matrix$isolation[k] == 1){
          ## To update isolation duration of cases detected during their quarantine
          if(status.matrix$infected[k] == 1 & status.matrix$Detected[k] == 1){
            status.matrix$back.from.isolation[k] <- status.matrix$TimeRashOnset[k] + t.isolation + 1
          }
        }
      }
    }
    
    ##----------------------------------------------------------------
    ## event: BackFromIsolation
    
    if(next.evts == 'BackFromIso'){
      
      current.time <- events$BackFromIso
      individuals.back.from.iso <- which(status.matrix$back.from.isolation == events$BackFromIso)
      status.matrix$back.from.isolation[individuals.back.from.iso] <- Inf    
      
      for(w in individuals.back.from.iso){
        
        if(status.matrix$infected[w] == 1){
          # ## If still infectious: re-start making contacts
          # is.infectious <- fun.Infectiousness(current.time, status.matrix$TimeRashOnset[w])
          # if(is.infectious){
          #   
          # }
          
          ## Individuals w/ rash onset before isolation (i.e. those diagnosed and then isolated) OR after coming out of isolation
          if((status.matrix$TimeRashOnset[w] <= (current.time - t.isolation)+0.00000001) | (status.matrix$TimeRashOnset[w] > current.time)){
            status.matrix$isolation[w] <- 0
            status.matrix$Detected[w] <- 0
            status.matrix$index.contact.within[w] <- 1
            if(status.matrix$Category[w] != 0){status.matrix$index.contact.between[w] <- 1}
            time.events <<- rbind(time.events, c(current.time, 'BackFromIso', w))
          }else{
            ## Individuals developing rash during isolation are kept in isolation for 4 days after rash onset
            status.matrix$back.from.isolation[w] <- status.matrix$TimeRashOnset[w] + t.isolation + 1
            # status.matrix$school.days.losts[w]<-status.matrix$school.days.losts[w]+t.isolation
            time.events <<- rbind(time.events, c(current.time, 'StillInIso', w))
          }
        }else{
          status.matrix$isolation[w] <- 0
          status.matrix$Detected[w] <- 0
          time.events <<- rbind(time.events, c(current.time, 'BackFromIso', w))
        }
      }
    }
    
    
    ##----------------------------------------------------------------
    ## event: Recovery
    
    if(next.evts == 'Recovery'){
      
      current.time <- events$Recovery
      recovered <- which(status.matrix$recovery.vector == events$Recovery)
      status.matrix$recovery.vector[recovered] <- NA
      
      ## Set as recovered = not detectable anymore
      status.matrix$infected[recovered] <- (-1)
      status.matrix$Detected[recovered] <- 0
      time.events <<- rbind(time.events, c(current.time, -1, recovered))
      
      ## Reset all infection-related event features
      status.matrix$contact.time.between.pr.ctc[recovered] <- NA
      status.matrix$contact.time.within.pr.ctc[recovered]  <- NA
      status.matrix$index.contact.between[recovered]       <- 0
      status.matrix$index.contact.within[recovered]        <- 0
      status.matrix$infectives[recovered]                  <- 0
      status.matrix$detection.day[recovered]               <- Inf
      status.matrix$isolation.day[recovered]               <- Inf
      status.matrix$isolation[recovered]                   <- 0
      
    }
    
    ##----------------------------------------------------------------
    ## event: NextCtc
    
    if(next.evts == 'NextCtc'){
      
      current.time <- events$NextCtc
      
      if(length(which(events$NextCtc == status.matrix$contact.time.within.pr.ctc)) > 0){
        ## Within-class contacts
        infector <- which(status.matrix$contact.time.within.pr.ctc == events$NextCtc)
        status.matrix$index.contact.within[infector] <- 1 # re-enable to schedule a contact event
        status.matrix$contact.time.within.pr.ctc[infector] <- NA
        
        if(status.matrix$Category[infector] == 0){ # if infector is an adult
          # get all children in the class
          temp.cls <- status.matrix$TeacherAssign[infector]
          infectee.pool <- c(which(status.matrix$Category == temp.cls, which(status.matrix$TeacherAssign == temp.cls)))
          if(bool.log.contacts) status.matrix$log.cnt.within.ad[infector] <- paste(status.matrix$log.cnt.within.ad[infector], current.time)
        }else{ # if infector is a child
          infectee.pool <- c(which(status.matrix$Category == status.matrix$Category[infector]), which(status.matrix$TeacherAssign == status.matrix$Category[infector]))
          if(bool.log.contacts) status.matrix$log.cnt.within.ch[infector] <- paste(status.matrix$log.cnt.within.ch[infector], current.time)
        }
      }else{
        ## Between-class contacts
        infector <- which(status.matrix$contact.time.between.pr.ctc == events$NextCtc)
        status.matrix$index.contact.between[infector]   <- 1
        status.matrix$contact.time.between.pr.ctc       <- NA
        
        is.between <- TRUE
        infectee.pool <- which(status.matrix$Category != status.matrix$Category[infector] & status.matrix$Category != 0)
        if(bool.log.contacts) status.matrix$log.cnt.between.ch[infector] <- paste(status.matrix$log.cnt.between.ch[infector], current.time)
      }
      
      ## Pick a random infectee
      infectee <- sample(setdiff(infectee.pool, infector), 1)
      if(is.between == TRUE){
        between.classes.contacts[[infector]] <- c(between.classes.contacts[[infector]], infectee)
        is.between <- FALSE
      }
      
      # Probability of effective contact: infectivity measure x transmission potential
      acc.rate <- fun.Infectiousness(current.time, status.matrix$TimeRashOnset[infector]) *
        status.matrix$q[infector] * status.matrix$susceptibility[infectee]
      
      if(status.matrix$isolation[infectee] == 1){acc.rate <- 0} # if infectee is already isolated
      if(acc.rate > 1){err <- err + 1}
      # Set infected
      if(status.matrix$infected[infectee] == 0 & runif(1) < acc.rate){
        status.matrix <- register_infected_cases(id.vector = infectee,
                                                 current.time,
                                                 status.matrix,
                                                 id.infector = infector,
                                                 overdispersion = overdispersion,
                                                 k.overd = k.overd)
        
        time.events <<- rbind(time.events, c(current.time, 'Infector', infector))
      }
      
    }
    
  }
  
  ##-------------------------------------------------------
  ## GET MODEL OUTPUT
  
  timev.name <- c('time','event','who')
  dimnames(time.events) <- list(NULL, timev.name)
  
  return(list(
    time.events = time.events,
    status.matrix = status.matrix,
    err = err,
    # n.tests = amountOfTest,
    # school.days.losts = sum(status.matrix$school.days.losts),
    n.detectedpositive = n.detectedpositive,
    between.classes.contacts = between.classes.contacts
  ))
  
  
}