
measles_simulator <- function(t.stop, seeding.time,
                              n.teachers, classes.comp,
                              seeds.ch, seeds.ad = 0, reseeding.ch, reseeding.ad = 0,
                              R.s = 9.9, # R0; https://www.sciencedirect.com/science/article/pii/S1473309917303079#cesec60 
                              overdispersion = FALSE, k.overd, # overdispersion parameter for the contact rate
                              # R.ctcs, # total contacts
                              alpha.as = 1,
                              prop.immune.ch1, # prop vaccinated/immune -> status.matrix$infected = -2 # year 1-3
                              prop.immune.ch2, # year 4-6
                              prop.immune.ad = 1, # prop vaccinated/immune
                              # lambda.w, 
                              lambda.b,
                              suscep.ch = 1,
                              rho.ch = 1, rho.ad = 1,
                              pdiagn.ch, pdiagn.ad,
                              t.detection = 3, # time it takes before an individual becomes detectable
                              detectionSymp = 'symp.onset', # should cases be diagnosed at time of symptom onset
                              median.incubation = 12.5,
                              fixed.incubation = FALSE,
                              const.infect = TRUE, # whether to use constant infectiousness or peaked around rash onset
                              test.sens.as, test.sens.s, test.delay.s, test.delay.as = 10000, 
                              delay.isolation = 0,
                              delay.PEV = 0,
                              isolate.once = FALSE,
                              t.isolation = 4, # isolation for 4 days after rash onset
                              t.isolation.q = 21, # isolation after exposure
                              daycare = FALSE, prop.infants = 0.3, days.present, children.present, weekend = FALSE,
                              PEV = FALSE, # post-exposure vaccination in schools
                              pev.accept = 0.9, pev.eff = 0.83, pev.duration.q = 0, 
                              bool.log.contacts = FALSE
){
  
  
  ##-------------------------
  ## SETUP POPULATION
  
  ## Adjust days.present to exclude weekends
  if(weekend == TRUE & daycare == TRUE){
    for(i in 1:length(days.present)){
      day_of_week <- days.present[[i]] %% 7
      days.present[[i]] <- days.present[[i]][!(day_of_week %in% c(5,6))]
    }
    # remove children.present on weekends
    for(j in 1:length(children.present)){
      if(((j-1) %% 7) %in% c(5,6)){
        children.present[[j]] <- NA
      }
    }    
  }
  
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
                              SchoolYear        = NA,
                              TeacherAssign     = 0,         #9
                              IndexCase         = 0,         #10
                              TimePosTest       = Inf,       #11
                              VaccStatus        = 0,          #12
                              R.ctcs            = 0,
                              id                = 1:n,
                              pev.accept        = 0, # willingness to accept PEV
                              PEV.status        = 0
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
  
  ## Set school years (1-3 + 4-6) -> half/half for now
  if(daycare == FALSE){
    status.matrix$SchoolYear <- ifelse(status.matrix$Category <= floor(n.class/2) & status.matrix$Category != 0, 1, 
                                       ifelse(status.matrix$Category != 0, 2, status.matrix$SchoolYear))  
  }else{
    infants <- sample(1:dim(status.matrix[status.matrix$Category == 1, ])[1], floor(prop.infants*n), replace = FALSE)
    status.matrix$SchoolYear[infants] <- 1 # 30% are infants under 1y 
    status.matrix$SchoolYear <- ifelse(status.matrix$Category == 0, NA, 
                                       ifelse(is.na(status.matrix$SchoolYear), 2, status.matrix$SchoolYear))
  }
  
  ## Within-class contact rate
  lambda.w <- NULL
  if(daycare == FALSE){
    for(c in 1:length(classes.comp)){
      ids.class <- c(which(status.matrix$Category == c), which(status.matrix$TeacherAssign == c))
      lambda.w[ids.class] <- classes.comp[c] - 1
    }    
  }else{
    lambda.w <- lengths(children.present)[1] + length(which(status.matrix$Category == 0)) - 1
  }
  
  ## Total contact rate
  status.matrix$R.ctcs <- lambda.w + lambda.b
  
  ## Proportion immune
  if(daycare == FALSE){
    ids.immune.1 <- sample(seq(min(which(status.matrix$SchoolYear == 1)), max(which(status.matrix$SchoolYear == 1)), 1), 
                           floor(prop.immune.ch1 * sum(status.matrix$SchoolYear == 1, na.rm = T)))
    ids.immune.2 <- sample(seq(min(which(status.matrix$SchoolYear == 2)), max(which(status.matrix$SchoolYear == 2)), 1),
                           floor(prop.immune.ch2 * sum(status.matrix$SchoolYear == 2, na.rm = T)))
  }else if(daycare == TRUE){
    ids.immune.1 <- sample(infants, ceiling(prop.immune.ch1 * sum(status.matrix$SchoolYear == 1, na.rm = T)))
    ids.immune.2 <- sample(status.matrix$id[status.matrix$SchoolYear == 2 & !is.na(status.matrix$SchoolYear)],
                           floor(prop.immune.ch2 * sum(status.matrix$SchoolYear == 2, na.rm = T)))
  }
  status.matrix$infected[ids.immune.1] <- (-2) # children in year 1-3 that are immune
  status.matrix$infected[ids.immune.2] <- (-2) # children in year 4-6 that are immune
  # status.matrix$infected[sample(1:sum(classes.comp), floor(prop.immune.ch * sum(classes.comp)))] <- (-2) # children that are immune
  status.matrix$infected[sample((sum(classes.comp)+1):n, floor(prop.immune.ad * n.teachers))] <- (-2) # adults that are immune
  # Set vaccination status
  status.matrix$VaccStatus[status.matrix$infected == (-2)] <- 1    
  
  ## PEV acceptance willingness: 90% of susceptibles will accept PEV
  if(PEV == TRUE){
    ids.accept.pev <- sample(status.matrix$id[status.matrix$infected == 0], floor(pev.accept * sum(status.matrix$infected == 0)))
    status.matrix$pev.accept[ids.accept.pev] <- 1
  }
  
  # }else{
  #   status.matrix$infected[sample(1:sum(classes.comp), floor(prop.immune.ch1 * sum(classes.comp)))] <- (-2) # children that are immune
  #   status.matrix$infected[sample((sum(classes.comp)+1):n, floor(prop.immune.ad * n.teachers))] <- (-2) # adults that are immune
  #   # Set vaccination status
  #   status.matrix$VaccStatus[status.matrix$infected == (-2)] <- 1
  # }
  
  ## Other population features
  status.matrix$recovery.vector       <- rep(NA, n) # recovery times
  status.matrix$detection.day         <- rep(Inf, n) # diagnosis times
  status.matrix$infectives            <- rep(0, n) # indicator for who is infectious at current time
  status.matrix$isolation             <- rep(0, n) # indicator for who is isolated at current time
  status.matrix$already.isolated      <- rep(0, n) # indicator for who has already been isolated 
  status.matrix$index.contact.within  <- rep(0, n) # indivs that have to propose a new contact
  status.matrix$index.contact.between <- rep(0, n) # indivs that have to propose a new contact
  status.matrix$positive.per.class    <- rep(0, n) # vector of size n.class (warning!)
  status.matrix$testing.day           <- rep(Inf, n)
  status.matrix$isolation.day         <- rep(Inf, n)
  status.matrix$back.from.isolation   <- rep(Inf, n)
  # status.matrix$det.timewindow
  # status.matrix$school.days.losts     <- rep(0, n)
  
  ## Log contacts
  status.matrix$log.cnt.within.ch     <- ''
  status.matrix$log.cnt.within.ad     <- ''
  status.matrix$log.cnt.between.ch    <- ''
  
  ## Transmission parameters, per individual
  # status.matrix$id                    <- 1:n
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
  
  # PEV administration
  status.matrix$pev.time <- rep(Inf, n)
  status.matrix$infected.contact <- NA
  
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
                       TestingDay   = Inf,
                       ContactPEV   = Inf
                       # ScheduleTest = Inf
  )
  
  ## Initialize events
  # events$NewSeeding      <- 0 # seed infected to start simulation
  
  # Initial seeding event
  nSeeds.child <- ifelse(current.time == 0, seeds.ch, reseeding.ch)
  nSeeds.adult <- ifelse(current.time == 0, seeds.ad, reseeding.ad)
  
  temp.suscept.ch <- which(status.matrix$Category != 0 & status.matrix$infected == 0)
  # seed should be present at day 0
  if(daycare == TRUE){
    temp.suscept.ch <- temp.suscept.ch[temp.suscept.ch %in% children.present[[floor(current.time) + 1]]]
  }
  temp.seeds.ch <- sample_cases(id.potential = temp.suscept.ch, num.infections = nSeeds.child, diff.class = F, status.matrix = status.matrix)
  
  temp.suscept.ad <- which(status.matrix$Category == 0 & status.matrix$infected == 0)
  temp.seeds.ad <- sample_cases(id.potential = temp.suscept.ad, num.infections = nSeeds.adult, diff.class = F, status.matrix = status.matrix)
  
  status.matrix <- register_infected_cases(id.vector = c(temp.seeds.ch, temp.seeds.ad),
                                           current.time,
                                           status.matrix,
                                           id.infector = NA, # index case imported
                                           overdispersion = overdispersion,
                                           k.overd = k.overd,
                                           R.s = R.s,
                                           detectionSymp = detectionSymp,
                                           m = median.incubation,
                                           fixed.incubation = fixed.incubation,
                                           daycare = daycare
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
  
  eventtimes <- c()
  
  while(sum(status.matrix$infectives, na.rm = T) > 0){ # while there are still infectives
    # while(current.time < t.stop){
    
    eventtimes <- c(eventtimes, current.time)
    ##----------------------------
    ## Set events
    
    ## Individuals who can, propose a new social contact(s)
    # Within class
    if(daycare == FALSE){
      for(i in which(status.matrix$index.contact.within == 1)){
        status.matrix$contact.time.within.pr.ctc[i] <- rexp(1, status.matrix$contact_rate_within[i]) + current.time # generate the next interarrival time for indiv i
        status.matrix$index.contact.within[i] <- 0
        
        # # no contacts on weekends
        if(weekend == TRUE){
          if((floor(status.matrix$contact.time.within.pr.ctc[i]) %% 7) == 5){
            status.matrix$contact.time.within.pr.ctc[i] <- (status.matrix$contact.time.within.pr.ctc[i]) + 2
          }else if((floor(status.matrix$contact.time.within.pr.ctc[i]) %% 7) == 6){
            status.matrix$contact.time.within.pr.ctc[i] <- (status.matrix$contact.time.within.pr.ctc[i]) + 1
          }else{
            status.matrix$contact.time.within.pr.ctc[i] <- status.matrix$contact.time.within.pr.ctc[i]
          }
        }
      }      
    }else if(daycare == TRUE){
      # index has to be present at the day of contact
      for(i in which(status.matrix$index.contact.within == 1)){
        mintime <- min(days.present[[i]][days.present[[i]]>floor(current.time)])
        status.matrix$contact.time.within.pr.ctc[i] <- rexp(1, status.matrix$contact_rate_within[i]) + current.time
        if(!(floor(status.matrix$contact.time.within.pr.ctc[i]) %in% days.present[[i]])){
          status.matrix$contact.time.within.pr.ctc[i] <- mintime + rexp(1, status.matrix$contact_rate_within[i])
        }
        status.matrix$index.contact.within[i] <- 0
      }
    }
    
    # Between class
    if(daycare == FALSE){
      for(i in which(status.matrix$index.contact.between == 1)){
        status.matrix$contact.time.between.pr.ctc[i] <- rexp(1, status.matrix$contact_rate_between[i]) + current.time
        status.matrix$index.contact.between[i] <- 0
        
        # # no contacts on weekends
        if(weekend == TRUE){
          if((floor(status.matrix$contact.time.between.pr.ctc[i]) %% 7) == 5){
            status.matrix$contact.time.between.pr.ctc[i] <- (status.matrix$contact.time.between.pr.ctc[i]) + 2
          }else if((floor(status.matrix$contact.time.between.pr.ctc[i]) %% 7) == 6){
            status.matrix$contact.time.between.pr.ctc[i] <- (status.matrix$contact.time.between.pr.ctc[i]) + 1
          }else{
            status.matrix$contact.time.between.pr.ctc[i] <- status.matrix$contact.time.between.pr.ctc[i]
          }
        }
      }
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
    events$ContactPEV   <- min(status.matrix$pev.time)
    
    # Select the next event
    next.evts <- colnames(events)[min(events) == events]
    if(length(next.evts) > 1){
      next.evts <- sample(next.evts, 1)
    }
    
    # if(next.evts != 'NextCtc'){stop()}
    
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
      temp.seeds.ch <- sample_cases(id.potential = temp.suscept.ch, num.infections = nSeeds.child, diff.class = F, status.matrix = status.matrix)
      
      temp.suscept.ad <- which(status.matrix$Category == 0 & status.matrix$infected == 0)
      temp.seeds.ad <- sample_cases(id.potential = temp.suscept.ad, num.infections = nSeeds.adult, diff.class = F, status.matrix = status.matrix)
      
      status.matrix <- register_infected_cases(id.vector = c(temp.seeds.ch, temp.seeds.ad),
                                               current.time,
                                               status.matrix,
                                               id.infector = NA, # index case imported
                                               overdispersion = overdispersion,
                                               k.overd = k.overd,
                                               R.s = R.s,
                                               detectionSymp = detectionSymp,
                                               m = median.incubation,
                                               fixed.incubation = fixed.incubation,
                                               daycare = daycare
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
          # status.matrix$Detected[person.detected] <- 1
          
          # If PCR test positive
          if(is.positive(individual = person.detected,
                         sensitivity.as = test.sens.as,
                         sensitivity.s = test.sens.s,
                         t.detection = t.detection,
                         current.time = current.time,
                         status.matrix = status.matrix)){
            
            status.matrix$Detected[person.detected] <- 1
            
            time.events    <<- rbind(time.events,
                                     c(current.time, 'Detection', person.detected))
            n.detectedpositive <- n.detectedpositive + 1
            status.matrix$TimePosTest[person.detected] <- current.time
            
            ## Isolate all individuals who test positive, regardless of symptoms (symptoms affect the duration of isolation later on though)
            status.matrix$isolation.day[person.detected] <- current.time + test.delay.s
            test.delay.classmates <- delay.isolation
            
            ## Isolate classmates but not for baseline scenario (t.isolation.q = 0)
            if(t.isolation.q > 0 | PEV == TRUE){
              
              # only isolate classmates IF case was present in class during the 4 days before or after rash, but isolation is always within 4 days after rash onset
              if(status.matrix$isolation.day[person.detected] >= (status.matrix$TimeRashOnset[person.detected] - 4)){
                
                ## SChool
                if(daycare == FALSE){
                  # Children: isolate unvaccinated/not immune classmates
                  if(status.matrix$Category[person.detected] > 0){
                    class.mates <- c(which(status.matrix$Category[person.detected] == status.matrix$Category))
                    # isolate those who are not vaccinated (i.e. not immune/recovered/infected)
                    class.mates <- class.mates[which(status.matrix$VaccStatus[class.mates] == 0)]
                    class.mates <- class.mates[which(status.matrix$PEV.status[class.mates] != 1)] # not received PEV yet
                    class.mates <- class.mates[which(status.matrix$infected[class.mates]!=(-1) & status.matrix$Detected[class.mates]!=1)] # already recovered or already detected
                    class.mates <- setdiff(class.mates, person.detected)
                    if(t.isolation.q > 0){
                      if(isolate.once == TRUE){
                        class.mates <- class.mates[which(status.matrix$already.isolated[class.mates] == 0)]
                      }
                      status.matrix$isolation.day[class.mates] <- current.time + test.delay.classmates
                    }
                  }else{
                    temp.cls <- status.matrix$TeacherAssign[person.detected]
                    class.mates <- c(which(status.matrix$Category == temp.cls), which(status.matrix$TeacherAssign == temp.cls))
                    # isolate those who are not vaccinated (i.e. not immune(-2)/recovered(-1)/infected(1))
                    class.mates <- class.mates[which(status.matrix$VaccStatus[class.mates] == 0)]
                    class.mates <- class.mates[which(status.matrix$PEV.status[class.mates] != 1)]
                    class.mates <- class.mates[which(status.matrix$infected[class.mates]!=(-1)  & status.matrix$Detected[class.mates]!=1)] # already recovered
                    class.mates <- setdiff(class.mates, person.detected)
                    if(t.isolation.q > 0){
                      if(isolate.once == TRUE){
                        class.mates <- class.mates[which(status.matrix$already.isolated[class.mates] == 0)]
                      }
                      status.matrix$isolation.day[class.mates] <- current.time + test.delay.classmates
                    }
                  }
                  ## If PEV has to be included (only for schools) --> adjust quarantine
                  if(PEV == TRUE){
                    for(c in class.mates){
                      status.matrix$infected.contact[c] <- person.detected
                      if(status.matrix$PEV.status[c] == 0 & status.matrix$pev.accept[c] == 1){
                        status.matrix$pev.time[c] = current.time + delay.PEV
                        if(pev.duration.q == 0){
                          status.matrix$isolation.day[c] <- Inf
                        }else{ # Otherwise, time of quarantine is aldready defined above?
                          status.matrix$isolation.day[c] <- current.time + delay.PEV + 1e-10 # to make sure PEV occurs first (some will become exempt from Q)
                        }
                      }
                    }
                  }
                  
                  ## Daycare: only isolate those that were present on the same day as the source during their infectious period
                }else if(daycare == TRUE){
                  # Children: isolate unvaccinated/not immune classmates
                  if(status.matrix$Category[person.detected] > 0){
                    class.mates <- c(which(status.matrix$Category[person.detected] == status.matrix$Category))
                    # isolate those who are not vaccinated (i.e. not immune/recovered/infected)
                    class.mates <- class.mates[which(status.matrix$VaccStatus[class.mates] == 0)]
                    class.mates <- class.mates[which(status.matrix$infected[class.mates]!=(-1) & status.matrix$Detected[class.mates]!=1)] # already recovered or detected
                    # only take those that were present on same days as index
                    index.present <- days.present[[person.detected]]
                    index.present.infect <- index.present[index.present >= floor(status.matrix$TimeRashOnset[person.detected] - 4)]
                    index.present.infect <- index.present.infect[index.present.infect <= ceiling(status.matrix$TimeRashOnset[person.detected] + 4)]
                    to.isol <- c()
                    for(d in index.present.infect){
                      to.isol <- c(to.isol, class.mates[which(class.mates %in% children.present[[d+1]])])
                    }
                    class.mates <- unique(to.isol)
                    class.mates <- setdiff(class.mates, person.detected)
                    status.matrix$isolation.day[class.mates] <- current.time + test.delay.classmates
                  }else{
                    temp.cls <- status.matrix$TeacherAssign[person.detected]
                    class.mates <- c(which(status.matrix$Category == temp.cls), which(status.matrix$TeacherAssign == temp.cls))
                    # isolate those who are not vaccinated (i.e. not immune(-2)/recovered(-1)/infected(1))
                    class.mates <- class.mates[which(status.matrix$VaccStatus[class.mates] == 0)]
                    class.mates <- class.mates[which(status.matrix$infected[class.mates]!=(-1)& status.matrix$Detected[class.mates]!=1)] # already recovered or detected
                    # only take those that were present on same days as index
                    index.present <- days.present[[person.detected]]
                    index.present.infect <- index.present[index.present >= floor(status.matrix$TimeRashOnset[person.detected] - 4)]
                    index.present.infect <- index.present.infect[index.present.infect <= ceiling(status.matrix$TimeRashOnset[person.detected] + 4)]
                    to.isol <- c()
                    for(d in index.present.infect){
                      to.isol <- c(to.isol, class.mates[which(class.mates %in% children.present[[d+1]])])
                    }
                    class.mates <- unique(to.isol)
                    class.mates <- setdiff(class.mates, person.detected)
                    status.matrix$isolation.day[class.mates] <- current.time + test.delay.classmates
                  }
                }
              }
            }
          }
          
          ## If already isolated because of exposure
        }else if(status.matrix$isolation[person.detected] == 1 & status.matrix$Detected[person.detected] == 0){
          
          # amountOfTest <- amountOfTest + 1
          # status.matrix$Detected[person.detected] <- 1
          
          # If PCR test positive
          if(is.positive(individual = person.detected,
                         sensitivity.as = test.sens.as,
                         sensitivity.s = test.sens.s,
                         t.detection = t.detection,
                         current.time = current.time,
                         status.matrix = status.matrix)){
            
            status.matrix$Detected[person.detected] <- 1
            
            time.events    <<- rbind(time.events,
                                     c(current.time, 'Detection', person.detected))
            n.detectedpositive <- n.detectedpositive + 1
            status.matrix$TimePosTest[person.detected] <- current.time
            
            ## Isolate all individuals who test positive, regardless of symptoms (symptoms affect the duration of isolation later on though)
            status.matrix$isolation.day[person.detected] <- current.time + test.delay.s
            
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
      if(t.isolation.q > 0 | pev.duration.q > 0){
        isolated.individuals <- which(status.matrix$isolation.day == events$Isolation)
      }else if(t.isolation.q == 0 & pev.duration.q == 0){
        isolated.individuals <- which(status.matrix$isolation.day == events$Isolation)
        ## individuals who are not detected should not be isolated in this case (no quarantine of contacts)
        status.matrix$isolation.day[isolated.individuals[which(status.matrix$Detected[isolated.individuals]==0)]] <- Inf
        # isolated.individuals <- which(status.matrix$isolation.day == events$Isolation & status.matrix$infected == 1)
        isolated.individuals <- which(status.matrix$isolation.day == events$Isolation)
      }
      status.matrix$isolation.day[isolated.individuals] <- Inf
      
      for(k in isolated.individuals){
        
        ## If individual is not already isolated as a contact
        if(status.matrix$isolation[k] == 0){
          
          status.matrix$isolation[k] <- 1
          
          # Isolated because of infection, i.e. is a detected case
          if(status.matrix$infected[k] == 1 & status.matrix$Detected[k] == 1){
            
            status.matrix$back.from.isolation[k] <- status.matrix$TimeRashOnset[k] + t.isolation + 1 # return the 5th day after rash onset
            time.events <<- rbind(time.events, c(current.time, 'Isolation', k))
            
            # can be that they have been previously detected, already back from isolation, but not 'recovered' yet -> shouldn't be isolated again
            if(!is.infinite(status.matrix$TimePosTest[k])){
              if(!is.infinite(status.matrix$TimeRashOnset[k])){
                # if no longer infectious
                if((status.matrix$TimeRashOnset[k] + 4) < current.time){
                  status.matrix$isolation[k] <- 0 # i.e. not really isolated
                  status.matrix$back.from.isolation[k] <- Inf
                }              
              }
            }
            
          }else{
            # Isolated because of exposure, doesn't matter if infected or not (not detected yet)
            status.matrix$back.from.isolation[k] <- current.time + (t.isolation.q - delay.isolation) # return on day 22
            # status.matrix$school.days.losts[k] <- status.matrix$school.days.losts[k] + t.isolation.q
            
            # If contact received PEV: shorter duration of isolation (regardless of whether it was effective or not)
            if(status.matrix$PEV.status[k] == 1){
              # if(pev.duration.q > delay.isolation){
              #   status.matrix$back.from.isolation[k] <- current.time + (pev.duration.q - delay.isolation)
              #   # status.matrix$school.days.losts[k] 
              # }else{
              #   status.matrix$back.from.isolation[k] <- current.time + pev.duration.q
              # }
              status.matrix$back.from.isolation[k] <- current.time + (pev.duration.q - delay.PEV)
            }
            
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
    ## event: Post-exposure vaccination
    
    if(next.evts == 'ContactPEV'){
      
      current.time <- events$ContactPEV
      
      pev.individuals <- which(status.matrix$pev.time == events$ContactPEV)
      status.matrix$pev.time[pev.individuals] <- Inf
      
      print(paste0(length(pev.individuals), " children are receiving PEV"))
      
      for(c in pev.individuals){ # classmates that are not yet vaccinated after exposure and not already experienced infection
        # PEV effectiveness depending on days since first exposure
        index.c <- status.matrix$infected.contact[c]
        print(paste0("Infector id ", index.c))
        exposure.time <- (status.matrix$TimeRashOnset[index.c] - 4)
        # shouldn't be a weekend day
        if(weekend == TRUE){
          if(floor(exposure.time)%%7 == 5){ # saturday
            exposure.time <- exposure.time + 2
          }
          if(floor(exposure.time)%%7 == 6){ # sunday
            exposure.time <- exposure.time + 1
          }
        }
        # If case was not already isolated 
        # if(status.matrix$isolation.day[index.c] > exposure.time){
        eventtime.id <- which(time.events[,2] == 'Isolation' & time.events[,3] == index.c)
        if(time.events[eventtime.id, 1] > exposure.time){  # if index is isolated after first exposure 
          
          time.events <<- rbind(time.events, c(current.time, "ContactPEV", c))
          
          # no isolation if Q for PEV is 0 --> ALREADY DEFINED ABOVE?
          # if(pev.duration.q == 0){
          #   status.matrix$isolation.day[c] <- Inf
          # }else{
          #   status.matrix$isolation.day[c] <- current.time + test.delay.classmates # Q can also be from day 9 onwards
          # }
          
          status.matrix$PEV.status[c] <- 1
          
          # if PEV taken within 3 days of first exposure and already infected
          if(status.matrix$infected[c] == 1){
            if(((current.time - exposure.time) <= 3) & (status.matrix$time.of.infection[c] >= exposure.time)){ # to make sure infection is from the identified index (and not from)
                                              # only effective (in preventing current infection) for individuals who are not yet infected by someone else more than 3 days ago
              # never quarantined if PEV within 72h (even if not effective due to vaccine failure)
              status.matrix$isolation.day[c] <- Inf
              if(rbinom(1, 1, pev.eff) == 1){
                ## Reset infection-related events
                status.matrix$recovery.vector[c] <- NA
                status.matrix$contact.time.between.pr.ctc[c] <- NA
                status.matrix$contact.time.within.pr.ctc[c]  <- NA
                status.matrix$index.contact.between[c]       <- 0
                status.matrix$index.contact.within[c]        <- 0
                status.matrix$infectives[c]                  <- 0
                status.matrix$detection.day[c]               <- Inf
                # status.matrix$isolation.day[c]               <- Inf
                # status.matrix$isolation[c]                   <- 0
                
                status.matrix$infected[c] <- (-1) # set immune-PEV; set to -1 to count them as a case
                status.matrix$VaccStatus[c] <- 1
                
                time.events <<- rbind(time.events, c(current.time, "EffectivePEV", c)) # effective in preventing current infection
              }else{
                #if PEV not effective: no need to do anything since they are immune upon recovery
                # but set to vaccinated just to keep track
                # can still make infectious contacts if not in Q
                if(rbinom(1, 1, 0.93) == 1){
                  status.matrix$VaccStatus[c] <- 1
                }
              }
            }else{ # this is only if they were already infected more than 3 days before first exposure (i.e. by someone else)
              if(rbinom(1, 1, 0.93) == 1){
                status.matrix$VaccStatus[c] <- 1 # set to vaccinated but don't change current 'infected' state; is later set to -1 upon recovery
              }
              # no quarantine if PEV given within 3 days
              if((current.time - exposure.time) <= 3){
                status.matrix$isolation.day[c] <- Inf
              }
            }
            # }
          }else if(status.matrix$infected[c] == 0){
            ## If not infected yet, vaccination prevents future infections
            if(rbinom(1, 1, 0.93) == 1){
              status.matrix$infected[c] <- (-2) # set immune-PEV w/ regular vaccine effectiveness for one dose; set to -2 as vaccinated never infected
              status.matrix$VaccStatus[c] <- 1
            }
            ## Also not quarantined if PEV given within 3 days
            if((current.time - exposure.time) <= 3){
              status.matrix$isolation.day[c] <- Inf
            }
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
        
        if(status.matrix$infected[w] == 1 & status.matrix$Detected[w] == 1){
          ## Individuals w/ rash onset before isolation (i.e. those diagnosed and then isolated) OR after coming out of isolation
          if((status.matrix$TimeRashOnset[w] <= (current.time - t.isolation-1)+0.00000001) | (status.matrix$TimeRashOnset[w] > current.time)){
            status.matrix$isolation[w] <- 0
            # status.matrix$Detected[w] <- 0 # do not reset detection status b/c they should not be isolated again if immune due to infection
            status.matrix$already.isolated[w] <- 1
            # Can still infect others after isolation
            status.matrix$index.contact.within[w] <- 1
            if(daycare == FALSE){
              if(status.matrix$Category[w] != 0){status.matrix$index.contact.between[w] <- 1}
            }
            time.events <<- rbind(time.events, c(current.time, 'BackFromIso', w))
          }else{
            ## DETECTED Individuals who developed rash during isolation are kept in isolation for 4 days after rash onset
            status.matrix$back.from.isolation[w] <- status.matrix$TimeRashOnset[w] + t.isolation + 1
            # status.matrix$school.days.losts[w]<-status.matrix$school.days.losts[w]+t.isolation
            time.events <<- rbind(time.events, c(current.time, 'StillInIso', w))
          }
        }else{ # Individuals who have not been detected (i.e. isolated because of contact with a case)
          status.matrix$isolation[w] <- 0
          # status.matrix$Detected[w] <- 0
          status.matrix$already.isolated[w] <- 1
          # Can still infect others after isolation, if infected
          if(status.matrix$infected[w] == 1){
            status.matrix$index.contact.within[w] <- 1
            if(daycare == FALSE){
              if(status.matrix$Category[w] != 0){status.matrix$index.contact.between[w] <- 1}
            }
          }
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
      # status.matrix$Detected[recovered] <- 0
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
        # School
        if(daycare == FALSE){
          if(status.matrix$Category[infector] == 0){ # if infector is an adult
            # get all children in the class
            temp.cls <- status.matrix$TeacherAssign[infector]
            infectee.pool <- c(which(status.matrix$Category == temp.cls, which(status.matrix$TeacherAssign == temp.cls)))
            if(bool.log.contacts) status.matrix$log.cnt.within.ad[infector] <- paste(status.matrix$log.cnt.within.ad[infector], current.time)
          }else{ # if infector is a child
            infectee.pool <- c(which(status.matrix$Category == status.matrix$Category[infector]), which(status.matrix$TeacherAssign == status.matrix$Category[infector]))
            if(bool.log.contacts) status.matrix$log.cnt.within.ch[infector] <- paste(status.matrix$log.cnt.within.ch[infector], current.time)
          }
          # Daycare: smaller infectee.pool
        }else if(daycare == TRUE){
          if(status.matrix$Category[infector] == 0){ # if infector is an adult
            # get all children present at that time
            temp.cls <- status.matrix$TeacherAssign[infector]
            infectee.pool <- c(which(status.matrix$Category == temp.cls, which(status.matrix$TeacherAssign == temp.cls)))
            infectee.pool <- infectee.pool[infectee.pool %in% children.present[[floor(current.time) + 1]]]
            if(bool.log.contacts) status.matrix$log.cnt.within.ad[infector] <- paste(status.matrix$log.cnt.within.ad[infector], current.time)
          }else{
            # infectee pool are those that are present at time t
            infectee.pool <- c(which(status.matrix$Category == status.matrix$Category[infector]), which(status.matrix$TeacherAssign == status.matrix$Category[infector]))
            infectee.pool <- infectee.pool[infectee.pool %in% children.present[[floor(current.time) + 1]]] # current.time + 1 because list starts at 1 instead of 0
            if(bool.log.contacts) status.matrix$log.cnt.within.ch[infector] <- paste(status.matrix$log.cnt.within.ch[infector], current.time)
          }
        }
      }else{ # only for schools
        ## Between-class contacts
        infector <- which(status.matrix$contact.time.between.pr.ctc == events$NextCtc)
        if(daycare == FALSE){ # no between contacts
          status.matrix$index.contact.between[infector]   <- 1
          status.matrix$contact.time.between.pr.ctc       <- NA          
        }
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
      acc.rate <- fun.Infectiousness(current.time, status.matrix$TimeRashOnset[infector], const = const.infect) *
        status.matrix$q[infector] * status.matrix$susceptibility[infectee]
      
      if(status.matrix$isolation[infectee] == 1){acc.rate <- 0} # if infectee is already isolated
      if(acc.rate > 1){err <- err + 1}
      # Set infected; only possible if not immune
      if(status.matrix$infected[infectee] == 0 & runif(1) < acc.rate){
        status.matrix <- register_infected_cases(id.vector = infectee,
                                                 current.time,
                                                 status.matrix,
                                                 id.infector = infector,
                                                 overdispersion = overdispersion,
                                                 k.overd = k.overd,
                                                 R.s = R.s,
                                                 detectionSymp = detectionSymp,
                                                 m = median.incubation,
                                                 fixed.incubation = fixed.incubation,
                                                 daycare = daycare)
        
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
    eventtimes = eventtimes,
    # n.tests = amountOfTest,
    # school.days.losts = sum(status.matrix$school.days.losts),
    n.detectedpositive = n.detectedpositive,
    between.classes.contacts = between.classes.contacts
  ))
  
  
}