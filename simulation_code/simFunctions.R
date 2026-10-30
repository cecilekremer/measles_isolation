
## Function to compute Rt after simulations
comp.RT<-function(status.matrix,individual,Rt){
  infectees<-which(status.matrix$infector==individual) #people infected by individual
  Rt.temp<-0
  if (length(infectees)>0){
    for (i1 in 1:length(infectees)){
      if (status.matrix$time.of.infection[infectees[i1]]>status.matrix$time.of.infection[individual]){
        Rt.temp<-Rt.temp+1
      }
    }
  }
  Rt<-rbind(Rt,c(status.matrix$time.of.infection[individual],Rt.temp))
  return(Rt)
}

# fun.SymptomOnset: Function to simulate incubation period (infection to first symptoms)
fun.SymptomOnset <- function(t = 10, median = 12.5, sd = 2.65, fixed = TRUE){
  if(fixed == TRUE){
    return(median)
  }else{
    mulog = log(median)
    sdlog = sqrt(log(1 + (sd^2 / median^2)))
    return(rlnorm(1, mulog, sdlog))
  }
}

# fun.RashOnset: Function to simulate incubation period (infection to rash onset)
fun.RashOnset <- function(t = 14, lb = 3, ub = 5, fixed = TRUE){
  if(fixed == TRUE){
    return(t)
  }else{
    return(runif(1, min = lb, max = ub))
  }
}

# rashonsets <- c()
# symponsets <- c()
# for(i in 1:500){
#   symponsets <- c(symponsets, fun.SymptomOnset(fixed = FALSE))
#   rashonsets <- c(rashonsets, symponsets[i] + fun.RashOnset(fixed = FALSE))
# }
# hist(rashonsets); hist(symponsets)
# summary(rashonsets); summary(symponsets)

# # fun.SymptmPeriod: Function to simulate the infectious period length
# fun.SymptmPeriod <- function(t = 9, fixed = TRUE){
#   if(fixed == TRUE){
#     return(t)
#   }else{
#     stop('not implemented yet')
#   }
# }

# fun.ctRate : Function for contact rate accounting for overdispersion; no overd: contact rate = lambda
fun.ctRate <- function(Rs, k.overd, lambda, dist = 'NB'){
  if(dist == 'NB'){
    nctr <- rnbinom(1, size = k.overd, mu = Rs)*lambda/Rs
  }
  
  return(nctr)
}

# ctRates <- c()
# for(i in 1:1000){
#   ctRates <- c(ctRates, fun.ctRate(9.9, 0.1, 16, 'NB'))
# }
# hist(ctRates); summary(ctRates)

# fun.Rs : Function for reproduction number accounting for overdispersion
fun.Rs <- function(Rs, k.overd, dist = 'NB'){
  if(dist == 'NB'){
    r <- rnbinom(1, size = k.overd, mu = Rs)
  }
  
  return(r)
}

# R0s <- c()
# for(i in 1:1000){
#   R0s <- c(R0s, fun.Rs(9.9, Inf, 'NB'))
# }
# hist(R0s); summary(R0s)

# fun.Infectiousness: Function for the infectiousness profile
fun.Infectiousness <- function(t.curr, t.rash.onset, const = TRUE){
  if(t.curr < (t.rash.onset - 4) | t.curr > (t.rash.onset + 4)){
    return(0)
  }else{ # infectious between 4 days before and 4 days after rash onset
    if(const == TRUE){
      return(1)
    }else if(const == FALSE){
      dtri(t.curr, min = t.rash.onset - 4.01, max = t.rash.onset + 4.01, mode = t.rash.onset)
    }
  }
}

# fun.InfectDuration: Function for the duration of infection (rash onset time to recovery time)
# set as recovered as soon as they are not infectious anymore?
fun.InfectDuration <- function(t = 14, fixed = TRUE){
  if(fixed == TRUE){
    return(t)
  }else{
    stop('not implemented yet')
  }
}

# is.positive: Function that simulates the test result
#
# This is modeled to follow a gamma distribution.
#
# INPUT:
# individual: ID of the individual that is tested
# sensitivity.as: sensitivity if asymptomatic
# sensitivity.s: sensitivity if symptomatic
# t.detection: minum amount of time from the infection day to be detectable
# current.time: current time
#
# OUTPUT:
# TRUE\FALSE 
# 

is.positive<-function(individual, sensitivity.as, sensitivity.s, t.detection, current.time, status.matrix){
  if (status.matrix[individual,1]==1){
    if ((current.time - status.matrix[individual,2]) > t.detection){ # if test performed at least t.detection days after infection
      # if (current.time < status.matrix[individual,5]){ 
      # if current time more than 4 days before or 7 days after rash onset
      if((current.time - status.matrix[individual, 5]) < -4 | (current.time - status.matrix[individual, 5]) > 7){
        if (rbinom(1,1,sensitivity.as)==1){return(TRUE)}else{return(FALSE)}
      }else{
        if (rbinom(1,1,sensitivity.s)==1){return(TRUE)}else{return(FALSE)}
      }
    }else{
      return(FALSE) 
    }
  }else{
    return(FALSE) 
  }
}

# class.composition: Vector with the size of the classes
classes.composition.function<-function(gam.shape, gam.scale,n.classes){
  return(rgamma(n.classes,shape = gam.shape, scale = gam.scale))  
}

#function to assign teachers to classes
assignTeacherToClasses<-function(status.matrix,classes.comp,n.teachers){
  teachers.id<-which(status.matrix$Category==0)
  count<-1
  for (j in teachers.id){
    if (count==(length(classes.comp)+1)){count<-1}
    status.matrix$TeacherAssign[j]<- count
    count<-count+1
  }
  return(status.matrix)
}

# function to sample from the given list, with defensive programming checks 
sample_cases <- function(id.potential,num.infections,diff.class,status.matrix){
  
  if(num.infections==0){
    return(NULL)
  }
  
  if (length(id.potential)==1){
    id.vector <- id.potential
  } else {
    # should cases be introduced in different classes
    if(diff.class == FALSE){
      id.vector<-sample(id.potential,min(num.infections,length(id.potential)), replace = FALSE)
    }else if(diff.class == TRUE){
      id.vector<-rep(NA, num.infections)
      for(i in 1:num.infections){
        id.vector[i] <- sample(id.potential, 1, replace = FALSE)
        id.potential <- id.potential[-which(id.potential==id.vector[i])]
        class.tmp <- status.matrix$Category[status.matrix$id==id.vector[i]]
        id.potential <- id.potential[-which(id.potential %in% status.matrix$id[status.matrix$Category==class.tmp])]
      }
    }
  }
  
  return(id.vector)
}

# function to start infection for the individuals of the given id's at current.time
register_infected_cases <- function(id.vector, current.time, status.matrix, id.infector, overdispersion, k.overd, R.s, detectionSymp, m, fixed.incubation, daycare){
  
  for (i_id in id.vector){
    
    status.matrix$infected[i_id]             <- 1 
    status.matrix$time.of.infection[i_id]    <- current.time
    status.matrix$TimeSymptomOnset[i_id]     <- current.time + fun.SymptomOnset(median = m, fixed = fixed.incubation)
    status.matrix$TimeRashOnset[i_id]        <- status.matrix$TimeSymptomOnset[i_id] + fun.RashOnset(fixed = FALSE)
    status.matrix$recovery.vector[i_id]      <- status.matrix$TimeRashOnset[i_id] + fun.InfectDuration(t = 10, fixed = TRUE) # recovery time in days from rash onset
    status.matrix$total_infectionPeriod[i_id]<- status.matrix$recovery.vector[i_id] - current.time
    status.matrix$infectives[i_id]           <- 1
    
    status.matrix$infector[i_id] <- id.infector
    
    lambda.w <- status.matrix$contact_rate_within[i_id]
    lambda.b <- status.matrix$contact_rate_between[i_id]
    
    # default severity
    # status.matrix$severity[i_id]     <- 1
    
    if (runif(1) < status.matrix$rho[i_id]){ # if the indiviual is going to be symptomatic
      if(overdispersion == FALSE){
        status.matrix$q[i_id]  <- R.s/status.matrix$R.ctcs[i_id]  # a single q parameter for everyone
        if(status.matrix$q[i_id] > 1){
          status.matrix$q[i_id] <- 1
        }
      }else{
        status.matrix$q[i_id] <- fun.Rs(R.s, k.overd, 'NB')/status.matrix$R.ctcs[i_id]
        if(status.matrix$q[i_id] > 1){
          status.matrix$q[i_id] <- 1
        }
      }
      if (runif(1) < status.matrix$pdiagn[i_id]){ # if the individual is going to be diagnosed: detection between symptom onset and rash+d
        if(detectionSymp == 'default'){
          # detection between symptom onset and 3 days after rash
          status.matrix$detection.day[i_id]   <- runif(1, status.matrix$TimeSymptomOnset[i_id], status.matrix$TimeRashOnset[i_id]+3)  
        }else if(detectionSymp == 'symp.onset'){
          status.matrix$detection.day[i_id] <- status.matrix$TimeSymptomOnset[i_id]
        }else if(detectionSymp == 'dist'){
          # detection probability highest at onset of rash
          status.matrix$detection.day[i_id] <- rtri(1, min = status.matrix$TimeSymptomOnset[i_id], max = status.matrix$TimeRashOnset[i_id]+3,
                                                    mode = status.matrix$TimeRashOnset[i_id])
        }else if(detectionSymp == 'dist2'){
          # no later than rash onset
          status.matrix$detection.day[i_id] <- rtri(1, min = status.matrix$TimeSymptomOnset[i_id], max = status.matrix$TimeRashOnset[i_id],
                                                    mode = status.matrix$TimeRashOnset[i_id]-0.00000001)
        }else if(detectionSymp == 'rash.onset'){
          status.matrix$detection.day[i_id] <- status.matrix$TimeRashOnset[i_id]
        }
        # status.matrix$severity[i_id]        <- 2
      }
    }else{ # not symptomatic
      status.matrix$TimeSymptomOnset[i_id] <- Inf
      status.matrix$TimeRashOnset[i_id] <- Inf
      if(overdispersion == FALSE){
        status.matrix$q[i_id]<-R.s/status.matrix$R.ctcs[i_id]*alpha.as # relative infectiousness of asymptomatic
        if(status.matrix$q[i_id] > 1){
          status.matrix$q[i_id] <- 1
        }
      }else{
        status.matrix$q[i_id] <- fun.Rs(R.s, k.overd, 'NB')/status.matrix$R.ctcs[i_id]*alpha.as
        if(status.matrix$q[i_id] > 1){
          status.matrix$q[i_id] <- 1
        }
      }
    }
    time.events <<- rbind(time.events, c(current.time, 1, i_id))
    
    # register contacts 
    status.matrix$index.contact.within[i_id] <- 1  
    if (status.matrix$Category[i_id]!=0 & daycare==FALSE){ 
      status.matrix$index.contact.between[i_id]<-1  
    }
    
  } # end for-loop to seed infected
  
  return(status.matrix)
}

