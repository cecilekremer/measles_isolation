
# fun.SymptomOnset: Function to simulate incubation period (infection to first symptoms)
fun.SymptomOnset <- function(t = 10, fixed = TRUE){
  if(fixed == TRUE){
    return(t)
  }else{
    stop('not implemented yet')
  }
}

# fun.RashOnset: Function to simulate incubation period (infection to rash onset)
fun.RashOnset <- function(t = 14, fixed = TRUE){
  if(fixed == TRUE){
    return(t)
  }else{
    stop('not implemented yet')
  }
}

# fun.SymptmPeriod: Function to simulate the infectious period length
fun.SymptmPeriod <- function(t = 9, fixed = TRUE){
  if(fixed == TRUE){
    return(t)
  }else{
    stop('not implemented yet')
  }
}

# fun.Infectiousness: Function for the infectiousness profile
fun.Infectiousness <- function(t.curr, t.rash.onset){
  if(t.curr < (t.rash.onset - 4) | t.curr > (t.rash.onset + 4)){
    return(0)
  }else{ # infectious between 4 days before and 4 days after rash onset
    return(1)
  }
}

# fun.InfectDuration: Function for the duration of infection (rash onset time to recovery time)
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
sample_cases <- function(id.potential,num.infections){
  
  if(num.infections==0){
    return(NULL)
  }
  
  if (length(id.potential)==1){
    id.vector <- id.potential
  } else {
    id.vector<-sample(id.potential,min(num.infections,length(id.potential)), replace = FALSE)
  }
  
  return(id.vector)
}

# function to start infection for the individuals of the given id's at current.time
register_infected_cases <- function(id.vector, current.time, status.matrix, id.infector){
  
  for (i_id in id.vector){
    
    status.matrix$infected[i_id]             <- 1 
    status.matrix$time.of.infection[i_id]    <- current.time
    status.matrix$TimeSymptomOnset[i_id]     <- current.time + fun.SymptomOnset(t = 10, fixed = TRUE)
    status.matrix$TimeRashOnset[i_id]        <- current.time + fun.RashOnset(t = 14, fixed = TRUE)
    status.matrix$recovery.vector[i_id]      <- status.matrix$TimeRashOnset[i_id] + fun.InfectDuration(t = 10, fixed = TRUE) # recovery time in days from rash onset
    status.matrix$total_infectionPeriod[i_id]<- status.matrix$recovery.vector[i_id] - current.time
    status.matrix$infectives[i_id]           <- 1
    
    status.matrix$infector[i_id] <- id.infector
    
    lambda.w <- status.matrix$contact_rate_within[i_id]
    lambda.b <- status.matrix$contact_rate_between[i_id]
    
    # default severity
    # status.matrix$severity[i_id]     <- 1
    
    if (runif(1) < status.matrix$rho[i_id]){ # if the indiviual is going to be symptomatic
      status.matrix$q[i_id]  <- R.s/R.ctcs  # a single q parameter for everyone
      if (runif(1) < status.matrix$pdiagn[i_id]){ # if the individual is going to be diagnosed: detection at time of rash onset
        status.matrix$detection.day[i_id]   <- status.matrix$TimeRashOnset[i_id]  
        # status.matrix$severity[i_id]        <- 2
      }
    }else{ # not symptomatic
      status.matrix$TimeSymptomOnset[i_id] <- Inf
      status.matrix$TimeRashOnset[i_id] <- Inf
      status.matrix$q[i_id]<-R.s/R.ctcs*alpha.as # relative infectiousness of asymptomatic
    }
    time.events <<- rbind(time.events, c(current.time, 1, i_id))
    
    # register contacts 
    status.matrix$index.contact.within[i_id] <- 1  
    if (status.matrix$Category[i_id]!=0){ 
      status.matrix$index.contact.between[i_id]<-1  
    }
    
  } # end for-loop to seed infected
  
  return(status.matrix)
}

