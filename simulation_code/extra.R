

setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_delayQ/')
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_lowerDiagn/')

# scenarios <- c("Baseline", "Scen1", "Scen2", "Scen3", "Scen4", "Scen5", "Scen6", "Scen7", "Scen8", "Scen9")
scenarios <- c("Sens2_baseline", "Sens2_scen1", "Sens2_scen2", "Sens2_scen3", "Sens2_scen4", 
               "Sens2_scen5", "Sens2_scen6", "Sens2_scen7", "Sens2_scen8", "Sens2_scen9")

###-----------------------------------------------------------------
### Number of school days lost = duration of Q minus weekend days?

# mean.SchoolDaysLost <- c()

for(s in scenarios){
  
  datfile <- paste0("epi_outbreak_", s, ".rds")
  out <- readRDS(datfile)
  
  numSchoolDaysLost <- c()
  for(i in 1:length(out)){
    SDL <- c()
    eventmat <- as.data.frame(out[[i]]$time.events)
    eventmat <- eventmat[!is.na(eventmat$time), ]
    contacts.isol <- eventmat$who[eventmat$event == 'IsolationContact']
    if(length(contacts.isol) > 0){
      for(c in contacts.isol){
        begin.isol <- round(as.numeric(eventmat$time[eventmat$event == 'IsolationContact' & eventmat$who == c]))
        end.isol <- round(as.numeric(eventmat$time[eventmat$event == 'BackFromIso' & eventmat$who == c]))
        for(t in 1:length(begin.isol)){
          bb <- begin.isol[t]
          ee <- end.isol[t]
          ee <- ifelse(is.na(ee), max(round(as.numeric(eventmat$time)), na.rm = T), ee)
          isol.days <- seq(bb, ee, 1)
          day_of_week <- isol.days %% 7
          isol.days <- isol.days[!(day_of_week %in% c(5,6))]
          schooldayslost <- length(isol.days)-1
          
          SDL <- c(SDL, schooldayslost)
        }
      }
    }else{
      SDL <- 0
    }
    numSchoolDaysLost <- c(numSchoolDaysLost, mean(SDL)) # mean number of school days lost for each simulation run
  }
  
  ss <- ifelse(s %in% c("Sens2_baseline","Sens2_scen1","Sens2_scen2","Sens2_scen3","Sens2_scen4"), 'No heterogeneity', 'Heterogeneity')
  scen <- s
  if(ss == 'Heterogeneity'){
    if(s == 'Sens2_scen5'){
      scen <- 'Baseline'
    }else if(s == 'Sens2_scen6'){
      scen <- 'A1'
    }else if(s == 'Sens2_scen7'){
      scen <- 'A2'
    }else if(s == 'Sens2_scen8'){
      scen <- 'A3'
    }else if(s == 'Sens2_scen9'){
      scen <- 'A4'
    }
  }
  
  mat.SDL <- matrix(NA, nrow = 10000, ncol = 3)
  for(i in 1:length(out)){
    mat.SDL[i, 1] <- scen
    mat.SDL[i, 2] <- ss
    mat.SDL[i, 3] <- numSchoolDaysLost[i]
  }
  
  if(s == 'Sens2_baseline'){
    df.SDL <- mat.SDL
  }else{
    df.SDL <- rbind(df.SDL, mat.SDL)
  }
  
}

df.SDL <- as.data.frame(df.SDL)
names(df.SDL) <- c('Scenario','Heterogeneity','SchoolDaysLost')
head(df.SDL)

# ## PEV w/ quarantine if not within 72 h
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_delayQ/PEV/')
# scenarios <- c("Sens7_baseline", "Sens7_scen1", "Sens7_scen2", "Sens7_scen3", "Sens7_scen4",
#                "Sens7_scen5", "Sens7_scen6", "Sens7_scen7", "Sens7_scen8", "Sens7_scen9")

## PEV w/o quarantine
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_delayQ/PEV/')
setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV_delay/')
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV_lowerDiagn/')
scenarios <- c("Sens6_baseline", "Sens6_scen1", "Sens6_scen2", "Sens6_scen3", "Sens6_scen4",
               "Sens6_scen5", "Sens6_scen6", "Sens6_scen7", "Sens6_scen8", "Sens6_scen9")

for(s in scenarios){
  
  datfile <- paste0("epi_outbreak_", s, ".rds")
  out <- readRDS(datfile)
  
  numSchoolDaysLost <- c()
  for(i in 1:length(out)){
    SDL <- c()
    eventmat <- as.data.frame(out[[i]]$time.events)
    eventmat <- eventmat[!is.na(eventmat$time), ]
    contacts.isol <- eventmat$who[eventmat$event == 'IsolationContact']
    if(length(contacts.isol) > 0){
      for(c in contacts.isol){
        begin.isol <- round(as.numeric(eventmat$time[eventmat$event == 'IsolationContact' & eventmat$who == c]))
        end.isol <- round(as.numeric(eventmat$time[eventmat$event == 'BackFromIso' & eventmat$who == c]))
        for(t in 1:length(begin.isol)){
          bb <- begin.isol[t]
          ee <- end.isol[t]
          ee <- ifelse(is.na(ee), max(round(as.numeric(eventmat$time)), na.rm = T), ee)
          isol.days <- seq(bb, ee, 1)
          day_of_week <- isol.days %% 7
          isol.days <- isol.days[!(day_of_week %in% c(5,6))]
          schooldayslost <- length(isol.days)-1
          
          SDL <- c(SDL, schooldayslost)
        }
      }
    }else{
      SDL <- 0
    }
    numSchoolDaysLost <- c(numSchoolDaysLost, mean(SDL)) # mean number of school days lost for each simulation run
  }
  
  ss <- ifelse(s %in% c("Sens6_baseline","Sens6_scen1","Sens6_scen2","Sens6_scen3","Sens6_scen4"), 'No heterogeneity', 'Heterogeneity')
  scen <- s
  if(ss == 'Heterogeneity'){
    if(s == 'Sens6_scen5'){
      scen <- 'Baseline'
    }else if(s == 'Sens6_scen6'){
      scen <- 'A1'
    }else if(s == 'Sens6_scen7'){
      scen <- 'A2'
    }else if(s == 'Sens6_scen8'){
      scen <- 'A3'
    }else if(s == 'Sens6_scen9'){
      scen <- 'A4'
    }
  }
  
  mat.SDL <- matrix(NA, nrow = 10000, ncol = 3)
  for(i in 1:length(out)){
    mat.SDL[i, 1] <- scen
    mat.SDL[i, 2] <- ss
    mat.SDL[i, 3] <- numSchoolDaysLost[i]
  }
  
  if(s == 'Sens6_baseline'){
    df.SDLPEV <- mat.SDL
  }else{
    df.SDLPEV <- rbind(df.SDLPEV, mat.SDL)
  }
  
}

df.SDLPEV <- as.data.frame(df.SDLPEV)
names(df.SDLPEV) <- c('Scenario','Heterogeneity','SchoolDaysLost')
head(df.SDLPEV)

## Combine
df.SDL$PEV <- 'No PEV'
df.SDLPEV$PEV <- 'PEV'

df.SDL.all <- rbind(df.SDL, df.SDLPEV)

df.SDL.all$SchoolDaysLost <- as.numeric(df.SDL.all$SchoolDaysLost)
df.SDL.all$SchoolDaysLost <- ifelse(df.SDL.all$SchoolDaysLost<0, 0, df.SDL.all$SchoolDaysLost)
summary(df.SDL.all$SchoolDaysLost)

aggregate(df.SDL.all$SchoolDaysLost[df.SDL.all$Heterogeneity=='Heterogeneity' & df.SDL.all$PEV=='No PEV'], 
          by = list(df.SDL.all$Scenario[df.SDL.all$Heterogeneity=='Heterogeneity' & df.SDL.all$PEV=='No PEV']),
          mean, na.rm = T)
aggregate(df.SDL.all$SchoolDaysLost[df.SDL.all$Heterogeneity=='Heterogeneity' & df.SDL.all$PEV=='PEV'], 
          by = list(df.SDL.all$Scenario[df.SDL.all$Heterogeneity=='Heterogeneity' & df.SDL.all$PEV=='PEV']),
          mean, na.rm = T)


library(ggplot2)
pp <- ggplot(df.SDL.all[df.SDL.all$Heterogeneity == 'Heterogeneity' & df.SDL.all$Scenario %in% c('A1','A2','A3','A4'), ], 
             aes(x = Scenario, y = SchoolDaysLost, fill = Scenario)) +
  geom_boxplot(show.legend = F) + # median line
  # geom_violin() +
  # ggtitle('(a)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Average number of school days lost') +
  facet_wrap(~PEV) +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        plot.title = element_text(size = 32),
        strip.text.x = element_text(size = 32))

pp
# ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/SchoolDaysLost_PEVnoQ.jpeg', pp, width = 70, height = 50, units = 'cm', dpi = 300)
ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/Figure3_lowerDiagn.pdf', width = 80, height = 50, units = 'cm', dpi = 300)

###-----------------------------------------------------------------
### pInfQ higher for lower immunity??

s1<-'Scen7'
s2<-'Sens3_scen7'

dat1 <- read.csv(paste0(s1, "_onward_transmission.csv"), header = T)
summary(dat1$pInfectiousAfterIsol)
summary(dat1$R_index)
dat2 <- read.csv(paste0(s2, "_onward_transmission.csv"), header = T)
summary(dat2$pInfectiousAfterIsol)
summary(dat2$R_index)

par(mfrow = c(2,1))
hist(dat1$pInfectiousAfterIsol, breaks = 20)
hist(dat2$pInfectiousAfterIsol, breaks = 20)

# simid <- dat1$simID[dat1$pInfectiousAfterIsol == max(dat1$pInfectiousAfterIsol[dat1$pInfectiousAfterIsol<0.4])][2]
# # simid2 <- dat2$simID[dat2$pInfectiousAfterIsol > dat1$pInfectiousAfterIsol[dat1$simID==simid]][1]
# simid2 <- dat2$simID[dat2$pInfectiousAfterIsol == max(dat2$pInfectiousAfterIsol, na.rm = T)][10]
# 
# dat1$pInfectiousAfterIsol[dat1$simID == simid]
# dat2$pInfectiousAfterIsol[dat2$simID == simid2]
# 
# out1 <- readRDS(paste0("epi_outbreak_", s1, ".rds"))
# out2 <- readRDS(paste0("epi_outbreak_", s2, ".rds"))
# 
# epi.outbreak <- out2
# i_sim <- simid2
# 
# events.sim <- as.data.frame(epi.outbreak[[i_sim]]$time.events)[-1,]
# events.sim <- data.frame(time = as.numeric(events.sim[,1]),
#                          event = events.sim[,2],
#                          id = as.numeric(events.sim[,3]))
# events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
# events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
# table(events.sim$event)
# 
# ids.infected <- events.sim$id[events.sim$event == 'Infection']
# ids.infected.rash <- NULL
# ids.infected.backfromiso <- list()
# ids.infectious.after.iso <- c()
# for(i in 1:length(ids.infected)){
#   ids.infected.rash[i] <- epi.outbreak[[i_sim]]$status.matrix$TimeRashOnset[epi.outbreak[[i_sim]]$status.matrix$id == ids.infected[i]]
#   back.iso <- events.sim$time[events.sim$event=='BackFromIso' & events.sim$id == ids.infected[i]]
#   if(length(back.iso) > 0){
#     ids.infected.backfromiso[[i]] <- back.iso
#     # back from isolation during infectious period?
#     if(any(back.iso >= (ids.infected.rash[i] - 4) & back.iso <= (ids.infected.rash[i] + 4))){
#       ids.infectious.after.iso <- c(ids.infectious.after.iso, ids.infected[i])
#     }
#   }
# }
# ids.infected
# ids.infectious.after.iso
# # pInfectiousAfterIsol[i_sim] <- length(unique(ids.infectious.after.iso)) / length(unique(ids.infected))

### time between start infectious period and quarantine of contacts? 
# (perhaps in low immunity scenario, cases are quarantined sooner and therefore still infectious after)
# while for high immunity, delayed quarantine (due to lower case detection) could result in lower pInfQ

# Default
time.to.Q <- c()
for(i in 1:10000){
  TTQ <- c() # time between start infectiousness and start Q
  eventmat <- as.data.frame(out1[[i]]$time.events)
  eventmat <- eventmat[!is.na(eventmat$time), ]
  contacts.isol <- eventmat$who[eventmat$event == 'IsolationContact']
  if(length(contacts.isol) > 0){
    for(c in contacts.isol){
      # isolation time
      begin.isol <- (as.numeric(eventmat$time[eventmat$event == 'IsolationContact' & eventmat$who == c]))
      end.isol <- (as.numeric(eventmat$time[eventmat$event == 'BackFromIso' & eventmat$who == c]))
      # start infectiousness
      start.infectiousness <- as.numeric(out1[[i]]$status.matrix$TimeRashOnset[out1[[i]]$status.matrix$id == c]) - 4
      
      if(start.infectiousness != 'Inf'){ # only infected cases
        
        # only (first) isolation after infection
        if(length(begin.isol) > 1){
          begin.isol <- begin.isol[begin.isol > out1[[i]]$status.matrix$time.of.infection[out1[[i]]$status.matrix$id == c]][1]
        }
        if(length(begin.isol) != 0){
          TTQ <- c(TTQ, start.infectiousness - begin.isol)
        }else{
          stop('still more than one isolation time')
        }
        
        # for(t in 1:length(begin.isol)){
        #   bb <- begin.isol[t]
        #   ee <- end.isol[t]
        #   ee <- ifelse(is.na(ee), max((as.numeric(eventmat$time)), na.rm = T), ee)
        #   
        #   TTQ <- c(TTQ, start.infectiousness - bb) 
        # }        
      }else{
        TTQ <- NA
      }
    }
  }else{
    TTQ <- NA
  }
  # if(!is.numeric(TTQ)){stop()}
  time.to.Q <- c(time.to.Q, mean(TTQ, na.rm = T))
}
summary(time.to.Q)
hist(time.to.Q)
time.to.Q.default <- time.to.Q

# SA3 (lower immunity)
time.to.Q <- c()
for(i in 1:10000){
  TTQ <- c() # time between start infectiousness and start Q
  eventmat <- as.data.frame(out2[[i]]$time.events)
  eventmat <- eventmat[!is.na(eventmat$time), ]
  contacts.isol <- eventmat$who[eventmat$event == 'IsolationContact']
  if(length(contacts.isol) > 0){
    for(c in contacts.isol){
      # isolation time
      begin.isol <- (as.numeric(eventmat$time[eventmat$event == 'IsolationContact' & eventmat$who == c]))
      end.isol <- (as.numeric(eventmat$time[eventmat$event == 'BackFromIso' & eventmat$who == c]))
      # start infectiousness
      start.infectiousness <- as.numeric(out2[[i]]$status.matrix$TimeRashOnset[out2[[i]]$status.matrix$id == c]) - 4
      
      if(start.infectiousness != 'Inf'){ # only infected cases
        
        # only (first) isolation after infection
        if(length(begin.isol) > 1){
          begin.isol <- begin.isol[begin.isol > out2[[i]]$status.matrix$time.of.infection[out2[[i]]$status.matrix$id == c]][1]
        }
        if(length(begin.isol) != 0){
          TTQ <- c(TTQ, start.infectiousness - begin.isol)
        }else{
          stop('still more than one isolation time')
        }
        
        # for(t in 1:length(begin.isol)){
        #   bb <- begin.isol[t]
        #   ee <- end.isol[t]
        #   ee <- ifelse(is.na(ee), max((as.numeric(eventmat$time)), na.rm = T), ee)
        #   
        #   TTQ <- c(TTQ, start.infectiousness - bb) 
        # }        
      }else{
        TTQ <- NA
      }
    }
  }else{
    TTQ <- NA
  }
  # if(!is.numeric(TTQ)){stop()}
  time.to.Q <- c(time.to.Q, mean(TTQ, na.rm = T))
}
summary(time.to.Q)
hist(time.to.Q)
time.to.Q.SA3 <- time.to.Q

summary(time.to.Q.default)
summary(time.to.Q.SA3) # expected to be higher on average

hist(time.to.Q.default, breaks = 10, xlim = c(min(c(time.to.Q.default,time.to.Q.SA3), na.rm = T), max(c(time.to.Q.default,time.to.Q.SA3), na.rm = T)))
hist(time.to.Q.SA3, breaks = 10, xlim = c(min(c(time.to.Q.default,time.to.Q.SA3), na.rm = T), max(c(time.to.Q.default,time.to.Q.SA3), na.rm = T)))

# If negative: infectiousness starts before Q --> lower prop means more cases who start Q after start infectiousness
sum(time.to.Q.default < 0, na.rm = T) / sum(!is.na(time.to.Q.default))
sum(time.to.Q.SA3 < 0, na.rm = T) / sum(!is.na(time.to.Q.SA3))

###-------------------------------------------------------------------------------
### Visualize infectiousness profile vs. detection; wrt onset rash

source('~/GitHub/measles_isolation/simulation_code/simFunctions.R')

s <- 'Scen5'

datfile <- paste0(s,"_onward_transmission.csv") # proportions for each simulation run
dat <- read.csv(datfile, header = T)
dat <- dat[,-1]

simid <- dat$simID[dat$R_index==max(dat$R_index)][1] # sim w/ highest R_index

datfile <- paste0("epi_outbreak_", s, ".rds")
out <- readRDS(datfile)
statusmat <- out[[simid]]$status.matrix
indexID <- which(statusmat$IndexCase == 1)

eventmat <- as.data.frame(out[[simid]]$time.events)

library(EnvStats)

# Symptom onset
median <- 12.5; sd <- 2.65
mulog <- log(median)
sdlog <- sqrt(log(1 + (sd^2 / median^2)))
symp.onset <- (rlnorm(10000, mulog, sdlog))
summary(symp.onset)

# Rash onset
rash.onset <- symp.onset + runif(10000, 3, 5)

# distInf <- (rtri(10000, min = rash.onset-4.01, max = rash.onset+4.01, mode = rash.onset))
distInf <- (runif(10000, min = rash.onset-4.01, max = rash.onset+4.01))
summary(distInf)

# Detection probability highest at rash onset -> depends on specific symptom onset
# distDetect <- (rtri(10000, min = median(symp.onset), max = median(rash.onset)+3, mode = median(rash.onset)))
hist(distDetect, breaks = sqrt(length(distDetect)), main = '', xlab = 'Time of detection index case (infected at time 0)', prob = T,
     xlim = c(0, 30))
# Infectiousness profile
# curve(dtri(x, min = rash.onset-4.01, max = rash.onset+4.01, mode = rash.onset), from = min(distDetect,distInf), to = max(distDetect,distInf), add = T,
#       col = 2, lwd = 3)
curve(dunif(x, min = median(rash.onset)-4.01, max = median(rash.onset)+4.01), from = 0, to = 30, add = T,
      col = 2, lwd = 3)#, xlab = 'Time since infection', ylab = '', ylim = c(0,0.2))
# Symptom onset
curve(dlnorm(x, meanlog = mulog, sdlog = sdlog), from = 0, to = 30, add = T,
      col = 3, lwd = 3)
# Rash onset
lines(density(rash.onset), col = 4, lwd = 3)
legend('topleft', c('Infectiousness profile i(t)','Symptom onset','Rash onset'), 
       col = c(2,3,4), lty = c(1,1,1), lwd = c(3,3,3), cex = 1)

# abline(v = symp.onset, col = 3, lwd = 3)
# abline(v = rash.onset, col = 4, lwd = 3)

# symp.onsetI <- statusmat$TimeSymptomOnset[indexID]
# rash.onsetI <- statusmat$TimeRashOnset[indexID]
# infect.timeI <- statusmat$time.of.infection[indexID]
# detect.timeI <- as.numeric(eventmat$time[eventmat$who == indexID & eventmat$event == 'Detection'][2])
# 
# infectMat <- statusmat[!is.na(statusmat$time.of.infection),]
# # cases infected by index
# infectMat <- infectMat[infectMat$infector == indexID & !is.na(infectMat$infector), ]
# # summary(infectMat$time.of.infection - detect.timeI)
# # hist(infectMat$time.of.infection - detect.timeI, breaks = sqrt(10000))
# 
# abline(v = infectMat$time.of.infection, lty = 2, col = 5)
# abline(v = detect.timeI, col = 6, lwd = 3)
# legend('topright', c('Infectiousness profile i(t)','Symptom onset','Rash onset', 'Secondary case infection time', 'Index detection'), 
#        col = c(2,3,4,5,6), lty = c(1,1,1,2,1), lwd = c(3,3,3,1,3), cex = 0.8)


### Distribution of sec. case time of infection and index detection
infDetectDist <- c()
for(i in 1:10000){
  
  statusmat <- out[[i]]$status.matrix
  indexID <- which(statusmat$IndexCase == 1)
  eventmat <- as.data.frame(out[[i]]$time.events) 
  eventmat <- eventmat[!is.na(eventmat$time), ]
  
  symp.onsetI <- statusmat$TimeSymptomOnset[indexID]
  rash.onsetI <- statusmat$TimeRashOnset[indexID]
  infect.timeI <- statusmat$time.of.infection[indexID]
  detect.timeI <- as.numeric(eventmat$time[eventmat$who == indexID & eventmat$event == 'Detection'])
  
  infectMat <- statusmat[!is.na(statusmat$time.of.infection),]
  # cases infected by index
  infectMat <- infectMat[infectMat$infector == indexID & !is.na(infectMat$infector), ]
  
  infDetectDist <- c(infDetectDist, mean(infectMat$time.of.infection - detect.timeI))
  
}
summary(infDetectDist); length(infDetectDist)

jpeg('~/GitHub/measles_isolation/simulation_output/FINAL/figures/detection.jpeg', width = 30, height = 20, units = 'cm', res = 300)
hist(infDetectDist, breaks = sqrt(length(infDetectDist)), main = '', xlab = 'Average time between secondary case infection and index detection')
# Negative = infected before index detection
dev.off()
