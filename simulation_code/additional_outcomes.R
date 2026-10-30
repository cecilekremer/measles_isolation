

setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV/')

# scenarios <- c("Sens6_baseline", "Sens6_scen1", "Sens6_scen2", "Sens6_scen3", "Sens6_scen4", 
#                "Sens6_scen5", "Sens6_scen6", "Sens6_scen7", "Sens6_scen8", "Sens6_scen9")
scenarios <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
               paste0('Sens',1:8,'_scen5'),
               paste0('Sens',1:8,'_scen6'),
               paste0('Sens',1:8,'_scen7'),
               paste0('Sens',1:8,'_scen8'),
               paste0('Sens',1:8,'_scen9')
)

##------------------------------------------------------------------
## Proportion of quarantined cases that cause transmission after Q

for(s in scenarios){

  datfile <- paste0('epi_outbreak_',s,'.rds')
  out <- readRDS(datfile)

  if(grepl('cen5', s)){
    scenario <- 'Baseline'
  }else if(grepl('cen6', s)){
    scenario <- 'Scen1'
  }else if(grepl('cen7', s)){
    scenario <- 'Scen2'
  }else if(grepl('cen8', s)){
    scenario <- 'Scen3'
  }else if(grepl('cen9', s)){
    scenario <- 'Scen4'
  }

  if(grepl('Sens1', s)){
    analysis <- 'SA1'
  }else if(grepl('Sens2', s)){
    analysis <- 'SA2'
  }else if(grepl('Sens3', s)){
    analysis <- 'SA3'
  }else if(grepl('Sens4', s)){
    analysis <- 'SA4'
  }else if(grepl('Sens5', s)){
    analysis <- 'SA5'
  }else if(grepl('Sens6', s)){
    analysis <- 'SA6'
  }else if(grepl('Sens7', s)){
    analysis <- 'SA7'
  }else if(grepl('Sens8', s)){
    analysis <- 'SA8'
  }else if(grepl('Sens9', s)){
    analysis <- 'SA9'
  }else{
    analysis <- 'Default'
  }

  propOnwardAfterQ <- c()
  propInfectiousAfterQ <- c()
  mat <- matrix(NA, nrow = 10000, ncol = 4)

  for(i in 1:10000){

    outt <- out[[i]]

    # Simulation events
    events.sim <- outt$time.events[-1, ]
    events.sim <- data.frame(time = as.numeric(events.sim[,1]),
                             event = events.sim[,2],
                             id = as.numeric(events.sim[,3]))
    events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
    events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)

    total.seeds <- length(which(outt$status.matrix$IndexCase == 1))
    n <- dim(outt$status.matrix)[1]

    ## Proportion of quarantined cases that cause transmission after Q
    ids.isol <- unique(events.sim$id[events.sim$event == "BackFromIso"])
    id.index <- outt$status.matrix$id[outt$status.matrix$IndexCase==1]
    ids.isol <- ids.isol[!(ids.isol %in% id.index)]
    # quarantined cases who ever infected someone
    ids.inf.isol <- unique(ids.isol[ids.isol %in% events.sim$id[events.sim$event == "Infector"]])
    # check if infected someone after Q
    inf.after.isol <- rep(0, length(ids.inf.isol))
    if(length(ids.inf.isol) > 0){
      for(j in 1: length(ids.inf.isol)){
        # times that they infected someone
        inf.times <- events.sim$time[events.sim$event == 'Infector' & events.sim$id == ids.inf.isol[j]]
        # their own infection time
        infection.time <- outt$status.matrix$time.of.infection[outt$status.matrix$id == ids.inf.isol[j]]
        # time(s) they got back from Q
        backfromiso.times <- events.sim$time[events.sim$event == 'BackFromIso' & events.sim$id == ids.inf.isol[j]]
        # first time back from Q after infection
        backfromiso.times <- min(backfromiso.times[backfromiso.times > infection.time], na.rm = T)
        # time difference between infecting others and being back from Q
        timediff <- inf.times - backfromiso.times
        if(any((timediff[!is.na(timediff)]) > 0)){
          inf.after.isol[j] <- 1
        }
      }
    }
    propOnwardAfterQ[i] <- ifelse(length(ids.inf.isol) > 0,
                                  sum(inf.after.isol)/length(unique(ids.inf.isol)), # number of quarantined cases
                                  0)

    ## Proportion of quarantined cases that are infectious after Q
    # quarantined cases
    ids.infected.isol <- unique(ids.isol[ids.isol %in% events.sim$id[events.sim$event == "Infection"]])
    if(length(ids.infected.isol) > 0){
      # for each quarantined case, check whether still in infectious period after Q
      ids.infectious.after.Q <- c()
      for(j in 1:length(ids.infected.isol)){
        rash <- outt$status.matrix$TimeRashOnset[outt$status.matrix$id == ids.infected.isol[j]]
        back.iso <- events.sim$time[events.sim$event == 'BackFromIso' & events.sim$id == ids.infected.isol[j]]
        if(length(back.iso[!is.na(back.iso)]) > 0){
          # back from Q during infectious period?
          if(any((back.iso >= (rash - 4)) & (back.iso <= (rash + 4)))){
            ids.infectious.after.Q <- c(ids.infectious.after.Q, ids.infected.isol[j])
          }
        }
      }
    }
    propInfectiousAfterQ[i] <- ifelse(length(ids.infected.isol) > 0,
                                      length(unique(ids.infectious.after.Q)) / length(unique(ids.infected.isol)),
                                      0)

    mat[i, 1] <- analysis
    mat[i, 2] <- scenario
    mat[i, 3] <- propOnwardAfterQ[i]
    mat[i, 4] <- propInfectiousAfterQ[i]

  }

  if(s == scenarios[1]){
    df.all <- mat
  }else{
    df.all <- rbind(df.all, mat)
  }

}
dim(df.all)

load('additional_outcomes_PEV.RData')
head(df.all)


df.all <- as.data.frame(df.all)
df.all[,3] <- as.numeric(df.all[,3])
df.all[,4] <- as.numeric(df.all[,4])
names(df.all) <- c('Analysis', 'Scenario', 'propOnwardAfterQ', 'propInfectiousAfterQ')
head(df.all)


library(ggplot2)
library(forcats)
library(dplyr)
df.all <- df.all %>%
  mutate(Scenario = fct_recode(Scenario, "Baseline" = "Baseline", "A1" = "Scen1", "A2" = "Scen2",
                               "A3" = "Scen3", "A4" = "Scen4"))

ggplot(df.all, aes(x = Scenario, y = propOnwardAfterQ, fill = Scenario)) +
  # ylim(0,0.01) +
  geom_boxplot() +
  facet_wrap(~Analysis)

## Across different immunity levels
ggplot(df.all[df.all$Analysis %in% c("Default", "SA2", "SA6"), ], aes(x = Scenario, y = propOnwardAfterQ, fill = Scenario)) +
  # ylim(0,0.01) +
  geom_boxplot() +
  ylab("Proportion of quarantined cases that transmit after return") +
  facet_wrap(~Analysis) # Default (~93% immune) - SA2: 20% immune - SA3: 70% immune

ggplot(df.all[df.all$Analysis %in% c("Default", "SA2", "SA6"), ], aes(x = Scenario, y = propInfectiousAfterQ, fill = Scenario)) +
  # ylim(0,0.01) +
  geom_boxplot() +
  ylab("Proportion of quarantined cases that are infectious after return") +
  facet_wrap(~Analysis) # Default (~93% immune) - SA2: 20% immune - SA3: 70% immune

aggregate(df.all$propInfectiousAfterQ[df.all$Analysis == "SA6"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)
# aggregate(df.all$propInfectedEffectivePEV[df.all$Analysis == "SA6"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), median, na.rm = T)

aggregate(df.all$propInfectiousAfterQ[df.all$Analysis == "SA2"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)
aggregate(df.all$propInfectiousAfterQ[df.all$Analysis == "SA3"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)

aggregate(df.all$propInfectiousAfterQ[df.all$Analysis == "Default"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)




