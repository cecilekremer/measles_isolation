
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

##---------------------------------------------------
## Risk of onward transmission after PEV
## i.e. prop of PEV that was not effective?

# for(s in scenarios){
# 
#   datfile <- paste0('epi_outbreak_',s,'.rds')
#   out <- readRDS(datfile)
# 
#   if(grepl('cen5', s)){
#     scenario <- 'Baseline'
#   }else if(grepl('cen6', s)){
#     scenario <- 'Scen1'
#   }else if(grepl('cen7', s)){
#     scenario <- 'Scen2'
#   }else if(grepl('cen8', s)){
#     scenario <- 'Scen3'
#   }else if(grepl('cen9', s)){
#     scenario <- 'Scen4'
#   }
# 
#   if(grepl('Sens1', s)){
#     analysis <- 'SA1'
#   }else if(grepl('Sens2', s)){
#     analysis <- 'SA2'
#   }else if(grepl('Sens3', s)){
#     analysis <- 'SA3'
#   }else if(grepl('Sens4', s)){
#     analysis <- 'SA4'
#   }else if(grepl('Sens5', s)){
#     analysis <- 'SA5'
#   }else if(grepl('Sens6', s)){
#     analysis <- 'SA6'
#   }else if(grepl('Sens7', s)){
#     analysis <- 'SA7'
#   }else if(grepl('Sens8', s)){
#     analysis <- 'SA8'
#   }else if(grepl('Sens9', s)){
#     analysis <- 'SA9'
#   }else{
#     analysis <- 'Default'
#   }
# 
#   prop.effective.PEV <- c()
#   prop.ineffective.PEV <- c()
#   prop.inf.effective.PEV <- c()
#   prop.inf.ineffective.PEV <- c()
#   mat <- matrix(NA, nrow = 10000, ncol = 6)
# 
#   for(i in 1:10000){
# 
#     outt <- out[[i]]
# 
#     # Simulation events
#     events.sim <- outt$time.events[-1, ]
#     events.sim <- data.frame(time = as.numeric(events.sim[,1]),
#                              event = events.sim[,2],
#                              id = as.numeric(events.sim[,3]))
#     events.sim$event <- ifelse(events.sim$event == '1', 'Infection', events.sim$event)
#     events.sim$event <- ifelse(events.sim$event == '-1', 'Recovery', events.sim$event)
# 
#     ## Proportion effective PEV (i.e. prevents current infection)
#     n.received.PEV <- length(unique(events.sim$id[events.sim$event == 'ContactPEV']))
#     n.effective.PEV <- length(unique(events.sim$id[events.sim$event == 'EffectivePEV']))
# 
#     prop.effective.PEV[i] <- ifelse(n.received.PEV > 0,
#                                  n.effective.PEV/n.received.PEV,
#                                  NA)
#     prop.ineffective.PEV[i] <- ifelse(!is.na(prop.effective.PEV[i]),
#                                       1 - prop.effective.PEV[i],
#                                       NA)
#     
#     ## Proportion of INFECTED cases who received PEV and PEV was effective
#     ids.received.PEV <- unique(events.sim$id[events.sim$event == 'ContactPEV'])
#     ids.infected <- unique(outt$status.matrix$id[outt$status.matrix$infected == (-1) & outt$status.matrix$IndexCase!=1])
#     ids.inf.received.PEV <- ids.received.PEV[ids.received.PEV %in% ids.infected]
#     ids.effective.PEV <- unique(events.sim$id[events.sim$event == 'EffectivePEV'])
#     ids.inf.effective.PEV <- ids.effective.PEV[ids.effective.PEV %in% ids.infected]
#     
#     n.inf.received.PEV <- length(unique(ids.inf.received.PEV))
#     n.inf.effective.PEV <- length(unique(ids.inf.effective.PEV))
#     
#     prop.inf.effective.PEV[i] <- ifelse(n.inf.received.PEV > 0,
#                                         n.inf.effective.PEV/n.inf.received.PEV,
#                                         NA)
#     prop.inf.ineffective.PEV[i] <- ifelse(!is.na(prop.inf.effective.PEV[i]),
#                                           1 - prop.inf.effective.PEV[i],
#                                           NA)
# 
#     mat[i, 1] <- analysis
#     mat[i, 2] <- scenario
#     mat[i, 3] <- prop.effective.PEV[i]
#     mat[i, 4] <- prop.ineffective.PEV[i]
#     mat[i, 5] <- prop.inf.effective.PEV[i]
#     mat[i, 6] <- prop.inf.ineffective.PEV[i]
# 
#   }
# 
#   if(s == scenarios[1]){
#     df.all <- mat
#   }else{
#     df.all <- rbind(df.all, mat)
#   }
# 
# }
# 
# # summary(prop.effective.PEV)
# # summary(prop.ineffective.PEV)
# 
# dim(df.all)

load('PEV_additional.RData')
head(df.all)


df.all <- as.data.frame(df.all)
df.all[,3] <- as.numeric(df.all[,3])
df.all[,4] <- as.numeric(df.all[,4])
df.all[,5] <- as.numeric(df.all[,5])
df.all[,6] <- as.numeric(df.all[,6])
names(df.all) <- c('Analysis', 'Scenario', 'propEffectivePEV', 'propIneffectivePEV', 'propInfectedEffectivePEV', 'propInfectedIneffectivePEV')
head(df.all)


library(ggplot2)
library(forcats)
library(dplyr)
df.all <- df.all %>%
  mutate(Scenario = fct_recode(Scenario, "Baseline" = "Baseline", "A1" = "Scen1", "A2" = "Scen2",
                               "A3" = "Scen3", "A4" = "Scen4"))

ggplot(df.all, aes(x = Scenario, y = propInfectedEffectivePEV, fill = Scenario)) +
  # ylim(0,0.01) +
  geom_boxplot() +
  facet_wrap(~Analysis)

## Across different immunity levels
ggplot(df.all[df.all$Analysis %in% c("Default", "SA2", "SA6"), ], aes(x = Scenario, y = propInfectedEffectivePEV, fill = Scenario)) +
  # ylim(0,0.01) +
  geom_boxplot() +
  ylab("Proportion of cases receiving PEV where PEV was effective") +
  facet_wrap(~Analysis) # Default (~93% immune) - SA2: 20% immune - SA3: 70% immune

aggregate(df.all$propInfectedEffectivePEV[df.all$Analysis == "SA6"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)
# aggregate(df.all$propInfectedEffectivePEV[df.all$Analysis == "SA6"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), median, na.rm = T)
aggregate(df.all$propInfectedEffectivePEV[df.all$Analysis == "SA2"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)
aggregate(df.all$propInfectedEffectivePEV[df.all$Analysis == "Default"], by = list(df.all$Scenario[df.all$Analysis == "SA6"]), mean, na.rm = T)


