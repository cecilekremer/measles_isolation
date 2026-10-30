

setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school/')

scenarios <- c("Sens2_baseline", "Sens2_scen1", "Sens2_scen2", "Sens2_scen3", "Sens2_scen4", 
               "Sens2_scen5", "Sens2_scen6", "Sens2_scen7", "Sens2_scen8", "Sens2_scen9")

###-----------------------------------------------------------------------------------
### PROBABILITY OF ONWARD TRANSMISSION

mean.propOnward <- c()
mean.propOnwardNoSeed <- c()
mean.propOnwardAfterIsol <- c()
mean.pInfectiousAfterIsol <- c()
mean.propDetected <- c()
mean.propSuscIsol <- c()
mean.FinSize <- c()
mean.FinSizeProp <- c()
mean.Rindex <- c()
mean.propNewWave <- c()
mean.FinSizeTert <- c()
mean.FinSizeTertProp <- c()

for(s in scenarios){
  
  datfile <- paste0(s,"_onward_transmission.csv") # proportions for each simulation run
  dat <- read.csv(datfile, header = T)
  dat <- dat[,-1]
  
  dat2 <- read.csv(paste0(s,"_outbreak_characteristics.csv"))
  dat2 <- dat2[,-1]
  
  # finSize without index-generated cases
  dat3 <- readRDS(paste0('contact_networks_', s, '.rds'))
  finsize.tert <- c()
  finsize.tert.prop <- c()
  # finsize.class <- c()
  # max.finsize.class <- c()
  for(i in 1:10000){
    dd <- dat3[[i]]
    ind <- which(dd$IsIndex == 1)
    ntert <- length(dd$ID[!is.na(dd$InfectorID) & dd$InfectorID != ind])
    nsec.index <- length(dd$ID[!is.na(dd$InfectorID) & dd$InfectorID == ind])
    
    # if(ntert > 1){
    #   finsize.tert[i] <- (ntert - 1) / (dim(dd)[1])
    # }else{
    #   finsize.tert[i] <- 0
    # }
    finsize.tert[i] <- ntert
    finsize.tert.prop[i] <- ntert / (dim(dd)[1] - nsec.index - 1)
    
    # for(c in unique(dd$StudentClass)){
    #   finsize.class <- c(finsize.class, sum(!is.na(dd$InfectionTime[dd$StudentClass==c]))/(sum(dd$StudentClass==c)-1))
    # }
    # max.finsize.class[i] <- max(finsize.class, na.rm = T)
  }
  
  # mean proportions across simulation runs for scenario s
  mean.propOnward <- c(mean.propOnward, mean(dat$propOnward, na.rm = T))
  mean.propOnwardNoSeed <- c(mean.propOnwardNoSeed, mean(dat$propOnwardNoSeed, na.rm = T))
  mean.propOnwardAfterIsol <- c(mean.propOnwardAfterIsol, mean(dat$propOnwardAfterIsol, na.rm = T))
  mean.pInfectiousAfterIsol <- c(mean.pInfectiousAfterIsol, mean(dat$pInfectiousAfterIsol, na.rm = T))
  mean.Rindex <- c(mean.Rindex, mean(dat$R_index, na.rm = T))
  mean.propNewWave <- c(mean.propNewWave, sum(dat$propOnwardNoSeed > 0, na.rm = T)/10000)
  # outbreak characteristics
  mean.propDetected <- c(mean.propDetected, mean(dat2$propDetected, na.rm = T))
  mean.propSuscIsol <- c(mean.propSuscIsol, mean(dat2$propSuscIsolated, na.rm = T))
  mean.FinSize <- c(mean.FinSize, mean(dat2$FinSize, na.rm = T))
  mean.FinSizeProp <- c(mean.FinSizeProp, mean(dat2$FinSizeProp, na.rm = T))
  mean.FinSizeTert <- c(mean.FinSizeTert, mean(finsize.tert, na.rm = T))
  mean.FinSizeTertProp <- c(mean.FinSizeTertProp, mean(finsize.tert.prop, na.rm = T))
  
  ## Data for boxplots: output from all simulation runs
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 13) # 1 = scenario, 2 = propOnward, 3 = propOnwardAfterIsol, 4 = pInfectiousAfterIsol, 5 = heterogeneity
  ss <- ifelse(s %in% c("Sens2_baseline","Sens2_scen1","Sens2_scen2","Sens2_scen3","Sens2_scen4"), 'No heterogeneity', 'Heterogeneity')
  # scen <- s
  if(ss == 'No heterogeneity'){
    if(s == 'Sens2_baseline'){
      scen <- 'Baseline'
    }else if(s == 'Sens2_scen1'){
      scen <- 'Scen1'
    }else if(s == 'Sens2_scen2'){
      scen <- 'Scen2'
    }else if(s == 'Sens2_scen3'){
      scen <- 'Scen3'
    }else if(s == 'Sens2_scen4'){
      scen <- 'Scen4'
    }
  }else if(ss == 'Heterogeneity'){
    if(s == 'Sens2_scen5'){
      scen <- 'Baseline'
    }else if(s == 'Sens2_scen6'){
      scen <- 'Scen1'
    }else if(s == 'Sens2_scen7'){
      scen <- 'Scen2'
    }else if(s == 'Sens2_scen8'){
      scen <- 'Scen3'
    }else if(s == 'Sens2_scen9'){
      scen <- 'Scen4'
    }
  }
  for(i in 1:dim(dat)[1]){
    idd <- dat$simID[i]
    mat.propOnward[i, 1] <- scen
    mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[dat$simID == idd])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[dat$simID == idd])
    mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[dat$simID == idd])
    mat.propOnward[i, 5] <- ss
    mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[dat2$simID == idd])
    mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[dat2$simID == idd])
    mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[dat2$simID == idd])
    mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[dat2$simID == idd])
    mat.propOnward[i, 10] <- idd
    mat.propOnward[i, 11] <- as.numeric(dat$R_index[dat$simID == idd])
    mat.propOnward[i, 12] <- as.numeric(finsize.tert[i])
    mat.propOnward[i, 13] <- as.numeric(finsize.tert.prop[i])
  }
  
  # all output from all simulation runs of all scenarios
  if(s == 'Sens2_baseline'){
    df.all <- mat.propOnward
  }else{
    df.all <- rbind(df.all, mat.propOnward)
  }
  
}

# mean across simulation runs per scenario
df.onward <- data.frame(Scenario = scenarios,
                        # propOnward = mean.propOnward,
                        FinSizeProp = mean.FinSizeProp,
                        FinSizeTertProp = mean.FinSizeTertProp,
                        R_index = mean.Rindex,
                        pOnward = mean.propOnwardNoSeed,
                        pNewWave = mean.propNewWave,
                        pInfQ = mean.pInfectiousAfterIsol,
                        pOnwardQ = mean.propOnwardAfterIsol,
                        pDetected = mean.propDetected,
                        pSuscIsol = mean.propSuscIsol,
                        FinSize = mean.FinSize,
                        FinSizeTert = mean.FinSizeTert
)
df.onward[1:5, c(1,2,3,4,5,7,8,6,9:12)]
df.onward[6:10, c(1,2,3,4,5,7,8,6,9:12)]

## Check for scenario w/o immunity: FinSizeProp should be higher when there is heterogeneity? 
# summary(df.onward$FinSizeProp[df.onward$Scenario=='Baseline' & df.onward$FinSize>1])
# summary(df.onward$FinSizeProp[df.onward$Scenario=='Scen5' & df.onward$FinSize>1])

df.all <- as.data.frame(df.all)
df.all[,2] <- as.numeric(df.all[,2])
df.all[,3] <- as.numeric(df.all[,3])
df.all[,4] <- as.numeric(df.all[,4])
df.all[,6] <- as.numeric(df.all[,6])
df.all[,7] <- as.numeric(df.all[,7])
df.all[,8] <- as.numeric(df.all[,8])
df.all[,9] <- as.numeric(df.all[,9])
df.all[,10] <- as.numeric(df.all[,10])
df.all[,11] <- as.numeric(df.all[,11])
df.all[,12] <- as.numeric(df.all[,12])
df.all[,13] <- as.numeric(df.all[,13])
names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'Heterogeneity',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'simID', 'R_index', 'FinSizeTert', 'FinSizeTertProp')

summary(df.all$propOnward[df.all$propOnward>0 & df.all$Scenario == 'Scen1' & df.all$Heterogeneity == 'No heterogeneity'])
summary(df.all$propOnwardAfterIsol[df.all$propOnward>0.1 & df.all$Scenario == 'Scen1' & df.all$Heterogeneity == 'No heterogeneity'])
df.all$simID[df.all$Scenario == 'Scen1' & df.all$Heterogeneity == 'No heterogeneity' & df.all$propOnwardAfterIsol > 0 & df.all$propOnward > 0.2]
simID = 4120
s <- 'Sens2_scen1'

datfile <- paste0("epi_outbreak_", s, ".rds")
out <- readRDS(datfile)
statusmat <- out[[simID]]$status.matrix
indexID <- which(statusmat$IndexCase == 1)
eventmat <- as.data.frame(out[[simID]]$time.events)

statusmat$Category[statusmat$IndexCase == 1]
table(statusmat$infected[statusmat$Category == 7])
table(statusmat$infector)

dat3 <- readRDS('contact_networks_Sens2_scen1.rds')
contactNet <- dat3[[simID]]
infectionNet <- contactNet[!is.na(contactNet$InfectionTime), ]
infectionNet$inf.start <- infectionNet$RashOnsDate - 4
infectionNet$inf.end <- infectionNet$RashOnsDate + 4

epi.out <- out[[simID]]
events <- as.data.frame(epi.out$time.events[-1,])
infectionNet$isolation.contact <- NA
infectionNet$isolation.inf <- NA
for(i in 1:dim(infectionNet)[1]){
  id <- infectionNet$ID[i]
  inf.day <- as.numeric(events$time[events$event == 1 & events$who == id])
  isol.day <- as.numeric(events$time[events$event == 'IsolationContact' & events$who == id])
  if(length(isol.day) > 0){
    isol.day <- isol.day[which(isol.day > inf.day)]
    infectionNet$isolation.contact[i] <- isol.day[1]
  }
  isol.inf.day <- as.numeric(events$time[events$event == 'Isolation' & events$who == id])
  if(length(isol.inf.day) > 0){
    infectionNet$isolation.inf[i] <- isol.inf.day
  }
}
sum(infectionNet$isolation.contact > infectionNet$inf.start, na.rm = T)

contactNet$vaccinated <- ifelse(contactNet$isImmune == 1 & is.na(contactNet$InfectionTime), 1, 0)

library(epicontacts)
epiCases <- infectionNet[,c(1,2,5,6,7,12,13,14,15)]
epiContacts <- infectionNet[,c(6,1)]
cons_tree <- make_epicontacts(epiCases, epiContacts, directed = TRUE)
plot(cons_tree)
library(ggplot2)
library(igraph)
library(adegenet)
library(EpiEstim)
library(visNetwork)

library(ggraph)
edges <- na.omit(epiContacts)
library(igraph)
library(dplyr)
g <- graph_from_data_frame(edges, vertices = epiCases, directed = T)
V(g)$infectionTime <- epiCases$InfectionTime#[match(V(g)$ID, epiCases$ID)]
layout <- data.frame(x = V(g)$InfectionTime, y = seq_along(V(g)))
# layout <- layout[order(layout$x), ]
p <- ggraph(g, layout = 'manual', x = layout$x, y = layout$y) +
  geom_edge_link(arrow = arrow(length = unit(0.2, 'cm')), end_cap = circle(3, 'mm')) +
  geom_node_point(size = 5, color = 'blue') +
  geom_node_text(aes(label = name), vjust = -0.5) +
  geom_segment(data = epiCases, aes(x = inf.start, xend = inf.end, y = seq_along(ID), yend = seq_along(ID)), color = 'red', 
               linetype = 'solid',
               linewidth = 1) +
  geom_point(data = epiCases, aes(x = isolation.contact, y = seq_along(ID)), color = 'purple', shape = 17,
             size = 4) +
  geom_point(data = epiCases, aes(x = isolation.inf, y = seq_along(ID)), color = 'red', shape = 17,
             size = 4) +
  theme_minimal()
p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab('') + xlab('Infection time') +
  ggtitle('Sensitivity analysis 2 - scenario A1 - no heterogeneity')

ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/transm_tree_SA2A1_2.jpeg', width = 60, height = 40, units = 'cm')
