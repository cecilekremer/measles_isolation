
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV_delay/')
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_delayQ/PEV/')
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV_delay1day/')
setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV_lowerDiagn/MULTnoQafterPEV/')

scenarios <- c("Sens9_baseline", "Sens9_scen1", "Sens9_scen2", "Sens9_scen3", "Sens9_scen4", 
               "Sens9_scen5", "Sens9_scen6", "Sens9_scen7", "Sens9_scen8", "Sens9_scen9")

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
mean.propOnwardAfterPEV <- c()
mean.propOnwardNoPEV <- c()
mean.propEffectivePEV <- c()
mean.propReceivedPEV <- c()
mean.propSuscReceivedPEV <- c()
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
  # PEV
  mean.propOnwardAfterPEV <- c(mean.propOnwardAfterPEV, mean(dat$propOnwardAfterPEV, na.rm = T))
  mean.propOnwardNoPEV <- c(mean.propOnwardNoPEV, mean(dat$propOnwardNoPEV, na.rm = T))
  mean.propEffectivePEV <- c(mean.propEffectivePEV, mean(dat$propEffectivePEV, na.rm = T))
  mean.propReceivedPEV <- c(mean.propReceivedPEV, mean(dat$propReceivedPEV, na.rm = T))
  mean.propSuscReceivedPEV <- c(mean.propSuscReceivedPEV, mean(dat$propSuscReceivedPEV, na.rm = T))
  
  ## Data for boxplots: output from all simulation runs
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 18) # 1 = scenario, 2 = propOnward, 3 = propOnwardAfterIsol, 4 = pInfectiousAfterIsol, 5 = heterogeneity
  ss <- ifelse(s %in% c("Sens9_baseline","Sens9_scen1","Sens9_scen2","Sens9_scen3","Sens9_scen4"), 'No heterogeneity', 'Heterogeneity')
  # scen <- s
  if(ss == 'No heterogeneity'){
    if(s == 'Sens9_baseline'){
      scen <- 'Baseline'
    }else if(s == 'Sens9_scen1'){
      scen <- 'Scen1'
    }else if(s == 'Sens9_scen2'){
      scen <- 'Scen2'
    }else if(s == 'Sens9_scen3'){
      scen <- 'Scen3'
    }else if(s == 'Sens9_scen4'){
      scen <- 'Scen4'
    }
  }else if(ss == 'Heterogeneity'){
    if(s == 'Sens9_scen5'){
      scen <- 'Baseline'
    }else if(s == 'Sens9_scen6'){
      scen <- 'Scen1'
    }else if(s == 'Sens9_scen7'){
      scen <- 'Scen2'
    }else if(s == 'Sens9_scen8'){
      scen <- 'Scen3'
    }else if(s == 'Sens9_scen9'){
      scen <- 'Scen4'
    }
  }
  for(i in 1:dim(dat)[1]){
    mat.propOnward[i, 1] <- scen
    mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[i])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
    mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[i])
    mat.propOnward[i, 5] <- ss
    mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[i])
    mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[i])
    mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[i])
    mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[i])
    mat.propOnward[i, 10] <- as.numeric(dat$R_index[i])
    mat.propOnward[i, 11] <- as.numeric(dat$propOnwardAfterPEV[i])
    mat.propOnward[i, 12] <- as.numeric(dat$propEffectivePEV[i])
    mat.propOnward[i, 13] <- as.numeric(dat$propReceivedPEV[i])
    mat.propOnward[i, 14] <- as.numeric(dat$propSuscReceivedPEV[i])
    mat.propOnward[i, 15] <- as.numeric(finsize.tert[i])
    mat.propOnward[i, 16] <- as.numeric(finsize.tert.prop[i])
    mat.propOnward[i, 17] <- as.numeric(dat$propOnwardNoPEV[i])
    mat.propOnward[i, 18] <- i
  }
  
  # all output from all simulation runs of all scenarios
  if(s == 'Sens9_baseline'){
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
                        propOnwardAfterPEV = mean.propOnwardAfterPEV,
                        propOnwardNoPEV = mean.propOnwardNoPEV,
                        pOnwardQ = mean.propOnwardAfterIsol,
                        pDetected = mean.propDetected,
                        pSuscIsol = mean.propSuscIsol,
                        FinSize = mean.FinSize,
                        FinSizeTert = mean.FinSizeTert,
                        propEffectivePEV = mean.propEffectivePEV,
                        propReceivedPEV = mean.propReceivedPEV,
                        propSuscReceivedPEV = mean.propSuscReceivedPEV
)

# df.onward[1:5, c(1,2,3,4,8,9,5,7,10,6,11:17)]
# df.onward[6:10, c(1,2,3,4,8,9,7,10,6,11:17,5)]
knitr::kable(round(df.onward[1:5, c(2,3,4,5,7,10,6)], 4), "latex", booktab=T)
knitr::kable(round(df.onward[6:10, c(2,3,4,5,7,10,6)], 4), "latex", booktab=T)

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
df.all[,14] <- as.numeric(df.all[,14])
df.all[,15] <- as.numeric(df.all[,15])
df.all[,16] <- as.numeric(df.all[,16])
df.all[,17] <- as.numeric(df.all[,17])

mat.propOnward[i, 1] <- scen
mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[i])
mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[i])
mat.propOnward[i, 5] <- ss
mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[i])
mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[i])
mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[i])
mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[i])
mat.propOnward[i, 10] <- as.numeric(dat$R_index[i])
mat.propOnward[i, 11] <- as.numeric(dat$propOnwardAfterPEV[i])
mat.propOnward[i, 12] <- as.numeric(dat$propEffectivePEV[i])
mat.propOnward[i, 13] <- as.numeric(dat$propReceivedPEV[i])
mat.propOnward[i, 14] <- as.numeric(dat$propSuscReceivedPEV[i])
mat.propOnward[i, 15] <- as.numeric(finsize.tert[i])
mat.propOnward[i, 16] <- as.numeric(finsize.tert.prop[i])
mat.propOnward[i, 17] <- as.numeric(dat$propOnwardNoPEV[i])
names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'Heterogeneity',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'R_index', 'propOnwardAfterPEV', 
                   'propEffectivePEV', 'propReceivedPEV', 'propSuscReceivedPEV','FinSizeTert','FinSizeTertProp', 'propOnwardNoPEV','sim')
# summary(df.all$propOnward)
# summary(df.all$propOnwardAfterIsol)
# summary(df.all$pInfectiousAfterIsol)
# summary(df.all$propOnwardAfterPEV)
# summary(df.all$propOnwardNoPEV)
# summary(df.all$propEffectivePEV)
# summary(df.all$propReceivedPEV)
# head(df.all[which(df.all$propOnwardNoPEV > 1), ])



library(ggplot2)
ggplot(df.all, aes(x = Heterogeneity, y = propOnward, fill = Scenario)) +
  geom_boxplot() + # median line
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of all cases (excl. index) causing onward transmission') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# ggsave('~/GitHub/measles_isolation/simulation_output/figures/FigureS1_DC_SA3.jpeg', width = 40, height = 20, units = 'cm', dpi = 300)

p1 <- ggplot(df.all[df.all$Heterogeneity == 'Heterogeneity', ], aes(x = Scenario, y = propOnward, fill = Scenario)) +
  geom_boxplot(show.legend = F) + # median line
  ggtitle('(a)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of all cases (excl. index) causing onward transmission') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))
p2 <- ggplot(df.all[df.all$Heterogeneity == 'Heterogeneity', ], aes(x = Scenario, y = pInfectiousAfterIsol, fill = Scenario)) +
  geom_boxplot(show.legend = F) + # median line
  ggtitle('(b)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of infected contacts that are infectious after quarantine') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))
p3 <- ggplot(df.all[df.all$Heterogeneity == 'Heterogeneity', ], aes(x = Scenario, y = FinSizeProp, fill = Scenario)) +
  geom_boxplot(show.legend = F) + # median line
  ggtitle('(c)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size (proportion of children that got infected, excl. index)') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))
p4 <- ggplot(df.all[df.all$Heterogeneity == 'Heterogeneity', ], aes(x = Scenario, y = FinSizeTertProp, fill = Scenario)) +
  geom_boxplot(show.legend = F) + # median line
  ggtitle('(d)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size excluding index-generated cases') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))
library(gridExtra); library(ggpubr); library(grid)
pp <- grid.arrange(p1, p2, p3, p4, ncol = 2, top = textGrob("School setting - sensitivity analysis 9", gp = gpar(fontsize=30,font=3)))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/Figure1_SA9.jpeg', pp, width = 70, height = 50, units = 'cm', dpi = 300)

# summary(df.all$propOnward[df.all$Scenario=='Baseline' & df.all$Heterogeneity=='Heterogeneity'])

ggplot(df.all[df.all$Scenario != 'Baseline',], aes(x = Heterogeneity, y = pInfectiousAfterIsol, fill = Scenario)) +
  # geom_violin() +
  geom_boxplot() + # median line
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  xlab('') + ylab('Proportion of infected contacts that are infectious after isolation') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/FigureS2_DC_SA3.jpeg', width = 40, height = 20, units = 'cm', dpi = 300)

ggplot(df.all[df.all$Scenario != 'Baseline',], aes(x = Heterogeneity, y = propOnwardAfterIsol, fill = Scenario)) +
  # geom_violin() + #ylim(0,0.02) +
  geom_boxplot() +
  xlab('') + ylab('Proportion of exposed individuals causing onward transmission AFTER isolation')+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/FigureS3_DC_SA3.jpeg', width = 40, height = 20, units = 'cm', dpi = 300)

## Proportion of simulations with >1% onward transmission after isolation
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Baseline' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen1' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen2' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen3' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen4' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000

sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Baseline' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen1' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen2' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen3' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen4' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000

## Final size
ggplot(df.all, aes(x = Heterogeneity, y = FinSizeProp, fill = Scenario)) +
  geom_boxplot() + # median line
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size (proportion of children that got infected)') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/FinSize_DC_SA3.jpeg', width = 40, height = 20, units = 'cm', dpi = 300)

###--------------------------------------------------------------------------------------------------------------------
### TIME-VARYING EFFECTIVE REPRODUCTION NUMBER
library(dplyr)
nSim <- 1000
# Rt_out <- matrix(NA, nrow = nSim*length(scenarios), ncol = 6) # cols: Scenario, Heterogeneity, sim, time, Rt_forw, Rt_backw
Rt_out <- data.frame(Scenario = NA, Heterogeneity = NA, sim = NA, time = NA, Rt_forw = NA, Rt_backw = NA)
cnt <- 1
# listplots <- list()
for(s in scenarios){
  
  ss <- ifelse(s %in% c("Sens9_baseline","Sens9_scen1","Sens9_scen2","Sens9_scen3","Sens9_scen4"), 'No heterogeneity', 'Heterogeneity')
  # scen <- s
  if(ss == 'No heterogeneity'){
    if(s == 'Sens9_baseline'){
      scen <- 'Baseline'
    }else if(s == 'Sens9_scen1'){
      scen <- 'Scen1'
    }else if(s == 'Sens9_scen2'){
      scen <- 'Scen2'
    }else if(s == 'Sens9_scen3'){
      scen <- 'Scen3'
    }else if(s == 'Sens9_scen4'){
      scen <- 'Scen4'
    }
  }else if(ss == 'Heterogeneity'){
    if(s == 'Sens9_scen5'){
      scen <- 'Baseline'
    }else if(s == 'Sens9_scen6'){
      scen <- 'Scen1'
    }else if(s == 'Sens9_scen7'){
      scen <- 'Scen2'
    }else if(s == 'Sens9_scen8'){
      scen <- 'Scen3'
    }else if(s == 'Sens9_scen9'){
      scen <- 'Scen4'
    }
  }
  
  datfile <- paste0("epi_outbreak_", s, ".rds")
  out <- readRDS(datfile)
  
  # Rt_forw <- list(); Rt_backw <- list()
  
  selected_sims <- sample(1:nSim, 1000)
  
  # cnt2 <- 1
  for(i_sim in selected_sims){
    
    statusmat <- out[[i_sim]]$status.matrix
    time_range <- seq(min(statusmat$time.of.infection, na.rm = T), max(statusmat$time.of.infection, na.rm = T))
    Rt_res <- data.frame(time = time_range) %>%
      rowwise() %>%
      mutate(
        # count infectors
        infectors = sum(floor(statusmat$time.of.infection) == time, na.rm = T),
        Rt_forw = 0,
        Rt_backw = 0
      )
    for(i in 1:dim(Rt_res)[1]){
      t <- Rt_res$time[i]
      ## Forward Rt
      n.infectors <- sum(floor(statusmat$time.of.infection[!is.na(statusmat$time.of.infection)]) == t)
      infectors <- which(floor(statusmat$time.of.infection) == t)
      # infectees
      n.infectees <- sum(statusmat$infector %in% infectors, na.rm = T)
      if(n.infectors > 0){
        Rt_res$Rt_forw[i] <- n.infectees / n.infectors
      }
      ## Backward Rt
      infectors <- which((statusmat$TimeRashOnset - 4) <= t & (statusmat$TimeRashOnset + 4) >= t)
      n.infectors <- length(infectors)
      n.infectees <- sum(statusmat$infector %in% infectors, na.rm = T)
      if(n.infectors > 0){
        Rt_res$Rt_backw[i] <- n.infectees / n.infectors
      }
    }
    # cnt2 <- cnt2 + 1
    Rt_res$Scenario <- scen
    Rt_res$sim <- i_sim
    Rt_res$Heterogeneity <- ss
    Rt_res <- Rt_res[,c(5,7,6,1,3,4)]
    
    Rt_out <- rbind(Rt_out, Rt_res)
    
  }
  
  # Rt_out[cnt, ] <- c(s, mean(Rt_forw), mean(Rt_backw))
  # plotname <- paste0('p_', s)
  # assign(plotname, p)
  # listplots[[cnt]] <- p
  
  # cnt <- cnt + 1
  
}

Rt_out <- Rt_out[-1, ]

head(Rt_out)

ggplot(Rt_out,
       aes(x = time, y = sim, fill = Rt_forw)) +
  geom_tile() +
  scale_fill_viridis_c(name = 'Effective Rt') +
  ylab('Simulation run') +
  facet_grid(Heterogeneity~Scenario)

## Proportion of sims w/ Rt>1 for t>0
Rt_prop <- Rt_out[Rt_out$time > 0, ]
Rt_prop <- Rt_prop %>%
  group_by(Scenario, Heterogeneity, time) %>%
  mutate(Rt_1 = ifelse(Rt_forw > 1, 1, 0))
Rt_prop_summary <- Rt_prop %>%
  group_by(Scenario, Heterogeneity) %>%
  summarise(propRT1 = mean(Rt_1))


##----------------------------------------------------
## Keep those with more than just the index case

Rt_out <- Rt_out %>%
  group_by(Scenario, Heterogeneity, sim) %>%
  mutate(group_id = cur_group_id()) 
cnts <- Rt_out %>% 
  group_by(group_id) %>%
  count()
head(cnts); dim(cnts)
ids.keep <- cnts$group_id[cnts$n>1]
length(unique(ids.keep))
Rt_out <- Rt_out[Rt_out$group_id %in% ids.keep, ]

Rt_out %>%
  group_by(Scenario, Heterogeneity) %>%
  summarise(meanRt = mean(Rt_forw, na.rm = T), medianRt = median(Rt_forw, na.rm = T))

## Plot Rt for each scenario: randomly select 20 simulation runs
Rt_out$id <- paste0(Rt_out$Scenario, Rt_out$Heterogeneity, Rt_out$sim)
sampled_sims <- c()
for(i in unique(Rt_out$Scenario)){
  for(j in unique(Rt_out$Heterogeneity)){
    sampled_sims <- c(sampled_sims,
                      sample(unique(Rt_out$id[Rt_out$Scenario == i & Rt_out$Heterogeneity == j]), 20, replace = FALSE)
    )
  }
}

Rt_plot <- Rt_out[Rt_out$id %in% sampled_sims, ]

library(ggplot2)
p.wIndex <- Rt_plot %>%  
  ggplot(aes(x = time, y = Rt_forw, group = factor(sim))) +
  geom_line(aes(color = factor(sim)), size = 0.5, show.legend = F) +
  # geom_smooth(aes(color = factor(sim)), size = 0.5, se = F, method = 'loess', span = 0.3, show.legend = F) + # small span = less smooth
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  labs(
    title = '',
    x = 'Time',
    y = 'Effective reproduction number Rt'
  ) +
  facet_grid(Heterogeneity~Scenario) +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size= 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/FigureRt_forw_DC_SA3.jpeg', plot = p.wIndex, width = 60, height = 40, dpi = 300, units = 'cm')

# excluding index case
p.noIndex <- Rt_plot[Rt_plot$time > 0, ] %>%  
  ggplot(aes(x = time, y = Rt_forw, group = factor(id))) +
  geom_line(aes(color = factor(sim)), size = 0.5, show.legend = F) +
  # geom_smooth(aes(color = factor(sim)), size = 0.5, se = F, method = 'loess', span = 0.3, show.legend = F) + # small span = less smooth
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  labs(
    title = '',
    x = 'Time',
    y = 'Effective reproduction number Rt'
  ) +  
  facet_grid(Heterogeneity~Scenario) +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size= 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/FigureRt1_DC_SA3.jpeg', plot = p.noIndex, width = 60, height = 40, dpi = 300, units = 'cm')



