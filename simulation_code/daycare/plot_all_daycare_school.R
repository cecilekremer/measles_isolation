
setwd('~/GitHub/measles_isolation/simulation_output/FINAL/daycare_delayQ/')

## Only scenarios including heterogeneity
scenariosHet <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
                  paste0('Sens',1:3,'_scen5'),
                  paste0('Sens',1:3,'_scen6'),
                  paste0('Sens',1:3,'_scen7'),
                  paste0('Sens',1:3,'_scen8'),
                  paste0('Sens',1:3,'_scen9')
)

for(s in scenariosHet){
  
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
    
    finsize.tert[i] <- ntert
    finsize.tert.prop[i] <- ntert / (dim(dd)[1] - nsec.index - 1)
  }
  
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 12)
  
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
  }else{
    analysis <- 'Default'
  }
  
  for(i in 1:dim(dat)[1]){
    mat.propOnward[i, 1] <- scenario
    mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[i])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
    mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[i])
    mat.propOnward[i, 5] <- analysis
    mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[i])
    mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[i])
    mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[i])
    mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[i])
    mat.propOnward[i, 10] <- as.numeric(dat$R_index[i])
    mat.propOnward[i, 11] <- as.numeric(finsize.tert[i])
    mat.propOnward[i, 12] <- as.numeric(finsize.tert.prop[i])
  }
  
  ## Combine output from all analyses
  if(s == scenariosHet[1]){
    df.all <- mat.propOnward
  }else{
    df.all <- rbind(df.all, mat.propOnward)
  }
  
}

dim(df.all)
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
names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'Analysis',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'R_index', 'FinSizeTert', 'FinSizeTertProp')
head(df.all)

library(dplyr)
library(forcats)
df.all <- df.all %>%
  mutate(Scenario = fct_recode(Scenario, "Baseline" = "Baseline", "A1" = "Scen1", "A2" = "Scen2",
                               "A3" = "Scen3", "A4" = "Scen4"))

library(ggplot2)

p1a <- ggplot(df.all[df.all$Analysis == 'Default' | df.all$Analysis == 'SA2', ],
       aes(x = Analysis, y = FinSizeProp, fill = Scenario)) +
  geom_boxplot() +
  # geom_violin() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_violin() +
  # facet_wrap(~Analysis) +
  xlab('') + ylab('Final size excluding index') + ggtitle('(a)') +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        # legend.text = element_text(size = 16),
        # legend.title = element_text(size = 18),
        legend.position = "none",
        title = element_text(size = 32))
p1a
  
p1b <- ggplot(df.all[df.all$Analysis == 'Default' | df.all$Analysis == 'SA2', ],
              aes(x = Analysis, y = FinSizeTertProp, fill = Scenario)) +
  geom_boxplot() +
  # geom_violin() + 
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_violin() +
  # facet_wrap(~Analysis) +
  xlab('') + ylab('Final size excluding secondary cases') + ggtitle('(b)') +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        legend.position = "bottom",
        title = element_text(size = 32))
p1b

library(gridExtra)
p1 <- grid.arrange(p1a,p1b, ncol = 1)
p1
# ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/Figure1_final.jpeg', p1, width = 70, height = 50, units = 'cm', dpi = 300)
ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/Figure2_FINAL.pdf', p1, width = 70, height = 50, units = 'cm', dpi = 300)

df.all$setting = 'Daycare'
df.daycare <- df.all
rm(df.all)

###------------------------------------------------------------------------------------------
### School

setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_delayQ/')

## Only scenarios including heterogeneity
scenariosHet <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
                  paste0('Sens',1:9,'_scen5'),
                  paste0('Sens',1:9,'_scen6'),
                  paste0('Sens',1:9,'_scen7'),
                  paste0('Sens',1:9,'_scen8'),
                  paste0('Sens',1:9,'_scen9')
)

for(s in scenariosHet){
  
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
  
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 12)
  
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
  
  for(i in 1:dim(dat)[1]){
    mat.propOnward[i, 1] <- scenario
    mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[i])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
    mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[i])
    mat.propOnward[i, 5] <- analysis
    mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[i])
    mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[i])
    mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[i])
    mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[i])
    mat.propOnward[i, 10] <- as.numeric(dat$R_index[i])
    mat.propOnward[i, 11] <- as.numeric(finsize.tert[i])
    mat.propOnward[i, 12] <- as.numeric(finsize.tert.prop[i])
    # mat.propOnward[i, 11] <- as.numeric(dat$propOnwardAfterPEV[i])
    # mat.propOnward[i, 12] <- as.numeric(dat$propEffectivePEV[i])
    # mat.propOnward[i, 13] <- as.numeric(dat$propReceivedPEV[i])
    # mat.propOnward[i, 14] <- as.numeric(dat$propSuscReceivedPEV[i])
  }
  
  ## Combine output from all analyses
  if(s == scenariosHet[1]){
    df.all <- mat.propOnward
  }else{
    df.all <- rbind(df.all, mat.propOnward)
  }
  
}

dim(df.all)
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
names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'Analysis',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'R_index', 'FinSizeTert', 'FinSizeTertProp')
head(df.all)

df.all$setting <- 'School'

df.all <- df.all %>%
  mutate(Scenario = fct_recode(Scenario, "Baseline" = "Baseline", "A1" = "Scen1", "A2" = "Scen2",
                               "A3" = "Scen3", "A4" = "Scen4"))
table(df.all$Scenario)

names(df.daycare)
names(df.all)

df.combined <- rbind(df.all, df.daycare)

## Figure S2: pOnwardQ

library(ggplot2)
pS2 <- ggplot(df.combined[df.combined$Scenario != 'Baseline', ], aes(x = Analysis, y = propOnwardAfterIsol, fill = Scenario)) +
  geom_boxplot() +
  # geom_violin() +
  facet_wrap(~setting, scales = "free_x") +
  xlab('') + ylab('Proportion of infections attributable to quarantined cases') +
  theme(legend.position = 'top',
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        title = element_text(size = 18),
        strip.text.x = element_text(size = 18))
pS2
ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/FigureS2_final.jpeg', pS2, width = 50, height = 30, units = 'cm', dpi = 300)


